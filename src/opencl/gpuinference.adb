with opencl; use opencl;
with cl_objects; use cl_objects;
with NeuralNet; use NeuralNet;

with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with System;
with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

package body GpuInference is
   NL: constant Character := Ada.Characters.Latin_1.LF;

   multiply_weight_kernel_text: constant String :=
     "__kernel void multiply_weights(__global float *input, __global float *output, __global float *weights, int weight_offset, int layer_size)" & NL &
     "{" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int px_y = get_global_id(1);" & NL &
     "   const int px_i = px_x + get_global_size(0) * px_y;" & NL &
     "   output[px_i] = weights[px_i + weight_offset] * input[px_i % layer_size];" & NL &
     "}" & NL &
     "" & NL;

   reduce_sum_kernel_text: constant String :=
     "__kernel void reduce_sum(__global float *input, __global float *output, __global float *bias, int bias_offset, int layer_size, int activator)" & NL &
     "{" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int px_y = get_global_id(1);" & NL &
     "   const int px_i = px_x + get_global_size(0) * px_y;" & NL &
     "   float sum = 0.0f;" & NL &
     "   for (int i=0 ; i<layer_size; ++i) {" & NL &
     "      sum += input[px_i * layer_size + i];" & NL &
     "   }" & NL &
     "   const float res = sum + bias[px_i + bias_offset];" & NL &
     "   output[px_i] = (activator == 0) ? (fmax(0.0f, res)) : (1.0f / (1.0f + exp(-res)));" & NL &
     "}" & NL &
     "" & NL;

   processing_program_text: constant String :=
     multiply_weight_kernel_text & NL &
     reduce_sum_kernel_text & NL;

   function Get_Temp_Buffer_Size(nn: in NeuralNet.Net) return Positive is
      result: Positive := nn.conf.inputSize * nn.conf.sizes(1);
      current_size: Positive := 1;
   begin
      for n in 2 .. nn.conf.sizes'Length loop
         current_size := nn.conf.sizes(n) * nn.conf.sizes(n - 1);
         if current_size > result then
            result := current_size;
         end if;
      end loop;
      return result;
   end Get_Temp_Buffer_Size;

   function Get_Temp_Input_Buffer_Size(nn: in NeuralNet.Net) return Positive is
      result: Positive := nn.conf.sizes(1);
   begin
      for n in 2 .. nn.conf.sizes'Length loop
         if nn.conf.sizes(n) > result then
            result := nn.conf.sizes(n);
         end if;
      end loop;
      return result;
   end Get_Temp_Input_Buffer_Size;

   function Create(ctx: cl_objects.Context_Access; nn: in NeuralNet.Net; cl_code: out opencl.Status) return NNData is
   begin
      return res: NNData do
         res.ctx := ctx;

         res.nn_activator := nn.conf.act;
         res.nn_shape.Append(nn.conf.inputSize);
         for i in 1 .. nn.conf.sizes'Length loop
            res.nn_shape.Append(nn.conf.sizes(i));
         end loop;

         res.processing_queue := new cl_objects.Command_Queue'(ctx.Create_Command_Queue(result_status => cl_code));
         res.processing_prog := new cl_objects.Program'(ctx.Create_Program(source        => processing_program_text,
                                                                           result_status => cl_code));
         cl_code := ctx.Build(prog    => res.processing_prog.all,
                              options => "-w -Werror");
         res.multiply_weights_kernel := new cl_objects.Kernel'(res.processing_prog.Create_Kernel("multiply_weights", cl_code));
         res.reduce_sum_kernel := new cl_objects.Kernel'(res.processing_prog.Create_Kernel("reduce_sum", cl_code));
         res.nn_weights := new cl_objects.Buffer'(Upload_Weights(ctx     => ctx.all,
                                                                 nn      => nn,
                                                                 cl_code => cl_code));
         res.nn_biases := new cl_objects.Buffer'(Upload_Biases(ctx     => ctx.all,
                                                               nn      => nn,
                                                               cl_code => cl_code));
         res.temp_buffer := new cl_objects.Buffer'(cl_objects.Create_Buffer(ctx => ctx.all,
                                                                            flags         => (opencl.ALLOC_HOST_PTR => True, others => False),
                                                                            size          => Get_Temp_Buffer_Size(nn) * 4,
                                                                            host_ptr      => System.Null_Address,
                                                                            result_status => cl_code));
         res.temp_input_buffer := new cl_objects.Buffer'(cl_objects.Create_Buffer(ctx => ctx.all,
                                                                                  flags         => (opencl.ALLOC_HOST_PTR => True, others => False),
                                                                                  size          => Get_Temp_Input_Buffer_Size(nn) * 4,
                                                                                  host_ptr      => System.Null_Address,
                                                                                  result_status => cl_code));
      end return;
   end Create;

   procedure Finalize(This: in out NNData) is
      procedure Free_Queue is new Ada.Unchecked_Deallocation(Object => cl_objects.Command_Queue,
                                                             Name   => cl_objects.Command_Queue_Access);
      procedure Free_Program is new Ada.Unchecked_Deallocation(Object => cl_objects.Program,
                                                               Name   => cl_objects.Program_Access);
      procedure Free_Kernel is new Ada.Unchecked_Deallocation(Object => cl_objects.Kernel,
                                                              Name   => cl_objects.Kernel_Access);
      procedure Free_Buffer is new Ada.Unchecked_Deallocation(Object => cl_objects.Buffer,
                                                              Name   => cl_objects.Buffer_Access);
   begin
      Free_Queue(This.processing_queue);
      Free_Kernel(This.reduce_sum_kernel);
      Free_Kernel(This.multiply_weights_kernel);
      Free_Program(This.processing_prog);
      Free_Buffer(This.nn_weights);
      Free_Buffer(This.nn_biases);
      Free_Buffer(This.temp_buffer);
      Free_Buffer(This.temp_input_buffer);
   end Finalize;

   type FlattenedWeights is array(Positive range <>) of aliased opencl.cl_float;
   type FlattenedBiases is array(Positive range <>) of aliased opencl.cl_float;


   function flatten_weights(nn: in NeuralNet.Net) return FlattenedWeights is
      total_size: Natural := 0;
   begin
      for layer of nn.layers loop
         for neuron of layer loop
            total_size := total_size + neuron.w'Length;
         end loop;
      end loop;
      return result: FlattenedWeights(1 .. total_size) do
         declare
            i: Positive := 1;
         begin
            for layer of nn.layers loop
               for neuron of layer loop
                  for weight of neuron.w loop
                     result(i) := opencl.cl_float(weight);
                     i := i + 1;
                  end loop;
               end loop;
            end loop;
         end;
      end return;
   end flatten_weights;

   function flatten_biases(nn: in NeuralNet.Net) return FlattenedBiases is
      total_size: Natural := 0;
   begin
      for layer of nn.layers loop
         total_size := total_size + Natural(layer.Length);
      end loop;
      return result: FlattenedBiases(1 .. total_size) do
         declare
            i: Positive := 1;
         begin
            for layer of nn.layers loop
               for neuron of layer loop
                  result(i) := opencl.cl_float(neuron.bias);
                  i := i + 1;
               end loop;
            end loop;
         end;
      end return;
   end flatten_biases;

   function Upload_Weights(ctx: in out cl_objects.Context; nn: in NeuralNet.Net; cl_code: out opencl.Status) return cl_objects.Buffer is
      host_buffer: aliased FlattenedWeights := flatten_weights(nn);
   begin
      return ctx.Create_Buffer(flags         => (opencl.COPY_HOST_PTR => True, others => False),
                               size          => host_buffer'Length * 4,
                               host_ptr      => host_buffer'Address,
                               result_status => cl_code);
   end Upload_Weights;

   function Upload_Biases(ctx: in out cl_objects.Context; nn: in NeuralNet.Net; cl_code: out opencl.Status) return cl_objects.Buffer is
      host_buffer: aliased FlattenedBiases := flatten_biases(nn);
   begin
      return ctx.Create_Buffer(flags         => (opencl.COPY_HOST_PTR => True, others => False),
                               size          => host_buffer'Length * 4,
                               host_ptr      => host_buffer'Address,
                               result_status => cl_code);
   end Upload_Biases;

   function Multiply_Weights(context: NNData; input, output: in System.Address; weight_offset: in Natural; layer_size, output_size: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
      layer_size_arg: aliased opencl.cl_int := opencl.cl_int(layer_size);
      weight_offset_arg: aliased opencl.cl_int := opencl.cl_int(weight_offset);
   begin
      cl_code := context.multiply_weights_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, input);
      cl_code := context.multiply_weights_kernel.Set_Arg(1, opencl.Raw_Address'Size / 8, output);
      cl_code := context.multiply_weights_kernel.Set_Arg(2, opencl.Raw_Address'Size / 8, context.nn_weights.Get_Address);
      cl_code := context.multiply_weights_kernel.Set_Arg(3, 4, weight_offset_arg'Address);
      cl_code := context.multiply_weights_kernel.Set_Arg(4, 4, layer_size_arg'Address);
      return context.processing_queue.Enqueue_Kernel(kern               => context.multiply_weights_kernel.all,
                                                     glob_ws            => (1 => output_size, 2 => 1),
                                                     loc_ws             => (1 => 1, 2 => 1), --TODO
                                                     events_to_wait_for => events_to_wait,
                                                     code               => cl_code);
   end Multiply_Weights;

   function Multiply_Weights(context: NNData; input, output: in out cl_objects.Buffer; weight_offset: in Natural; layer_size, output_size: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
   begin
      return Multiply_Weights(context, input.Get_Address, output.Get_Address, weight_offset, layer_size, output_size, events_to_wait, cl_code);
   end Multiply_Weights;

   function Reduce_Activate(context: NNData; input, output: in System.Address; bias_offset: in Natural; layer_size, output_size: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
      layer_size_arg: aliased opencl.cl_int := opencl.cl_int(layer_size);
      activator_arg: aliased opencl.cl_int := opencl.cl_int(if context.nn_activator = NeuralNet.RELU then 0 else 1);
      bias_offset_arg: aliased opencl.cl_int := opencl.cl_int(bias_offset);
   begin
      cl_code := context.reduce_sum_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, input);
      cl_code := context.reduce_sum_kernel.Set_Arg(1, opencl.Raw_Address'Size / 8, output);
      cl_code := context.reduce_sum_kernel.Set_Arg(2, opencl.Raw_Address'Size / 8, context.nn_biases.Get_Address);
      cl_code := context.reduce_sum_kernel.Set_Arg(3, 4, bias_offset_arg'Address);
      cl_code := context.reduce_sum_kernel.Set_Arg(4, 4, layer_size_arg'Address);
      cl_code := context.reduce_sum_kernel.Set_Arg(5, 4, activator_arg'Address);
      return context.processing_queue.Enqueue_Kernel(kern               => context.reduce_sum_kernel.all,
                                                     glob_ws            => (1 => output_size, 2 => 1),
                                                     loc_ws             => (1 => 1, 2 => 1),
                                                     events_to_wait_for => events_to_wait,
                                                     code               => cl_code);
   end Reduce_Activate;

   function Reduce_Activate(context: NNData; input, output: in out cl_objects.Buffer; bias_offset: in Natural; layer_size, output_size: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
   begin
      return Reduce_Activate(context, input.Get_Address, output.Get_Address, bias_offset, layer_size, output_size, events_to_wait, cl_code);
   end Reduce_Activate;


   function Forward(context: NNData; input, output: in out cl_objects.Buffer; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
      curr_weight_off: Natural := 0;
      curr_bias_off: Natural := 0;
      curr_layer_size: Positive := 1;
      next_layer_size: Positive := 1;
      curr_output_size: Positive := 1;
      previous_ev: opencl.Events := (1 => 0);
      final_event: opencl.Event_ID := 0;
   begin
      for i in 1 .. context.nn_shape.Length - 1 loop
         curr_layer_size := context.nn_shape(Positive(i));
         next_layer_size := context.nn_shape(Positive(i + 1));
         curr_output_size := curr_layer_size * next_layer_size;
         declare
            mult_w_ev: constant cl_objects.Event := Multiply_Weights(context        => context,
                                                                     input          => (if i=1 then input.Get_Address else context.temp_input_buffer.Get_Address),
                                                                     output         => context.temp_buffer.Get_Address,
                                                                     weight_offset  => curr_weight_off,
                                                                     layer_size     => curr_layer_size,
                                                                     output_size    => curr_output_size,
                                                                     events_to_wait => (if i=1 then events_to_wait else previous_ev),
                                                                     cl_code        => cl_code);
            reduce_ev: constant cl_objects.Event := Reduce_Activate(context        => context,
                                                                    input          => context.temp_buffer.Get_Address,
                                                                    output         => (if i=context.nn_shape.Length - 1 then output.Get_Address else context.temp_input_buffer.Get_Address),
                                                                    bias_offset    => curr_bias_off,
                                                                    layer_size     => curr_layer_size,
                                                                    output_size    => next_layer_size,
                                                                    events_to_wait => (1 => mult_w_ev.Get_Handle),
                                                                    cl_code        => cl_code);
         begin
            if cl_code /= opencl.SUCCESS then
               return cl_objects.Create_Empty;
            end if;

            final_event := reduce_ev.Get_Handle;
            if i > 1 then
               cl_code := opencl.Release_Event(previous_ev(1));
            end if;
            if i /= context.nn_shape.Length - 1 then
               cl_code := opencl.Retain_Event(final_event);
            else
               return cl_objects.Create_Event(final_event);
            end if;

            if cl_code /= opencl.SUCCESS then
               return cl_objects.Create_Empty;
            end if;
            previous_ev(1) := final_event;
         end;
         curr_bias_off := curr_bias_off + next_layer_size;
         curr_weight_off := curr_weight_off + curr_output_size;
      end loop;
      return cl_objects.Create_Empty;
   end Forward;

end GpuInference;
