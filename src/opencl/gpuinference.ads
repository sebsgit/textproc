with NeuralNet;
with cl_objects;
with opencl;

with Ada.Finalization;
with Ada.Containers.Vectors;
with System;

package GpuInference is
   pragma Elaborate_Body;

   type NNData is tagged limited private;

   function Create(ctx: cl_objects.Context_Access; nn: in NeuralNet.Net; cl_code: out opencl.Status) return NNData;

   function Upload_Weights(ctx: in out cl_objects.Context; nn: in NeuralNet.Net; cl_code: out opencl.Status) return cl_objects.Buffer;
   function Upload_Biases(ctx: in out cl_objects.Context; nn: in NeuralNet.Net; cl_code: out opencl.Status) return cl_objects.Buffer;
   function Multiply_Weights(context: NNData; input, output: in out cl_objects.Buffer; weight_offset: in Natural; layer_size, output_size: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;
   function Reduce_Activate(context: NNData; input, output: in out cl_objects.Buffer; bias_offset: in Natural; layer_size, output_size: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;
   function Multiply_Weights(context: NNData; input, output: in System.Address; weight_offset: in Natural; layer_size, output_size: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;
   function Reduce_Activate(context: NNData; input, output: in System.Address; bias_offset: in Natural; layer_size, output_size: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;
   function Forward(context: NNData; input, output: in out cl_objects.Buffer; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;

private
   package Int_Vec is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                 Element_Type => Positive);

   type NNData is new Ada.Finalization.Limited_Controlled with record
      ctx: cl_objects.Context_Access;
      processing_prog: cl_objects.Program_Access;
      processing_queue: cl_objects.Command_Queue_Access;
      multiply_weights_kernel: cl_objects.Kernel_Access;
      reduce_sum_kernel: cl_objects.Kernel_Access;
      nn_weights: cl_objects.Buffer_Access;
      nn_biases: cl_objects.Buffer_Access;
      temp_buffer: cl_objects.Buffer_Access;
      temp_input_buffer: cl_objects.Buffer_Access;
      nn_shape: Int_Vec.Vector;
      nn_activator: NeuralNet.Activator;
   end record;
   overriding procedure Finalize(This: in out NNData);
end GpuInference;
