with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with System; use System;
with Ada.Containers; use Ada.Containers;

with opencl; use opencl;
with cl_objects;
with GpuInference;
with NeuralNet;
with MathUtils;

package body GpuInferenceTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, initOpenCL'Access, "init opencl");
      Register_Routine (T, testGpuWeightUpload'Access, "Gpu NN weights test");
      Register_Routine (T, testGpuWeightApply'Access, "Gpu NN weights apply test");
      Register_Routine (T, testGpuForward'Access, "Gpu NN forward");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("GPU Inference Tests");
   end Name;

   procedure initOpenCL(T: in out Test_Cases.Test_Case'Class) is
      cl_status: opencl.Status;
   begin
      cl_status := opencl.Init(opencl.Get_OpenCL_Path(opencl.ARCH_32));
      Assert(cl_status = opencl.SUCCESS, "init: " & cl_status'Image);
   end initOpenCL;

   procedure testGpuWeightUpload(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(2);
      net: NeuralNet.Net(config.size);
      cl_code: opencl.Status;
      gpu_context: aliased cl_objects.Context := cl_objects.Create_Gpu(result_status => cl_code);
      gpu_queue: cl_objects.Command_Queue := gpu_context.Create_Command_Queue(result_status => cl_code);
   begin
      Assert(cl_code = opencl.SUCCESS, "Create context: " & cl_code'Image);

      config.inputSize := 1;
      config.act := NeuralNet.RELU;
      config.sizes := (2, 1);

      net := NeuralNet.create(config);

      Assert(net.layers.Length = 2, "layer count");
      Assert(net.layers(1).Length = 2, "neurons in layer 1");
      Assert(net.layers(2).Length = 1, "neurons in output layer");

      -- hardcode biases and weights
      net.layers(1)(1).bias := 0.3;
      net.layers(1)(1).w := (1 => 0.5);

      net.layers(1)(2).bias := 0.7;
      net.layers(1)(2).w := (1 => 0.1);

      net.layers(2)(1).bias := 0.1;
      net.layers(2)(1).w := (0.4, 0.6);

      declare
         weight_buff: cl_objects.Buffer := GpuInference.Upload_Weights(ctx     => gpu_context,
                                                                       nn      => net,
                                                                       cl_code => cl_code);
         type HostWeightBuff is array (Positive range<>) of opencl.cl_float;
         host_weights: aliased HostWeightBuff(1 .. 4) := (others => 0.0);
      begin
         Assert(cl_code = opencl.SUCCESS, "Upload buffer: " & cl_code'Image);
         declare
            empty_ev: opencl.Events(1 .. 0);
            ev: cl_objects.Event := cl_objects.Enqueue_Read(queue => gpu_queue,
                                                            mem_ob             => weight_buff,
                                                            offset             => 0,
                                                            size               => 4 * 4,
                                                            ptr                => host_weights'Address,
                                                            events_to_wait_for => empty_ev,
                                                            code               => cl_code);
         begin
            cl_code := ev.Wait;
            Assert(cl_code = opencl.SUCCESS, "Download buffer: " & cl_code'Image);
            Assert(host_weights(1) = 0.5, "w0");
            Assert(host_weights(2) = 0.1, "w1");
            Assert(host_weights(3) = 0.4, "w2");
            Assert(host_weights(4) = 0.6, "w3");
         end;
      end;
   end testGpuWeightUpload;

   procedure testGpuWeightApply(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(2);
      net: NeuralNet.Net(config.size);
      cl_code: opencl.Status;
      gpu_context: aliased cl_objects.Context := cl_objects.Create_Gpu(result_status => cl_code);
      gpu_queue: cl_objects.Command_Queue := gpu_context.Create_Command_Queue(result_status => cl_code);
   begin
      Assert(cl_code = opencl.SUCCESS, "Create context: " & cl_code'Image);

      config.inputSize := 1;
      config.act := NeuralNet.RELU;
      config.sizes := (2, 1);

      net := NeuralNet.create(config);

      Assert(net.layers.Length = 2, "layer count");
      Assert(net.layers(1).Length = 2, "neurons in layer 1");
      Assert(net.layers(2).Length = 1, "neurons in output layer");

      -- hardcode biases and weights
      net.layers(1)(1).bias := 0.3;
      net.layers(1)(1).w := (1 => 0.5);

      net.layers(1)(2).bias := 0.7;
      net.layers(1)(2).w := (1 => 0.1);

      net.layers(2)(1).bias := 0.1;
      net.layers(2)(1).w := (0.4, 0.6);

      declare
         type InputHostBuff is array(Positive range <>) of opencl.cl_float;
         input_host_buff: aliased InputHostBuff := (1 => 0.1, 2 => 0.2);
         input_buff: cl_objects.Buffer := gpu_context.Create_Buffer(flags         => (opencl.COPY_HOST_PTR => True, others => False),
                                                                    size          => 2 * 4,
                                                                    host_ptr      => input_host_buff'Address,
                                                                    result_status => cl_code);
         output_buff: cl_objects.Buffer := gpu_context.Create_Buffer(flags         => (opencl.ALLOC_HOST_PTR => True, others => False),
                                                                     size          => 4 * 4,
                                                                     host_ptr      => System.Null_Address,
                                                                     result_status => cl_code);
         gpu_nn: constant GpuInference.NNData := GpuInference.Create(ctx     => gpu_context'Unchecked_Access,
                                                                     nn      => net,
                                                                     cl_code => cl_code);
      begin
         Assert(cl_code = opencl.SUCCESS, "GPU NN: " & cl_code'Image);
         declare
            type HostResultBuff is array (Positive range<>) of opencl.cl_float;
            host_result: aliased HostResultBuff(1 .. 4) := (others => 0.0);
            empty_ev: opencl.Events(1 .. 0);
            ev: constant cl_objects.Event := gpu_nn.Multiply_Weights(input          => input_buff,
                                                                     output         => output_buff,
                                                                     weight_offset  => 0,
                                                                     layer_size     => 2,
                                                                     output_size    => 4,
                                                                     events_to_wait => empty_ev,
                                                                     cl_code => cl_code);
            downl_ev: cl_objects.Event := gpu_queue.Enqueue_Read(mem_ob             => output_buff,
                                                                 offset             => 0,
                                                                 size               => 4 * 4,
                                                                 ptr                => host_result'Address,
                                                                 events_to_wait_for => (1 => ev.Get_Handle),
                                                                 code               => cl_code);
         begin
            cl_code := downl_ev.Wait;
            Assert(cl_code = opencl.SUCCESS, "result download: " & cl_code'Image);
            Assert(abs(host_result(1) - 0.1 * 0.5) < 0.001, "r0: " & host_result(1)'Image);
            Assert(abs(host_result(2) - 0.2 * 0.1) < 0.001, "r1: " & host_result(2)'Image);
            Assert(abs(host_result(3) - 0.1 * 0.4) < 0.001, "r2: " & host_result(3)'Image);
            Assert(abs(host_result(4) - 0.2 * 0.6) < 0.001, "r3: " & host_result(4)'Image);

            declare
               reduce_ev: constant cl_objects.Event := gpu_nn.Reduce_Activate(input          => output_buff,
                                                                              output         => input_buff,
                                                                              bias_offset    => 0,
                                                                              layer_size     => 2,
                                                                              output_size    => 2,
                                                                              act            => NeuralNet.RELU,
                                                                              events_to_wait => empty_ev,
                                                                              cl_code        => cl_code);
               downl_2_ev: cl_objects.Event := gpu_queue.Enqueue_Read(mem_ob             => input_buff,
                                                                      offset             => 0,
                                                                      size               => 2 * 4,
                                                                      ptr                => host_result'Address,
                                                                      events_to_wait_for => (1 => reduce_ev.Get_Handle),
                                                                      code               => cl_code);
            begin
               Assert(cl_code = opencl.SUCCESS, "enq read: " & cl_code'Image);
               cl_code := downl_2_ev.Wait;

               Assert(cl_code = opencl.SUCCESS, "result download: " & cl_code'Image);
               Assert(abs(host_result(1) - (0.3 + 0.1 * 0.5 + 0.2 * 0.1)) < 0.001, "r0: " & host_result(1)'Image);
               Assert(abs(host_result(2) - (0.7 + 0.1 * 0.4 + 0.2 * 0.6)) < 0.001, "r1: " & host_result(2)'Image);
            end;
         end;
      end;
   end testGpuWeightApply;

   procedure testGpuForward(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(2);
      net: NeuralNet.Net(config.size);
      cl_code: opencl.Status;
      gpu_context: aliased cl_objects.Context := cl_objects.Create_Gpu(result_status => cl_code);
      gpu_queue: cl_objects.Command_Queue := gpu_context.Create_Command_Queue(result_status => cl_code);
      cpu_results: MathUtils.Vector;
      cpu_result: Float := 0.0;
      input_value: Float;
      cpu_input: MathUtils.Vector;
   begin
      Assert(cl_code = opencl.SUCCESS, "Create context: " & cl_code'Image);

      config.inputSize := 1;
      config.act := NeuralNet.RELU;
      config.sizes := (2, 1);

      net := NeuralNet.create(config);

      Assert(net.layers.Length = 2, "layer count");
      Assert(net.layers(1).Length = 2, "neurons in layer 1");
      Assert(net.layers(2).Length = 1, "neurons in output layer");

      input_value := 0.7;
      -- hardcode biases and weights
      net.layers(1)(1).bias := 0.5;
      net.layers(1)(1).w := (1 => 0.3);

      net.layers(1)(2).bias := 0.2;
      net.layers(1)(2).w := (1 => 0.17);

      net.layers(2)(1).bias := 0.7;
      net.layers(2)(1).w := (0.4, 0.9);

      cpu_input.Append(input_value);
      cpu_results := net.forward(cpu_input);
      cpu_result := cpu_results(1);
      declare
         null_events: opencl.Events(1 .. 0);

         type HostBuff is array (Positive range<>) of opencl.cl_float;
         host_input: aliased HostBuff(1 .. 1) := (others => opencl.cl_float(input_value));
         host_result: aliased HostBuff(1 .. 1) := (others => 0.0);

         input_buff: cl_objects.Buffer := gpu_context.Create_Buffer(flags         => (opencl.COPY_HOST_PTR => True, others => False),
                                                                    size          => 4,
                                                                    host_ptr      => host_input'Address,
                                                                    result_status => cl_code);
         output_buff: cl_objects.Buffer := gpu_context.Create_Buffer(flags         => (opencl.ALLOC_HOST_PTR => True, others => False),
                                                                     size          => 4,
                                                                     host_ptr      => System.Null_Address,
                                                                     result_status => cl_code);
         gpu_nn: constant GpuInference.NNData := GpuInference.Create(ctx     => gpu_context'Unchecked_Access,
                                                                     nn      => net,
                                                                     cl_code => cl_code);
         forward_ev: cl_objects.Event := gpu_nn.Forward(input          => input_buff,
                                                        output         => output_buff,
                                                        act            => net.conf.act,
                                                        events_to_wait => null_events,
                                                        cl_code        => cl_code);
      begin
         Assert(cl_code = opencl.SUCCESS, "forward values");
         -- cl_code := forward_ev.Wait;
         declare
            downl_ev: cl_objects.Event := gpu_queue.Enqueue_Read(mem_ob             => output_buff,
                                                                 offset             => 0,
                                                                 size               => 4,
                                                                 ptr                => host_result'Address,
                                                                 events_to_wait_for => null_events,
                                                                 code               => cl_code);
         begin
            cl_code := downl_ev.Wait;
            Assert(cl_code = opencl.SUCCESS, "download result");
            Assert(abs(Float(host_result(1)) - cpu_result) < 0.001, "Gpu nn forward fail");
         end;
      end;
   end testGpuForward;
end GpuInferenceTests;
