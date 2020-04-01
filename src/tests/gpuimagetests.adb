with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with System; use System;

with PixelArray; use PixelArray;
with PixelArray.Gpu;
with ImageIO;
with opencl; use opencl;
with cl_objects;
with GpuImageProc;
with ImageThresholds;
with ImageFilters;

package body GpuImageTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, initOpenCL'Access, "init opencl");
      Register_Routine (T, testGpuImage'Access, "Gpu image test");
      Register_Routine (T, testGpuProcessing'Access, "Gpu image processing");
      Register_Routine (T, testGpuBernsenThreshold'Access, "Gpu Bernsen Threshold");
      Register_Routine (T, testGpuGaussianFilter'Access, "Gpu Gaussian Filter");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("GPU Image Tests");
   end Name;

   procedure Find_Gpu_Device(platf: out Platform_ID; dev: out Device_ID) is
      cl_code: opencl.Status;
      platform_ids: constant opencl.Platforms := opencl.Get_Platforms(cl_code);
   begin
      if cl_code = opencl.SUCCESS then
         for p_id of platform_ids loop
            declare
               device_ids: constant opencl.Devices := opencl.Get_Devices(id            => p_id,
                                                                         dev_type      => opencl.DEVICE_TYPE_GPU,
                                                                         result_status => cl_code);
            begin
               if cl_code = opencl.SUCCESS and device_ids'Length > 0 then
                  platf := p_id;
                  dev := device_ids(1);
                  exit;
               end if;
            end;
         end loop;
      end if;
   end Find_Gpu_Device;

   procedure initOpenCL(T: in out Test_Cases.Test_Case'Class) is
      cl_status: opencl.Status;
   begin
      cl_status := opencl.Init(opencl.Get_OpenCL_Path(opencl.ARCH_32));
      Assert(cl_status = opencl.SUCCESS, "init: " & cl_status'Image);
   end initOpenCL;

   procedure testGpuImage(T : in out Test_Cases.Test_Case'Class) is
      input: PixelArray.ImagePlane := ImageIO.load("../training_set/20180501.jpg");
      target: PixelArray.ImagePlane := PixelArray.allocate(width  => input.width,
                                                           height => input.height);
      platf_id: opencl.Platform_ID := 0;
      dev_id: opencl.Device_ID := 0;
   begin
      Assert(not input.isEqual(target), "input = target");
      Find_Gpu_Device(platf_id, dev_id);
      Assert(platf_id /= 0, "no platform");
      Assert(dev_id /= 0, "no device");
      declare
         cl_code: opencl.Status;
         context: cl_objects.Context := cl_objects.Create(platf_id, dev_id, cl_code);
         queue: cl_objects.Command_Queue := context.Create_Command_Queue(dev_id, cl_code);
         gpuSource: PixelArray.Gpu.GpuImage := PixelArray.Gpu.Upload(ctx    => context,
                                                                     flags  => (others => False),
                                                                     image  => input,
                                                                     status => cl_code);
      begin
         Assert(cl_code = opencl.SUCCESS, "Gpu upload failed: " & cl_code'Image);
         Assert(gpuSource.Get_Address /= System.Null_Address, "gpu address null");
         declare
            downl_ev: cl_objects.Event := PixelArray.Gpu.Download(queue, gpuSource, target, cl_code);
         begin
            Assert(cl_code = opencl.SUCCESS, "Gpu download failed " & cl_code'Image);
            cl_code := downl_ev.Wait;
            Assert(cl_code = opencl.SUCCESS, "wait failed " & cl_code'Image);
         end;
      end;

      Assert(input.isEqual(target), "Input /= target");
   end testGpuImage;

   procedure testGpuProcessing(T : in out Test_Cases.Test_Case'Class) is
      platf_id: opencl.Platform_ID := 0;
      dev_id: opencl.Device_ID := 0;
      cl_code: opencl.Status;
      input: PixelArray.ImagePlane := ImageIO.load("../training_set/20180501.jpg");
   begin
      Find_Gpu_Device(platf_id, dev_id);
      Assert(platf_id /= 0, "no platform");
      Assert(dev_id /= 0, "no device");
      declare
         context: cl_objects.Context := cl_objects.Create(context_platform => platf_id,
                                                          context_device   => dev_id,
                                                          result_status    => cl_code);
         gpuSource: PixelArray.Gpu.GpuImage := PixelArray.Gpu.Upload(ctx    => context,
                                                                     flags  => (others => False),
                                                                     image  => input,
                                                                     status => cl_code);
      begin
         Assert(cl_code = opencl.SUCCESS, "Create context and upload: " & cl_code'Image);
         declare
            minpx, maxpx: PixelArray.Pixel := 0;
            proc: GpuImageProc.Processor := GpuImageProc.Create_Processor(context => context,
                                                                          status  => cl_code);
         begin
            Assert(cl_code = opencl.SUCCESS, "Create processor: " & cl_code'Image);
            proc.Circle_Min_Max(ctx    => context,
                                image  => gpuSource,
                                x      => 15,
                                y      => 18,
                                radius => 7,
                                min    => minpx,
                                max    => maxpx);
            declare
               minmaxcpu: ImageThresholds.MinMaxIntensity;
            begin
               minmaxcpu := ImageThresholds.circleMinMax(image  => input,
                                                         x      => 15,
                                                         y      => 18,
                                                         radius => 7);
               Ada.Text_IO.Put_Line("mn / max cpu: " & minmaxcpu.min'Image & " " & minmaxcpu.max'Image);

               Assert(minmaxcpu.min = minpx, "min error: " & minmaxcpu.min'Image & " got " & minpx'Image);
               Assert(minmaxcpu.max = maxpx, "min error: " & minmaxcpu.max'Image & " got " & maxpx'Image);
            end;
            Ada.Text_IO.Put_Line("mn / max gpu: " & minpx'Image & " " & maxpx'Image);
         end;
      end;
   end testGpuProcessing;

   procedure testGpuBernsenThreshold(T : in out Test_Cases.Test_Case'Class) is
      platf_id: opencl.Platform_ID := 0;
      dev_id: opencl.Device_ID := 0;
      cl_code: opencl.Status;
      input: PixelArray.ImagePlane := ImageIO.load("../training_set/20180501.jpg");
      target: PixelArray.ImagePlane := PixelArray.allocate(width  => input.width,
                                                           height => input.height);
      target_cpu: constant PixelArray.ImagePlane := ImageThresholds.bernsenAdaptative(image  => input,
                                                                                      radius => 10,
                                                                                      c_min  => 35);
   begin
      Find_Gpu_Device(platf_id, dev_id);
      Assert(platf_id /= 0, "no platform");
      Assert(dev_id /= 0, "no device");
      declare
         context: cl_objects.Context := cl_objects.Create(context_platform => platf_id,
                                                          context_device   => dev_id,
                                                          result_status    => cl_code);
         gpuSource: PixelArray.Gpu.GpuImage := PixelArray.Gpu.Upload(ctx    => context,
                                                                     flags  => (others => False),
                                                                     image  => input,
                                                                     status => cl_code);
         gpuTarget: PixelArray.Gpu.GpuImage := PixelArray.Gpu.Upload(ctx    => context,
                                                                     flags  => (others => False),
                                                                     image  => target,
                                                                     status => cl_code);
         proc: GpuImageProc.Processor := GpuImageProc.Create_Processor(context => context,
                                                                       status  => cl_code);
      begin
         Assert(cl_code = opencl.SUCCESS, "Create processor: " & cl_code'Image);
         declare
            proc_event: cl_objects.Event := proc.Bernsen_Adaptative_Threshold(ctx     => context,
                                                                              source  => gpuSource,
                                                                              target  => gpuTarget,
                                                                              radius  => 10,
                                                                              c_min   => 35,
                                                                              cl_code => cl_code);
         begin
            Assert(cl_code = opencl.SUCCESS, "Run GPU threshold: " & cl_code'Image);
            cl_code := proc_event.Wait;
            Assert(cl_code = opencl.SUCCESS, "wait for threshold kernel: " & cl_code'Image);
            declare
               downl_ev: cl_objects.Event := PixelArray.Gpu.Download(proc.Get_Command_Queue.all, gpuTarget, target, cl_code);
            begin
               Assert(cl_code = opencl.SUCCESS, "Gpu download failed " & cl_code'Image);
               cl_code := downl_ev.Wait;
               Assert(cl_code = opencl.SUCCESS, "wait failed " & cl_code'Image);
               Assert(target.isEqual(target_cpu), "Bernsen GPU threshold failed");
            end;
         end;
      end;
   end testGpuBernsenThreshold;

   procedure testGpuGaussianFilter(T : in out Test_Cases.Test_Case'Class) is
      platf_id: opencl.Platform_ID := 0;
      dev_id: opencl.Device_ID := 0;
      cl_code: opencl.Status;
      input: PixelArray.ImagePlane := ImageIO.load("../training_set/20180501.jpg");
      target: PixelArray.ImagePlane := PixelArray.allocate(width  => input.width,
                                                           height => input.height);
      target_cpu: constant PixelArray.ImagePlane := ImageFilters.gaussian(image => input,
                                                                          size  => 7,
                                                                          sigma => 2.4);
   begin
      Find_Gpu_Device(platf_id, dev_id);
      Assert(platf_id /= 0, "no platform");
      Assert(dev_id /= 0, "no device");
      declare
         context: cl_objects.Context := cl_objects.Create(context_platform => platf_id,
                                                          context_device   => dev_id,
                                                          result_status    => cl_code);
         gpuSource: PixelArray.Gpu.GpuImage := PixelArray.Gpu.Upload(ctx    => context,
                                                                     flags  => (others => False),
                                                                     image  => input,
                                                                     status => cl_code);
         gpuTarget: PixelArray.Gpu.GpuImage := PixelArray.Gpu.Upload(ctx    => context,
                                                                     flags  => (others => False),
                                                                     image  => target,
                                                                     status => cl_code);
         proc: GpuImageProc.Processor := GpuImageProc.Create_Processor(context => context,
                                                                       status  => cl_code);
      begin
         Assert(cl_code = opencl.SUCCESS, "Create processor: " & cl_code'Image);
         declare
            proc_event: cl_objects.Event := proc.Gaussian_Filter(ctx     => context,
                                                                 source  => gpuSource,
                                                                 target  => gpuTarget,
                                                                 size    => 7,
                                                                 sigma   => 2.4,
                                                                 cl_code => cl_code);
         begin
            Assert(cl_code = opencl.SUCCESS, "Run GPU gaussian filter: " & cl_code'Image);
            cl_code := proc_event.Wait;
            Assert(cl_code = opencl.SUCCESS, "wait for kernel: " & cl_code'Image);
            declare
               downl_ev: cl_objects.Event := PixelArray.Gpu.Download(proc.Get_Command_Queue.all, gpuTarget, target, cl_code);
            begin
               Assert(cl_code = opencl.SUCCESS, "Gpu download failed " & cl_code'Image);
               cl_code := downl_ev.Wait;
               Assert(cl_code = opencl.SUCCESS, "wait failed " & cl_code'Image);
               Ada.Text_IO.Put_Line("Gaussian Filter CPU / GPU RMS distance: " & target.distanceRMS(target_cpu)'Image);
               Assert(target.distanceRMS(target_cpu) < 1.0, "Gaussian filter GPU failed");
            end;
         end;
      end;
   end testGpuGaussianFilter;

end GpuImageTests;
