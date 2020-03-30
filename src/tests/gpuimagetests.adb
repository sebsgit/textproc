with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers;

with PixelArray;
with PixelArray.Gpu;
with ImageIO;
with opencl; use opencl;
with cl_objects;

package body GpuImageTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, initOpenCL'Access, "init opencl");
      Register_Routine (T, testGpuImage'Access, "Gpu image test");
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

end GpuImageTests;
