with opencl;
with cl_objects;
with PixelArray.Gpu;

with Ada.Finalization;

package GpuImageProc is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Processor is tagged limited private;
   type Processor_Access is access all Processor;

   function Create_Processor(context: in out cl_objects.Context; status: out opencl.Status) return Processor;
   function Get_Command_Queue(proc: in out Processor) return cl_objects.Command_Queue_Access;

   function Bernsen_Adaptative_Threshold(proc: in out Processor;
                                         ctx: in out cl_objects.Context;
                                         source: in out PixelArray.Gpu.GpuImage;
                                         target: in out PixelArray.Gpu.GpuImage;
                                         radius: in Positive;
                                         c_min: in PixelArray.Pixel;
                                         cl_code: out opencl.Status) return cl_objects.Event
     with Pre => source.Get_Width = target.Get_Width and source.Get_Height = target.Get_Height;
   function Bernsen_Adaptative_Threshold(proc: in out Processor;
                                         ctx: in out cl_objects.Context;
                                         source: in out PixelArray.Gpu.GpuImage;
                                         target: in out PixelArray.Gpu.GpuImage;
                                         radius: in Positive;
                                         c_min: in PixelArray.Pixel;
                                         events_to_wait: in opencl.Events;
                                         cl_code: out opencl.Status) return cl_objects.Event
     with Pre => source.Get_Width = target.Get_Width and source.Get_Height = target.Get_Height;

   function Gaussian_Filter(proc: in out Processor;
                            ctx: in out cl_objects.Context;
                            source: in out PixelArray.Gpu.GpuImage;
                            target: in out PixelArray.Gpu.GpuImage;
                            size: in Positive;
                            sigma: in Float;
                            cl_code: out opencl.Status) return cl_objects.Event;

   function Erode(proc: in out Processor;
                  ctx: in out cl_objects.Context;
                  source: in out PixelArray.Gpu.GpuImage;
                  target: in out PixelArray.Gpu.GpuImage;
                  size: in Positive;
                  events_to_wait: in opencl.Events;
                  cl_code: out opencl.Status) return cl_objects.Event;

   function Dilate(proc: in out Processor;
                   ctx: in out cl_objects.Context;
                   source: in out PixelArray.Gpu.GpuImage;
                   target: in out PixelArray.Gpu.GpuImage;
                   size: in Positive;
                   events_to_wait: in opencl.Events;
                   cl_code: out opencl.Status) return cl_objects.Event;

   -- exposed for unit tests
   procedure Circle_Min_Max(proc: in out Processor; ctx: in out cl_objects.Context; image: in out PixelArray.Gpu.GpuImage; x, y: Natural; radius: Positive; min, max: out PixelArray.Pixel);

private
   type Processor is new Ada.Finalization.Limited_Controlled with record
      queue: cl_objects.Command_Queue_Access;
      program: cl_objects.Program_Access;
      bernsen_threshold_kernel: cl_objects.Kernel_Access;
      gaussian_blur_kernel: cl_objects.Kernel_Access;
      erode_kernel: cl_objects.Kernel_Access;
      dilate_kernel: cl_objects.Kernel_Access;
   end record;

   overriding procedure Finalize(This: in out Processor);
end GpuImageProc;
