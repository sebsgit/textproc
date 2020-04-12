with opencl;
with cl_objects;
with PixelArray.Gpu;

with System;
with Ada.Finalization;

--TODO optimize events
--TODO optimize local work group sizes
package GpuComponentLabeling is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Processor is tagged limited private;

   type Pixel_CCL_Data is record
      label: opencl.cl_uint;
   end record;
   for Pixel_CCL_Data'Size use 32;
   pragma Convention(Convention => C, Entity => Pixel_CCL_Data);

   type CCL_Data is array (Natural range <>) of Pixel_CCL_Data;
   pragma Convention(Convention => C, Entity => CCL_Data);

   function Create(ctx: cl_objects.Context_Access; width, height: in Positive; cl_code: out opencl.Status) return Processor;
   function Init_CCL_Data(proc: in out Processor; gpu_image: in out PixelArray.Gpu.GpuImage; cl_code: out opencl.Status) return cl_objects.Event
     with Pre => gpu_image.Get_Width = proc.Get_Width and gpu_image.Get_Height = proc.Get_Height;
   function Vertical_Pass(proc: in out Processor; cl_code: out opencl.Status) return cl_objects.Event;
   function Merge_Pass(proc: in out Processor; width_div: in Positive; cl_code: out opencl.Status) return cl_objects.Event;
   function Merge_Pass(proc: in out Processor; cl_code: out opencl.Status) return cl_objects.Event;

   function Run_CCL(proc: in out Processor; gpu_image: in out PixelArray.Gpu.GpuImage; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event
     with Pre => gpu_image.Get_Width = proc.Get_Width and gpu_image.Get_Height = proc.Get_Height;

   function Get_Width(proc: in Processor) return Natural;
   function Get_Height(proc: in Processor) return Natural;

   -- exposed for testing
   function Get_CCL_Data(proc: in out Processor; host_buff: in System.Address; cl_code: out opencl.Status) return cl_objects.Event;

private
   type Processor is new Ada.Finalization.Limited_Controlled with record
      context: cl_objects.Context_Access;
      width, height: aliased opencl.cl_int := 0;
      ccl_data: cl_objects.Buffer_Access;
      gpu_queue: cl_objects.Command_Queue_Access;
      processing_program: cl_objects.Program_Access;
      initialization_kernel: cl_objects.Kernel_Access;
      vertical_pass_kernel: cl_objects.Kernel_Access;
      merge_pass_kernel: cl_objects.Kernel_Access;
   end record;
   overriding procedure Finalize(This: in out Processor);
end GpuComponentLabeling;
