with opencl;
with cl_objects;
with PixelArray.Gpu;
with ImageRegions;

with System;
with Ada.Finalization;

package GpuComponentLabeling is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Processor is tagged limited private;
   type Processor_Access is access all Processor;

   type Pixel_CCL_Data is record
      label: opencl.cl_uint;
      min_x, min_y: opencl.cl_int;
      max_x, max_y: opencl.cl_int;
   end record;
   for Pixel_CCL_Data'Size use 32 * 5;
   pragma Convention(Convention => C, Entity => Pixel_CCL_Data);

   type CCL_Data is array (Natural range <>) of Pixel_CCL_Data;
   pragma Convention(Convention => C, Entity => CCL_Data);

   function Create(ctx: cl_objects.Context_Access; width, height: in Positive; cl_code: out opencl.Status) return Processor;
   function Init_CCL_Data(proc: in out Processor; gpu_image: in out PixelArray.Gpu.GpuImage; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;
   function Vertical_Pass(proc: in out Processor; events_to_wait: opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;
   function Merge_Pass(proc: in out Processor; width_div: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;
   function Merge_Pass(proc: in out Processor; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;

   function Run_CCL(proc: in out Processor; gpu_image: in out PixelArray.Gpu.GpuImage; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;

   function Get_Width(proc: in Processor) return Natural;
   function Get_Height(proc: in Processor) return Natural;

   function Detect_Regions(proc: in out Processor; preprocessed_cpu_image: in PixelArray.ImagePlane; cl_code: out opencl.Status) return ImageRegions.RegionVector.Vector;
   function Detect_Regions_And_Assign_Labels(proc: in out Processor; preprocessed_cpu_image: in out PixelArray.ImagePlane; cl_code: out opencl.Status) return ImageRegions.RegionVector.Vector;

   -- exposed for testing
   function Get_CCL_Data(proc: in out Processor; host_buff: in System.Address; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event;

private
   type Processor is new Ada.Finalization.Limited_Controlled with record
      context: cl_objects.Context_Access;
      width, height: aliased opencl.cl_int := 0;
      max_ccl_size: opencl.cl_int := 0;
      ccl_data: cl_objects.Buffer_Access;
      gpu_queue: cl_objects.Command_Queue_Access;
      processing_program: cl_objects.Program_Access;
      initialization_kernel: cl_objects.Kernel_Access;
      vertical_pass_kernel: cl_objects.Kernel_Access;
      merge_pass_kernel: cl_objects.Kernel_Access;
   end record;
   overriding procedure Finalize(This: in out Processor);
end GpuComponentLabeling;
