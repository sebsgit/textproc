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

   function Create_Processor(context: in out cl_objects.Context; status: out opencl.Status) return Processor;

   -- exposed for unit tests
   procedure Circle_Min_Max(proc: in out Processor; ctx: in out cl_objects.Context; image: in out PixelArray.Gpu.GpuImage; x, y: Natural; radius: Positive; min, max: out PixelArray.Pixel);

private
   type Processor is new Ada.Finalization.Limited_Controlled with record
      queue: cl_objects.Command_Queue_Access;
   end record;

   overriding procedure Finalize(This: in out Processor);
end GpuImageProc;
