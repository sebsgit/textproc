with Ada.Finalization;

with opencl; use opencl;

package cl_objects is
   type Context is tagged limited private;
   type Program is tagged limited private;
   type Kernel is tagged limited private;

   function Create(context_platform: in Platform_ID; context_device: in Device_ID; result_status: out Status) return Context;
   function Create_Program(ctx: in out Context; source: in String; result_status: out Status) return Program'Class;

private
   type Context is limited new Ada.Finalization.Limited_Controlled with record
      handle: opencl.Context_ID;
      device: Device_ID;
   end record;
   overriding procedure Finalize(This: in out Context);

   type Program is limited new Ada.Finalization.Limited_Controlled with record
      handle: opencl.Program_ID;
   end record;
   overriding procedure Finalize(This: in out Program);

   type Kernel is limited new Ada.Finalization.Limited_Controlled with record
      handle: opencl.Kernel_ID;
   end record;
   overriding procedure Finalize(This: in out Kernel);
end cl_objects;
