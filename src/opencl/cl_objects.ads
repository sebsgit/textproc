with Ada.Finalization;

with opencl; use opencl;

package cl_objects is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Context is tagged limited private;
   type Program is tagged limited private;
   type Kernel is tagged limited private;
   type Event is tagged limited private;
   type Command_Queue is tagged limited private;

   function Create(context_platform: in Platform_ID; context_device: in Device_ID; result_status: out Status) return Context
     with Pre => context_platform /= 0 and context_device /= 0;
   function Create_Program(ctx: in out Context'Class; source: in String; result_status: out Status) return Program
     with Pre => source'Length > 0;
   function Create_Command_Queue(ctx: in out Context'Class; dev: in Device_ID; result_status: out Status) return Command_Queue
     with Pre => dev /= 0;
   function Build(prog: in out Program'Class; device: in Device_ID; options: in String) return Status
     with Pre => device /= 0;
   function Create_Kernel(prog: in out Program'Class; name: in String; result_status: out Status) return Kernel
     with Pre => name'Length > 0;

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

   type Event is limited new Ada.Finalization.Limited_Controlled with record
      handle: opencl.Event_ID;
   end record;
   overriding procedure Finalize(This: in out Event);

   type Command_Queue is limited new Ada.Finalization.Limited_Controlled with record
      handle: opencl.Command_Queue;
   end record;
   overriding procedure Finalize(This: in out Command_Queue);
end cl_objects;
