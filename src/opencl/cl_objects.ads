with Ada.Finalization;
with System;

with opencl; use opencl;

package cl_objects is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Context is tagged limited private;
   type Program is tagged limited private;
   type Kernel is tagged limited private;
   type Event is tagged limited private;
   type Command_Queue is tagged limited private;
   type Buffer is tagged limited private;

   type Command_Queue_Access is access all Command_Queue;

   function Create(context_platform: in Platform_ID; context_device: in Device_ID; result_status: out Status) return Context
     with Pre => context_platform /= 0 and context_device /= 0;
   function Create_Program(ctx: in out Context'Class; source: in String; result_status: out Status) return Program
     with Pre => source'Length > 0;
   function Create_Command_Queue(ctx: in out Context'Class; dev: in Device_ID; result_status: out Status) return Command_Queue
     with Pre => dev /= 0;
   function Create_Command_Queue(ctx: in out Context'Class; result_status: out Status) return Command_Queue;

   function Create_Buffer(ctx: in out Context'Class; flags: in Mem_Flags; size: Positive; host_ptr: System.Address; result_status: out Status) return Buffer;
   function Enqueue_Write(queue: in out Command_Queue'Class; mem_ob: in out Buffer'Class; offset: Natural; size: Positive; ptr: System.Address; events_to_wait_for: in Events; code: out Status) return Event;
   function Enqueue_Read(queue: in out Command_Queue'Class; mem_ob: in out Buffer'Class; offset: Natural; size: Positive; ptr: System.Address; events_to_wait_for: in Events; code: out Status) return Event;
   function Enqueue_Kernel(queue: in out Command_Queue'Class; kern: in out Kernel'Class; glob_ws: Dimensions; loc_ws: Dimensions; events_to_wait_for: in Events; code: out Status) return Event;
   function Enqueue_Kernel(queue: in out Command_Queue'Class; kern: in out Kernel'Class; glob_ws: Dimensions; loc_ws: Dimensions; code: out Status) return Event;

   function Build(prog: in out Program'Class; device: in Device_ID; options: in String) return Status
     with Pre => device /= 0;
   function Build(ctx: in out Context'Class; prog: in out Program'Class; options: in String) return Status;
   function Get_Build_Log(prog: in out Program'Class; device: in Device_ID) return String;
   function Get_Build_Log(ctx: in out Context'Class; prog: in out Program'Class) return String;

   function Create_Kernel(prog: in out Program'Class; name: in String; result_status: out Status) return Kernel
     with Pre => name'Length > 0;
   function Set_Arg(kern: in out Kernel; index: Natural; size: Positive; address: System.Address) return Status;

   function Address(buff: in out Buffer'Class) return System.Address;
   function Get_ID(buff: in Buffer) return opencl.Mem_ID;

   function Wait(ev: in out Event) return Status;
   function Get_Handle(ev: in Event) return opencl.Event_ID;
   function Finish(queue: in out Command_Queue) return Status;

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

   type Buffer is limited new Ada.Finalization.Limited_Controlled with record
      handle: aliased opencl.Mem_ID;
   end record;
   overriding procedure Finalize(This: in out Buffer);
end cl_objects;
