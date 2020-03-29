with Ada.Text_IO;

package body cl_objects is
   function Create(context_platform: in Platform_ID; context_device: in Device_ID; result_status: out Status) return Context is
   begin
      return ctx: Context do
         ctx.device := context_device;
         ctx.handle := opencl.Create_Context(context_platform => context_platform,
                                             context_device   => context_device,
                                             result_status    => result_status);
      end return;
   end Create;

   function Create_Program(ctx: in out Context'Class; source: in String; result_status: out Status) return Program is
   begin
      return prog: Program do
         prog.handle := opencl.Create_Program(ctx           => ctx.handle,
                                              source        => source,
                                              result_status => result_status);
      end return;
   end Create_Program;

   function Create_Command_Queue(ctx: in out Context'Class; dev: in Device_ID; result_status: out Status) return Command_Queue is
   begin
      return q: Command_Queue do
         q.handle := opencl.Create_Command_Queue(ctx           => ctx.handle,
                                                 dev           => dev,
                                                 result_status => result_status);
      end return;
   end Create_Command_Queue;

   function Build(prog: in out Program'Class; device: in Device_ID; options: in String) return Status is
   begin
      return opencl.Build_Program(id      => prog.handle,
                                  device  => device,
                                  options => options);
   end Build;

   function Get_Build_Log(prog: in out Program'Class; device: in Device_ID) return String is
      cl_stat: opencl.Status;
   begin
      return opencl.Get_Program_Build_Log(id            => prog.handle,
                                          device        => device,
                                          result_status => cl_stat);
   end Get_Build_Log;

   function Create_Kernel(prog: in out Program'Class; name: in String; result_status: out Status) return Kernel is
   begin
      return kern: Kernel do
         kern.handle := opencl.Create_Kernel(program       => prog.handle,
                                             name          => name,
                                             result_status => result_status);
      end return;
   end Create_Kernel;

   function Set_Arg(kern: in out Kernel; index: Natural; size: Positive; address: System.Address) return Status is
   begin
      return opencl.Set_Kernel_Arg(id      => kern.handle,
                                   index   => index,
                                   size    => size,
                                   address => address);
   end Set_Arg;

   function Finish(queue: in out Command_Queue) return Status is
   begin
      return opencl.Finish(queue.handle);
   end Finish;

   generic
      type Handle_Type is new Raw_Address;
      with function Release_Callback (handle: in Handle_Type) return opencl.Status;
   package Finalization_Impl is
      procedure Release (handle: Handle_Type);
   end Finalization_Impl;

   package body Finalization_Impl is
      procedure Release (handle: Handle_Type) is
         cl_code: opencl.Status;
      begin
         if handle /= 0 then
            cl_code := Release_Callback(handle);
         end if;
      end Release;
   end Finalization_Impl;

   procedure Finalize(This: in out Context) is
      package Cleanup is new Finalization_Impl(Handle_Type      => opencl.Context_ID,
                                               Release_Callback => opencl.Release_Context);
   begin
      Cleanup.Release(This.handle);
   end Finalize;

   procedure Finalize(This: in out Program) is
      package Cleanup is new Finalization_Impl(Handle_Type      => opencl.Program_ID,
                                               Release_Callback => opencl.Release_Program);
   begin
      Cleanup.Release(This.handle);
   end Finalize;

   procedure Finalize(This: in out Kernel) is
      package Cleanup is new Finalization_Impl(Handle_Type      => opencl.Kernel_ID,
                                               Release_Callback => opencl.Release_Kernel);
   begin
      Cleanup.Release(This.handle);
   end Finalize;

   procedure Finalize(This: in out Event) is
      package Cleanup is new Finalization_Impl(Handle_Type      => opencl.Event_ID,
                                               Release_Callback => opencl.Release_Event);
   begin
      Cleanup.Release(This.handle);
   end Finalize;

   procedure Finalize(This: in out Command_Queue) is
      package Cleanup is new Finalization_Impl(Handle_Type      => opencl.Command_Queue,
                                               Release_Callback => opencl.Release_Command_Queue);
   begin
      Cleanup.Release(This.handle);
   end Finalize;

end cl_objects;
