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

end cl_objects;
