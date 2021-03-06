with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

package body cl_objects is
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

   function Create(context_platform: in Platform_ID; context_device: in Device_ID; result_status: out Status) return Context is
   begin
      return ctx: Context do
         ctx.device := context_device;
         ctx.handle := opencl.Create_Context(context_platform => context_platform,
                                             context_device   => context_device,
                                             result_status    => result_status);
      end return;
   end Create;

   function Create_Gpu(result_status: out Status) return Context is
      platf_id: Platform_ID := 0;
      dev_id: Device_ID := 0;
   begin
      Find_Gpu_Device(platf_id, dev_id);
      return Create(platf_id, dev_id, result_status);
   end Create_Gpu;

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

   function Create_Command_Queue(ctx: in out Context'Class; result_status: out Status) return Command_Queue is
   begin
      return Create_Command_Queue(ctx, ctx.device, result_status);
   end Create_Command_Queue;

   function Get_ID(ctx: in out Context'Class) return Context_ID is
   begin
      return ctx.handle;
   end Get_ID;

   function Create_Buffer(ctx: in out Context'Class; flags: in Mem_Flags; size: Positive; host_ptr: System.Address; result_status: out Status) return Buffer is
   begin
      return buff: Buffer do
         buff.handle := opencl.Create_Buffer(ctx           => ctx.handle,
                                             flags         => flags,
                                             size          => size,
                                             host_ptr      => host_ptr,
                                             result_status => result_status);
      end return;
   end Create_Buffer;

   function Enqueue_Write(queue: in out Command_Queue'Class; mem_ob: in out Buffer'Class; offset: Natural; size: Positive; ptr: System.Address; events_to_wait_for: in Events; code: out Status) return Event is
      ev_handle: Event_ID;
   begin
      return ev: Event do
         code := opencl.Enqueue_Write(queue                => queue.handle,
                                      mem_ob             => mem_ob.handle,
                                      block_write        => False,
                                      offset             => offset,
                                      size               => size,
                                      ptr                => ptr,
                                      events_to_wait_for => events_to_wait_for,
                                      event              => ev_handle);
         ev.handle := ev_handle;
      end return;
   end Enqueue_Write;

   function Enqueue_Read(queue: in out Command_Queue'Class; mem_ob: in out Buffer'Class; offset: Natural; size: Positive; ptr: System.Address; events_to_wait_for: in Events; code: out Status) return Event is
      ev_handle: Event_ID;
   begin
      return ev: Event do
         code := opencl.Enqueue_Read(queue                => queue.handle,
                                     mem_ob             => mem_ob.handle,
                                     block_read        => False,
                                     offset             => offset,
                                     size               => size,
                                     ptr                => ptr,
                                     events_to_wait_for => events_to_wait_for,
                                     event              => ev_handle);
         ev.handle := ev_handle;
      end return;
   end Enqueue_Read;

   function Enqueue_Read(queue: in out Command_Queue'Class; mem_ob: in out Buffer'Class; offset: Natural; size: Positive; ptr: System.Address; code: out Status) return Event is
      no_events: opencl.Events(1 .. 0);
   begin
      return Enqueue_Read(queue, mem_ob, offset, size, ptr, no_events, code);
   end Enqueue_Read;

   function Enqueue_Kernel(queue: in out Command_Queue'Class; kern: in out Kernel'Class; glob_ws: Dimensions; loc_ws: Dimensions; events_to_wait_for: in Events; code: out Status) return Event is
      ev_handle: Event_ID;
      glob_offs: constant Offsets(glob_ws'Range) := (others => 0);
   begin
      return ev: Event do
         code := opencl.Enqueue_Kernel(queue            => queue.handle,
                                       kernel           => kern.handle,
                                       global_offset    => glob_offs,
                                       global_work_size => glob_ws,
                                       local_work_size  => loc_ws,
                                       event_wait_list  => events_to_wait_for,
                                       event            => ev_handle);
         ev.handle := ev_handle;
      end return;
   end Enqueue_Kernel;

   function Enqueue_Kernel(queue: in out Command_Queue'Class; kern: in out Kernel'Class; glob_ws: Dimensions; loc_ws: Dimensions; code: out Status) return Event is
      no_events: Events(1 .. 0);
   begin
      return Enqueue_Kernel(queue, kern, glob_ws, loc_ws, no_events, code);
   end Enqueue_Kernel;

   function Build(prog: in out Program'Class; device: in Device_ID; options: in String) return Status is
   begin
      return opencl.Build_Program(id      => prog.handle,
                                  device  => device,
                                  options => options);
   end Build;

   function Build(ctx: in out Context'Class; prog: in out Program'Class; options: in String) return Status is
   begin
      return Build(prog, ctx.device, options);
   end Build;

   function Get_Build_Log(prog: in out Program'Class; device: in Device_ID) return String is
      cl_stat: opencl.Status;
   begin
      return opencl.Get_Program_Build_Log(id            => prog.handle,
                                          device        => device,
                                          result_status => cl_stat);
   end Get_Build_Log;

   function Get_Build_Log(ctx: in out Context'Class; prog: in out Program'Class) return String is
   begin
      return Get_Build_Log(prog, ctx.device);
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

   function Wait(ev: in out Event) return Status is
      event_ids: constant opencl.Events(1 .. 1) := (1 => ev.handle);
   begin
      return opencl.Wait_For_Events(event_ids);
   end Wait;

   function Get_Handle(ev: in Event) return opencl.Event_ID is
   begin
      return ev.handle;
   end Get_Handle;

   function Create_Empty return Event is
   begin
      return ev: Event do
         ev.handle := 0;
      end return;
   end Create_Empty;

   function Create_Event(id: in Event_ID) return Event is
      cl_code: opencl.Status;
   begin
      return ev: Event do
         ev.handle := id;
         if id /= 0 then
            cl_code := opencl.Retain_Event(id);
         end if;
      end return;
   end Create_Event;

   function Finish(queue: in out Command_Queue) return Status is
   begin
      return opencl.Finish(queue.handle);
   end Finish;

   function Get_Address(buff: in out Buffer'Class) return System.Address is
      package Addr_Conv is new System.Address_To_Access_Conversions(opencl.Mem_ID);
   begin
      return Addr_Conv.To_Address(buff.handle'Access);
   end Get_Address;

   function Get_ID(buff: in Buffer) return opencl.Mem_ID is
   begin
      return buff.handle;
   end Get_ID;

   procedure Set_ID(buff: in out Buffer; id: in opencl.Mem_ID) is
   begin
      buff.handle := id;
   end Set_ID;

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

   procedure Finalize(This: in out Buffer) is
      package Cleanup is new Finalization_Impl(Handle_Type      => opencl.Mem_ID,
                                               Release_Callback => opencl.Release);
   begin
      Cleanup.Release(This.handle);
   end Finalize;

   procedure Free_Context_Impl is new Ada.Unchecked_Deallocation(Object => Context,
                                                                 Name   => Context_Access);
   procedure Free_Command_Queue_Impl is new Ada.Unchecked_Deallocation(Object => Command_Queue,
                                                                       Name   => Command_Queue_Access);
   procedure Free_Program_Impl is new Ada.Unchecked_Deallocation(Object => Program,
                                                                 Name   => Program_Access);
   procedure Free_Kernel_Impl is new Ada.Unchecked_Deallocation(Object => Kernel,
                                                                Name   => Kernel_Access);
   procedure Free_Buffer_Impl is new Ada.Unchecked_Deallocation(Object => Buffer,
                                                                Name   => Buffer_Access);

   procedure Free(ob: in out Context_Access) is
   begin
      Free_Context_Impl(ob);
   end Free;

   procedure Free(ob: in out Command_Queue_Access) is
   begin
      Free_Command_Queue_Impl(ob);
   end Free;

   procedure Free(ob: in out Program_Access) is
   begin
      Free_Program_Impl(ob);
   end Free;

   procedure Free(ob: in out Kernel_Access) is
   begin
      Free_Kernel_Impl(ob);
   end Free;

   procedure Free(ob: in out Buffer_Access) is
   begin
      Free_Buffer_Impl(ob);
   end Free;

end cl_objects;
