with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with System.Address_To_Access_Conversions;

with dl_loader;
with opencl; use opencl;
with cl_objects; use cl_objects;
with System;
with System.Address_Image;

with Ada.Environment_Variables;

package body OpenCLTests is

   type Arch_Type is (ARCH_32, ARCH_64);
   --TODO better check
   function Get_OpenCL_Path(arch: Arch_Type) return String is
      is_linux: constant Boolean := Ada.Environment_Variables.Exists("HOME");
   begin
      if is_linux then
         return "TODO";
      else
         return (if arch = ARCH_64 then "C:/Windows/SysWOW64/OpenCL.dll" else "C:/Windows/System32/OpenCL.dll");
      end if;
   end Get_OpenCL_Path;

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testLoad'Access, "load runtime opencl");
      Register_Routine (T, testObjectAPI'Access, "opencl object API");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("OpenCL Tests");
   end Name;

   procedure testLoad(T : in out Test_Cases.Test_Case'Class) is
      cl_status: opencl.Status;
      path: constant String := Get_OpenCL_Path(ARCH_32);
   begin
      Assert(path'Length > 0, "invalid opencl path");

      cl_status := opencl.Init(path);
      Assert(cl_status = opencl.SUCCESS, "can't open library: " & path);

      declare
         platf_ids: constant opencl.Platforms := opencl.Get_Platforms(cl_status);
      begin
         Assert(platf_ids'Length > 0, "no opencl platforms");
         for p of platf_ids loop
            declare
               p_name: constant String := opencl.Get_Platform_Info(id     => p,
                                                                   info   => opencl.PLATFORM_NAME,
                                                                   result_status => cl_status);
            begin
               Assert(p_name'Length > 0, "invalid platform name");
               Ada.Text_IO.Put_Line("Platform: -> " & p_name);
               declare
                  dev_ids: constant opencl.Devices := opencl.Get_Devices(id            => p,
                                                                         dev_type      => opencl.DEVICE_TYPE_ALL,
                                                                         result_status => cl_status);
               begin
                  Assert(dev_ids'Length > 0, "no devices");
                  for d_id of dev_ids loop
                     declare
                        d_name: constant String := opencl.Get_Device_Info(id            => d_id,
                                                                          info          => opencl.DEVICE_NAME,
                                                                          result_status => cl_status);
                        ctx_id: constant opencl.Context_ID := opencl.Create_Context(context_platform => p,
                                                                                    context_device   => d_id,
                                                                                    result_status    => cl_status);
                     begin
                        Ada.Text_IO.Put_Line("--> device: " & d_name);

                        Assert(cl_status = opencl.SUCCESS, "can't create context");

                        declare
                           prog_source: constant String := "__kernel void empty_kernel() {}";
                           prog_id: constant opencl.Program_ID := opencl.Create_Program(ctx           => ctx_id,
                                                                                        source        => prog_source,
                                                                                        result_status => cl_status);
                           queue_status: opencl.Status;
                           queue_id: constant opencl.Command_Queue := opencl.Create_Command_Queue(ctx           => ctx_id,
                                                                                                  dev           => d_id,
                                                                                                  result_status => queue_status);
                        begin
                           Assert(cl_status = opencl.SUCCESS, "create program");
                           Assert(queue_status = opencl.SUCCESS, "create queue");
                           cl_status := opencl.Build_Program(id      => prog_id,
                                                             device  => d_id,
                                                             options => "-w -Werror");
                           if cl_status /= opencl.SUCCESS then
                              declare
                                 log_status: opencl.Status;
                              begin
                                 Ada.Text_IO.Put_Line("Compile log: " & opencl.Get_Program_Build_Log(id            => prog_id,
                                                                                                     device        => d_id,
                                                                                                     result_status => log_status));
                              end;
                           end if;
                           Assert(cl_status = opencl.SUCCESS, "Build program");

                           declare
                              kernel: constant Kernel_ID := opencl.Create_Kernel(program       => prog_id,
                                                                                 name          => "empty_kernel",
                                                                                 result_status => cl_status);
                           begin
                              Assert(cl_status = opencl.SUCCESS, "create kernel");

                              declare
                                 event: Event_ID := 0;
                                 glob_off: constant Offsets(1 .. 2) := (others => 0);
                                 glob_ws: constant Dimensions(1 .. 2) := (1 => 1, 2=> 1);
                                 loc_ws: constant Dimensions(1 .. 2) := (1 => 1, 2=> 1);
                                 wait_ev: opencl.Events(1 .. 0);
                                 ev_to_sync: opencl.Events(1 .. 1) := (others => 0);
                              begin
                                 cl_status := opencl.Enqueue_Kernel(queue            => queue_id,
                                                                    kernel           => kernel,
                                                                    global_offset    => glob_off,
                                                                    global_work_size => glob_ws,
                                                                    local_work_size  => loc_ws,
                                                                    event_wait_list  => wait_ev,
                                                                    event            => event);
                                 Assert(cl_status = opencl.SUCCESS, "enqueue kernel");

                                 ev_to_sync(1) := event;
                                 cl_status := opencl.Wait_For_Events(ev_to_sync);
                                 Assert(cl_status = opencl.SUCCESS, "wait for events");

                                 cl_status := opencl.Finish(queue_id);
                                 Assert(cl_status = opencl.SUCCESS, "cl finish");

                                 cl_status := opencl.Release_Event(event);
                                 Assert(cl_status = opencl.SUCCESS, "release event");
                              end;

                              cl_status := opencl.Release_Kernel(kernel);
                              Assert(cl_status = opencl.SUCCESS, "release kernel");
                           end;

                           cl_status := opencl.Release_Program(prog_id);
                           Assert(cl_status = opencl.SUCCESS, "release program");
                           queue_status := opencl.Release_Command_Queue(queue_id);
                           Assert(queue_status = opencl.SUCCESS, "release queue");
                        end;

                        cl_status := opencl.Release_Context(ctx_id);
                        Assert(cl_status = opencl.SUCCESS, "can't release context");
                     end;
                  end loop;
               end;
            end;
         end loop;
      end;
   end testLoad;

   procedure testObjectAPI(T: in out Test_Cases.Test_Case'Class) is
      cl_status: opencl.Status;
      platf_ids: constant opencl.Platforms := opencl.Get_Platforms(cl_status);
   begin
      Assert(platf_ids'Length > 0, "no opencl platforms");
      for p of platf_ids loop
         declare
            dev_ids: constant opencl.Devices := opencl.Get_Devices(id            => p,
                                                                   dev_type      => opencl.DEVICE_TYPE_ALL,
                                                                   result_status => cl_status);
         begin
            Assert(dev_ids'Length > 0, "no devices");
            for d_id of dev_ids loop
               declare
                  ctx: cl_objects.Context := cl_objects.Create(context_platform => p,
                                                               context_device   => d_id,
                                                               result_status    => cl_status);
               begin
                  Assert(cl_status = opencl.SUCCESS, "create ctx");
                  declare
                     prog: cl_objects.Program := ctx.Create_Program(source        => "__kernel void empty_func() {}",
                                                                    result_status => cl_status);
                     prog2: cl_objects.Program := ctx.Create_Program(source        => "__kernel void copy_int(int src, __global int *dest) {*dest += src;}",
                                                                     result_status => cl_status);
                  begin
                     Assert(cl_status = opencl.SUCCESS, "create prog");

                     cl_status := prog.Build(d_id, "-w -Werror");
                     Assert(cl_status = opencl.SUCCESS, "build prog");

                     cl_status := prog2.Build(d_id, "-w -Werror");
                     Assert(cl_status = opencl.SUCCESS, "build prog 2: " & cl_status'Image & " / " & prog2.Get_Build_Log(d_id));
                     declare
                        q_status: opencl.Status;
                        kern: cl_objects.Kernel := prog.Create_Kernel(name          => "empty_func",
                                                                      result_status => cl_status);
                        queue: cl_objects.Command_Queue := ctx.Create_Command_Queue(dev           => d_id,
                                                                                    result_status => q_status);
                     begin
                        Assert(cl_status = opencl.SUCCESS, "create kernel");
                        Assert(q_status = opencl.SUCCESS, "create command queue");

                        declare
                           kern2: cl_objects.Kernel := prog2.Create_Kernel(name          => "copy_int",
                                                                           result_status => cl_status);

                           package Addr_Conv is new System.Address_To_Access_Conversions(Object => Interfaces.C.int);

                           source_value: aliased Interfaces.C.int := 15;
                           dest_value: aliased Interfaces.C.int := 7;

                           buff_status: opencl.Status;
                           buff: cl_objects.Buffer := ctx.Create_Buffer(flags         => (ALLOC_HOST_PTR => True, others => False),
                                                                        size          => dest_value'Size / 8,
                                                                        host_ptr      => System.Null_Address,
                                                                        result_status => buff_status);
                        begin
                           Assert(cl_status = opencl.SUCCESS, "create kernel 2");
                           Assert(buff_status = opencl.SUCCESS, "create buffer");

                           cl_status := kern2.Set_Arg(0, Interfaces.C.int'Size / 8, Addr_Conv.To_Address(source_value'Access));
                           Assert(cl_status = opencl.SUCCESS, "set arg 0: " & cl_status'Image);

                           cl_status := kern2.Set_Arg(1, opencl.Raw_Address'Size / 8, buff.Address);
                           Assert(cl_status = opencl.SUCCESS, "set arg 1: " & cl_status'Image);

                           declare
                              ev_to_wait: cl_objects.Events(1 .. 0);
                              write_ev: cl_objects.Event := queue.Enqueue_Write(mem_ob             => buff,
                                                                                offset             => 0,
                                                                                size               => dest_value'Size / 8,
                                                                                ptr                => Addr_Conv.To_Address(dest_value'Access),
                                                                                events_to_wait_for => ev_to_wait,
                                                                                code               => cl_status);
                           begin
                              Assert(cl_status = opencl.SUCCESS, "Enqueue write: " & cl_status'Image);
                              cl_status := write_ev.Wait;
                              Assert(cl_status = opencl.SUCCESS, "Wait for write: " & cl_status'Image);
                              dest_value := 0;

                              declare
                                 read_ev: cl_objects.Event := queue.Enqueue_Read(mem_ob             => buff,
                                                                                 offset             => 0,
                                                                                 size               => dest_value'Size / 8,
                                                                                 ptr                => Addr_Conv.To_Address(dest_value'Access),
                                                                                 events_to_wait_for => ev_to_wait,
                                                                                 code               => cl_status);
                              begin
                                 Assert(cl_status = opencl.SUCCESS, "Enqueue read: " & cl_status'Image);
                                 cl_status := read_ev.Wait;
                                 Assert(cl_status = opencl.SUCCESS, "Wait for read: " & cl_status'Image);
                                 Assert(dest_value = 7, "Failed to read from cl buffer: " & dest_value'Image);
                              end;

                           end;

                           declare
                              ev_to_wait: cl_objects.Events(1 .. 0);
                              kern_ev: cl_objects.Event := queue.Enqueue_Kernel(kern               => kern2,
                                                                                glob_ws            => (1 => 1, 2=> 1),
                                                                                loc_ws             => (1 => 1, 2 => 1),
                                                                                events_to_wait_for => ev_to_wait,
                                                                                code               => cl_status);
                           begin
                              Assert(cl_status = opencl.SUCCESS, "kernel run: " & cl_status'Image);
                              cl_status := kern_ev.Wait;
                              declare
                                 read_ev: cl_objects.Event := queue.Enqueue_Read(mem_ob             => buff,
                                                                                 offset             => 0,
                                                                                 size               => dest_value'Size / 8,
                                                                                 ptr                => Addr_Conv.To_Address(dest_value'Access),
                                                                                 events_to_wait_for => ev_to_wait,
                                                                                 code               => cl_status);
                              begin
                                 Assert(cl_status = opencl.SUCCESS, "read buff: " & cl_status'Image);
                              end;
                           end;
                           cl_status := queue.Finish;
                           Assert(cl_status = opencl.SUCCESS, "finish queue");
                           Assert(dest_value = 22, "Failed kernel run");
                        end;
                     end;
                  end;
               end;
            end loop;
         end;
      end loop;
   end testObjectAPI;

end OpenCLTests;
