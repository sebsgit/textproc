with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;

with dl_loader;
with opencl; use opencl;
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
                                 wait_ev: Events(1 .. 0);
                                 ev_to_sync: Events(1 .. 1) := (others => 0);
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

end OpenCLTests;
