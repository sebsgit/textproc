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
                  devs: constant opencl.Devices := opencl.Get_Devices(id            => p,
                                                                      dev_type      => opencl.DEVICE_TYPE_ALL,
                                                                      result_status => cl_status);
               begin
                  Assert(devs'Length > 0, "no devices");
               end;
            end;
         end loop;
      end;
   end testLoad;

end OpenCLTests;
