with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with System; use System;
with System.Address_Image;
with System.Address_To_Access_Conversions;

with dl_loader;

package body opencl is

   type C_Address_Array is array (Interfaces.C.size_t range 1 .. 64) of aliased Raw_Address;
   type C_Char_Buffer is array (Interfaces.C.size_t range 1 .. 1024) of aliased Interfaces.C.char;

   pragma Convention (Convention => C,
                      Entity     => C_Address_Array);
   pragma Convention (Convention => C,
                      Entity => C_Char_Buffer);

   package C_Addr_Arr_Conv is new System.Address_To_Access_Conversions(Object => C_Address_Array);
   package C_Char_Buff_Conv is new System.Address_To_Access_Conversions(Object => C_Char_Buffer);

   cl_lib_handle: dl_loader.Handle;

   clGetPlatformIDs_addr: System.Address;
   clGetPlatformInfo_addr: System.Address;
   clGetDeviceIDs_addr: System.Address;

   function To_Ada(s: in C_Char_Buffer; size: Interfaces.C.size_t) return String is
      result: String(1 .. Integer(size)) := (others => ASCII.NUL);
   begin
      for idx in 1 .. size loop
         result(Integer(idx)) := Interfaces.C.To_Ada(s(idx));
      end loop;
      return result;
   end To_Ada;

   function Init(path: String) return Status is
      result: Boolean;
   begin
      result := dl_loader.Open(path, cl_lib_handle);
      if result then
         clGetPlatformIDs_addr := dl_loader.Get_Symbol(cl_lib_handle, "clGetPlatformIDs");
         clGetPlatformInfo_addr := dl_loader.Get_Symbol(cl_lib_handle, "clGetPlatformInfo");
         clGetDeviceIDs_addr := dl_loader.Get_Symbol(cl_lib_handle, "clGetDeviceIDs");
      end if;
      return (if result then SUCCESS else INVALID_VALUE);
   end Init;

   function Get_Platforms(result_status: out Status) return Platforms is
      function Impl(num_entries_p: Interfaces.C.unsigned; platforms_ptr_p: System.Address; num_platforms_p: access Interfaces.C.unsigned) return Interfaces.C.int
        with Import,
        Address => clGetPlatformIDs_addr,
        Convention => C;

      num_platforms: aliased Interfaces.C.unsigned := 0;
      platforms_ptr: aliased C_Address_Array := (others => 0);
      cl_code: Interfaces.C.int;
      current_index: Interfaces.C.size_t := 1;
      null_platforms: constant Platforms(1 .. 0) := (others => 0);
   begin
      if clGetPlatformIDs_addr = System.Null_Address then
         result_status := INVALID_PLATFORM;
         return null_platforms;
      end if;
      cl_code := Impl(platforms_ptr'Length,
                      C_Addr_Arr_Conv.To_Address(platforms_ptr'Unchecked_Access),
                      num_platforms'Access);
      result_status := Status'Enum_Val(cl_code);
      if result_status = SUCCESS then
         return result: Platforms(1 .. Integer(num_platforms)) do
            for ptr of result loop
               if current_index <= Interfaces.C.size_t(num_platforms) then
                  ptr := Platform_ID(platforms_ptr(current_index));
               else
                  ptr := Platform_ID(0);
               end if;
               current_index := current_index + 1;
            end loop;
         end return;

      end if;

      return null_platforms;
   end Get_Platforms;

   function Get_Platform_Info(id: in Platform_ID; info: Platform_Info; result_status: out Status) return String is
      function Impl(p: Raw_Address; info: Interfaces.C.unsigned; val_size: Interfaces.C.size_t; val: System.Address; val_size_ret: access Interfaces.C.size_t)
                    return Interfaces.C.int
        with
          Import,
          Address => clGetPlatformInfo_addr,
          Convention => C;

      cl_code: Interfaces.C.Int;
      size_ret: aliased Interfaces.C.size_t;
      string_ret: aliased C_Char_Buffer;
   begin
      cl_code := Impl(Raw_Address(id),
                      Platform_Info'Enum_Rep(info),
                      string_ret'Length,
                      C_Char_Buff_Conv.To_Address(string_ret'Unchecked_Access),
                      size_ret'Access);
      result_status := Status'Enum_Val(cl_code);

      return result: String(1 .. Integer(size_ret - 1)) do
         result := To_Ada(string_ret, size_ret - 1);
      end return;
   end Get_Platform_Info;

   function Get_Devices(id: in Platform_ID; dev_type: in Device_Type; result_status: out Status) return Devices is
      null_devices: constant Devices(1 .. 0) := (others => 0);

      function Impl(p: Raw_Address; dev_t: Interfaces.C.unsigned; num_entries: Interfaces.C.unsigned; out_devices: System.Address; num_devs: access Interfaces.C.unsigned)
                    return Interfaces.C.int
        with Import,
        Address => clGetDeviceIDs_addr,
        Convention => C;

      num_devices: aliased Interfaces.C.unsigned := 0;
      device_ids: aliased C_Address_Array := (others => 0);
      cl_res: Interfaces.C.int;
   begin
      cl_res := Impl(p           => Raw_Address(id),
                     dev_t       => Device_Type'Enum_Rep(dev_type),
                     num_entries => device_ids'Length,
                     out_devices => C_Addr_Arr_Conv.To_Address(device_ids'Unchecked_Access),
                     num_devs    => num_devices'Access);
      result_status := Status'Enum_Val(cl_res);
      if result_status = SUCCESS then
         return devs: Devices(1 .. Integer(num_devices)) do
            for idx in 1 .. num_devices loop
               devs(Integer(idx)) := Device_ID(device_ids(Interfaces.C.size_t(idx)));
            end loop;
         end return;
      end if;

      return null_devices;
   end Get_Devices;

end opencl;
