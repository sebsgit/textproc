with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System; use System;
with System.Address_Image;
with System.Address_To_Access_Conversions;

with opencl_api_spec;
with dl_loader;

package body opencl is

   use opencl_api_spec;

   type C_Address_Array is array (Interfaces.C.size_t range 1 .. 64) of aliased Raw_Address;
   type C_Char_Buffer is array (Interfaces.C.size_t range 1 .. 1024) of aliased Interfaces.C.char;
   type C_SizeT_Array is array (Interfaces.C.size_t range 1 .. 64) of aliased Interfaces.C.size_t;
   type Context_Property is new Long_Long_Integer;

   pragma Convention (Convention => C,
                      Entity     => C_Address_Array);
   pragma Convention (Convention => C,
                      Entity => C_Char_Buffer);

   package C_Addr_Arr_Conv is new System.Address_To_Access_Conversions(Object => C_Address_Array);
   package C_Char_Buff_Conv is new System.Address_To_Access_Conversions(Object => C_Char_Buffer);
   package C_SizeT_Arr_Conv is new System.Address_To_Access_Conversions(Object => C_SizeT_Array);

   cl_lib_handle: dl_loader.Handle;

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
         result := opencl_api_spec.Load_From(cl_lib_handle);
      end if;
      return (if result then SUCCESS else INVALID_VALUE);
   end Init;

   function Get_Platforms(result_status: out Status) return Platforms is
      function Impl(num_entries_p: Interfaces.C.unsigned; platforms_ptr_p: System.Address; num_platforms_p: access Interfaces.C.unsigned) return Interfaces.C.int
        with Import,
        Address => clGetPlatformIDs,
        Convention => C;

      num_platforms: aliased Interfaces.C.unsigned := 0;
      platforms_ptr: aliased C_Address_Array := (others => 0);
      cl_code: Interfaces.C.int;
      current_index: Interfaces.C.size_t := 1;
      null_platforms: constant Platforms(1 .. 0) := (others => 0);
   begin
      if clGetPlatformIDs = System.Null_Address then
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
          Address => clGetPlatformInfo,
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

      return To_Ada(string_ret, size_ret - 1);
   end Get_Platform_Info;

   function Get_Devices(id: in Platform_ID; dev_type: in Device_Type; result_status: out Status) return Devices is
      null_devices: constant Devices(1 .. 0) := (others => 0);

      function Impl(p: Raw_Address; dev_t: Interfaces.C.unsigned; num_entries: Interfaces.C.unsigned; out_devices: System.Address; num_devs: access Interfaces.C.unsigned)
                    return Interfaces.C.int
        with Import,
        Address => clGetDeviceIDs,
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

   function Get_Device_Info(id: in Device_ID; info: in Device_Info_Bool; result_status: out Status) return Boolean is
      function Impl(p: Raw_Address; dev_i: Interfaces.C.unsigned; res_size: Interfaces.C.size_t; out_info: access Interfaces.C.unsigned; info_len: System.Address) return Interfaces.C.int
        with Import,
        Address => clGetDeviceInfo,
        Convention => C;

      flag_value: aliased Interfaces.C.unsigned := 0;
      cl_status: Interfaces.C.int := 0;
   begin
      cl_status := Impl(p         => Raw_Address(id),
                        dev_i     => Device_Info_Bool'Enum_Rep(info),
                        res_size => Interfaces.C.unsigned'Size,
                        out_info  => flag_value'Access,
                        info_len  => System.Null_Address);
      result_status := Status'Enum_Val(cl_status);
      return (if flag_value = 0 then False else True);
   end Get_Device_Info;

   function Get_Device_Info(id: in Device_ID; info: in Device_Info_String; result_status: out Status) return String is
      function Impl(p: Raw_Address; dev_i: Interfaces.C.unsigned; res_size: Interfaces.C.size_t; out_info: System.Address; info_len: access Interfaces.C.size_t) return Interfaces.C.int
        with Import,
        Address => clGetDeviceInfo,
        Convention => C;

      null_string: constant String(1 .. 0) := (others => ' ');
      cl_status: Interfaces.C.int := 0;
      buffer: aliased C_Char_Buffer;
      actual_length: aliased Interfaces.C.size_t := 0;
   begin
      cl_status := Impl(p         => Raw_Address(id),
                        dev_i     => Device_Info_String'Enum_Rep(info),
                        res_size => buffer'Length,
                        out_info  => C_Char_Buff_Conv.To_Address(buffer'Unchecked_Access),
                        info_len  => actual_length'Access);
      result_status := Status'Enum_Val(cl_status);
      if result_status = SUCCESS then
         return To_Ada(buffer, actual_length - 1);
      end if;
      return null_string;
   end Get_Device_Info;

   function Create_Context(context_platform: in Platform_ID; context_device: in Device_ID; result_status: out Status) return Context_ID is
      function Impl(ctx_props: System.Address; num_devs: Interfaces.C.unsigned; devs: System.Address; cb: System.Address; user_data: System.Address; err_code: access Interfaces.C.int)
                    return Raw_Address
        with Import,
        Address => clCreateContext,
        Convention => C;

      properties: aliased C_Address_Array := (others => 0);
      dev_ids: aliased C_Address_Array := (others => 0);
      err_code: aliased Interfaces.C.int := 0;
      ctx_id: Raw_Address := 0;
   begin
      dev_ids(1) := Raw_Address(context_device);
      properties(1) := Raw_Address(Context_Properties'Enum_Rep(CONTEXT_PROP_PLATFORM));
      properties(2) := Raw_Address(context_platform);

      ctx_id := Impl(ctx_props => C_Addr_Arr_Conv.To_Address(properties'Unchecked_Access),
                     num_devs  => 1,
                     devs      => C_Addr_Arr_Conv.To_Address(dev_ids'Unchecked_Access),
                     cb        => System.Null_Address,
                     user_data => System.Null_Address,
                     err_code  => err_code'Access);
      result_status := Status'Enum_Val(err_code);
      return Context_ID(ctx_id);
   end Create_Context;

   function Release_Context(id: in Context_ID) return Status is
      function Impl(p: Raw_Address) return Interfaces.C.int
        with Import,
        Address => clReleaseContext,
        Convention => C;
      cl_code: Interfaces.C.int := 0;
   begin
      cl_code := Impl(Raw_Address(id));
      return Status'Enum_Val(cl_code);
   end Release_Context;

   function Create_Program(ctx: in Context_ID; source: in String; result_status: out Status) return Program_ID is
      function Impl(ctx: Raw_Address; count: Interfaces.C.unsigned; strs: access Interfaces.C.Strings.chars_ptr; lengths: System.Address; err_code: access Interfaces.C.int) return Raw_Address
        with Import,
        Address => clCreateProgramWithSource,
        Convention => C;
      prog_id: Raw_Address := 0;
      err_code: aliased Interfaces.C.int := 0;
      lengths: aliased C_SizeT_Array := (others => 0);
      source_ptr: aliased Interfaces.C.Strings.chars_ptr;
   begin
      lengths(1) := source'Length;
      source_ptr := Interfaces.C.Strings.New_String(Str => source);

      prog_id := Impl(ctx      => Raw_Address(ctx),
                      count    => 1,
                      strs     => source_ptr'Access,
                      lengths  => C_SizeT_Arr_Conv.To_Address(lengths'Unchecked_Access),
                      err_code => err_code'Access);
      result_status := Status'Enum_Val(err_code);
      Interfaces.C.Strings.Free(Item => source_ptr);
      return Program_ID(prog_id);
   end Create_Program;

   function Build_Program(id: in Program_ID; device: in Device_ID; options: in String) return Status is
      function Impl(p_id: Raw_Address;
                    num_devices: Interfaces.C.unsigned;
                    device_list: System.Address;
                    options: Interfaces.C.Strings.char_array_access;
                    user_cb: System.Address;
                    user_data:  System.Address) return Interfaces.C.int
        with Import,
        Address => clBuildProgram,
        Convention => C;

      cl_code: Interfaces.C.int := 0;
      device_array: aliased C_Address_Array := (others => 0);
      opts: aliased Interfaces.C.char_array := Interfaces.C.To_C(options);
   begin
      device_array(1) := Raw_Address(device);
      cl_code := Impl(p_id        => Raw_Address(id),
                      num_devices => 1,
                      device_list => C_Addr_Arr_Conv.To_Address(device_array'Unchecked_Access),
                      options     => opts'Unchecked_Access,
                      user_cb     => System.Null_Address,
                      user_data   => System.Null_Address);
      return Status'Enum_Val(cl_code);
   end Build_Program;

   function Get_Program_Build_Log(id: in Program_ID; device: in Device_ID; result_status: out Status) return String is
      function Impl(prog: Raw_Address; dev: Raw_Address; info: Interfaces.C.unsigned; available_size: Interfaces.C.size_t; ptr: System.Address; ret_size: access Interfaces.C.size_t) return Interfaces.C.int
        with Import,
        Address => clGetProgramBuildInfo,
        Convention => C;
      build_log_size: aliased Interfaces.C.size_t;
      cl_code: Interfaces.C.int := 0;
   begin
      cl_code := Impl(prog           => Raw_Address(id),
                      dev            => Raw_Address(device),
                      info           => Program_Build_Info_String'Enum_Rep(PROGRAM_BUILD_LOG),
                      available_size => 0,
                      ptr            => System.Null_Address,
                      ret_size       => build_log_size'Access);
      declare
         type C_Char_Buff is new Interfaces.C.char_array(1 .. build_log_size);
         build_log_buffer: aliased C_Char_Buff := (1 .. build_log_size => Interfaces.C.To_C(ASCII.NUL));

         package Char_Arr_Addr_Conv is new System.Address_To_Access_Conversions(Object => C_Char_Buff);
      begin
         cl_code := Impl(prog           => Raw_Address(id),
                         dev            => Raw_Address(device),
                         info           => Program_Build_Info_String'Enum_Rep(PROGRAM_BUILD_LOG),
                         available_size => build_log_size,
                         ptr            => Char_Arr_Addr_Conv.To_Address(build_log_buffer'Unchecked_Access),
                         ret_size       => build_log_size'Access);
         result_status := Status'Enum_Val(cl_code);
         return Interfaces.C.To_Ada(Interfaces.C.char_array(build_log_buffer));
      end;
   end Get_Program_Build_Log;

   function Release_Program(id: in Program_ID) return Status is
      function Impl(p: Raw_Address) return Interfaces.C.int
        with Import,
        Address => clReleaseProgram,
        Convention => C;
      cl_code: Interfaces.C.int := 0;
   begin
      cl_code := Impl(Raw_Address(id));
      return Status'Enum_Val(cl_code);
   end Release_Program;

   function Create_Kernel(program: in Program_ID; name: in String; result_status: out Status) return Kernel_ID is
      function Impl(prog: Raw_Address; name_str: Interfaces.C.Strings.char_array_access; err_c: access Interfaces.C.int) return Raw_Address
        with Import,
        Address => clCreateKernel,
        Convention => C;

      cl_code: aliased Interfaces.C.int := 0;
      str_ptr: aliased Interfaces.C.char_array := Interfaces.C.To_C(name);
      result: Raw_Address := 0;
   begin
      result := Impl(prog     => Raw_Address(program),
                     name_str => str_ptr'Unchecked_Access,
                     err_c    => cl_code'Access);
      result_status := Status'Enum_Val(cl_code);
      return Kernel_ID(result);
   end Create_Kernel;

   function Release_Kernel(id: in Kernel_ID) return Status is
      function Impl(p: Raw_Address) return Interfaces.C.int
        with Import,
        Address => clReleaseKernel,
        Convention => C;
      cl_code: Interfaces.C.int := 0;
   begin
      cl_code := Impl(Raw_Address(id));
      return Status'Enum_Val(cl_code);
   end Release_Kernel;

   function Set_Kernel_Arg(id: in Kernel_ID; index: Natural; size: Positive; address: System.Address) return Status is
      function Impl(k_id: Raw_Address; arg_index: Interfaces.C.unsigned; arg_size: Interfaces.C.size_t; arg_addr: System.Address) return Interfaces.C.int
        with Import,
        Address => clSetKernelArg,
        Convention => C;
      cl_code: Interfaces.C.int;
      arg_index: constant Interfaces.C.unsigned := Interfaces.C.unsigned(index);
      arg_size: constant Interfaces.C.size_t := Interfaces.C.size_t(size);
   begin
      cl_code := Impl(k_id      => Raw_Address(id),
                      arg_index => arg_index,
                      arg_size  => arg_size,
                      arg_addr  => address);
      return Status'Enum_Val(cl_code);
   end Set_Kernel_Arg;

   function Enqueue_Kernel(queue: in Command_Queue;
                           kernel: in Kernel_ID;
                           global_offset: in Offsets;
                           global_work_size,
                           local_work_size: in Dimensions;
                           event_wait_list: in Events;
                           event: out Event_ID) return Status is
      function Impl(q_id, k_id: Raw_Address;
                    dims: Interfaces.C.unsigned;
                    glob_off, glob_ws, local_ws: access C_SizeT_Array;
                    num_wait_events: Interfaces.C.unsigned;
                    event_wait: System.Address;
                    event_res: access Raw_Address) return Interfaces.C.int
        with Import,
        Address => clEnqueueNDRangeKernel,
        Convention => C;

      event_result: aliased Raw_Address;
      result: Interfaces.C.int := 0;
      global_ws_array: aliased C_SizeT_Array := (others => 0);
      local_ws_array: aliased C_SizeT_Array := (others => 0);
      global_off_array: aliased C_SizeT_Array := (others => 0);
      event_wait_array: aliased C_Address_Array := (others => 0);
   begin
      for i in 1 .. global_offset'Length loop
         global_off_array(Interfaces.C.size_t(i)) := Interfaces.C.size_t(global_offset(i));
      end loop;
      for i in 1 .. global_work_size'Length loop
         global_ws_array(Interfaces.C.size_t(i)) := Interfaces.C.size_t(global_work_size(i));
      end loop;
      for i in 1 .. local_work_size'Length loop
         local_ws_array(Interfaces.C.size_t(i)) := Interfaces.C.size_t(local_work_size(i));
      end loop;
      for i in 1 .. event_wait_list'Length loop
         event_wait_array(Interfaces.C.size_t(i)) := Raw_Address(event_wait_list(i));
      end loop;
      result := Impl(q_id       => Raw_Address(queue),
                     k_id       => Raw_Address(kernel),
                     dims       => global_work_size'Length,
                     glob_off   => global_off_array'Access,
                     glob_ws    => global_ws_array'Access,
                     local_ws   => local_ws_array'Access,
                     num_wait_events => event_wait_list'Length,
                     event_wait => (if event_wait_list'Length = 0 then System.Null_Address else C_Addr_Arr_Conv.To_Address(event_wait_array'Unchecked_Access)),
                     event_res  => event_result'Access);
      event := Event_ID(event_result);
      return Status'Enum_Val(result);
   end Enqueue_Kernel;

   function Create_Command_Queue(ctx: in Context_ID; dev: in Device_ID; result_status: out Status) return Command_Queue is
      function Impl(ctx_id: Raw_Address; dev_id: Raw_Address; props: System.Address; err_c: access Interfaces.C.int) return Raw_Address
        with Import,
        Address => clCreateCommandQueueWithProperties,
        Convention => C;
      cl_code: aliased Interfaces.C.int := 0;
      result: Raw_Address := 0;
   begin
      result := Impl(ctx_id => Raw_Address(ctx),
                     dev_id => Raw_Address(dev),
                     props  => System.Null_Address,
                     err_c  => cl_code'Access);
      result_status := Status'Enum_Val(cl_code);
      return Command_Queue(result);
   end Create_Command_Queue;

   function Release_Command_Queue(id: in Command_Queue) return Status is
      function Impl(p: Raw_Address) return Interfaces.C.int
        with Import,
        Address => clReleaseCommandQueue,
        Convention => C;
      cl_code: Interfaces.C.int := 0;
   begin
      cl_code := Impl(Raw_Address(id));
      return Status'Enum_Val(cl_code);
   end Release_Command_Queue;

   function Wait_For_Events(ev_list: Events) return Status is
      function Impl(num_events: Interfaces.C.unsigned; event_arr: access C_Address_Array) return Interfaces.C.int
        with Import,
        Address => clWaitForEvents,
        Convention => C;
      cl_code: Interfaces.C.int := 0;
      event_arr: aliased C_Address_Array := (others => 0);
   begin
      for i in 1 .. ev_list'Length loop
         event_arr(Interfaces.C.size_t(i)) := Raw_Address(ev_list(i));
      end loop;
      cl_code := Impl(num_events => ev_list'Length,
                      event_arr  => event_arr'Access);
      return Status'Enum_Val(cl_code);
   end Wait_For_Events;

   function Release_Event(ev: in Event_ID) return Status is
      function Impl(p: Raw_Address) return Interfaces.C.int
        with Import,
        Address => clReleaseEvent,
        Convention => C;
      cl_code: Interfaces.C.int := 0;
   begin
      cl_code := Impl(Raw_Address(ev));
      return Status'Enum_Val(cl_code);
   end Release_Event;

   function Finish(queue: in Command_Queue) return Status is
      function Impl(p: Raw_Address) return Interfaces.C.int
        with Import,
        Address => clFinish,
        Convention => C;
      cl_code: Interfaces.C.int := 0;
   begin
      cl_code := Impl(Raw_Address(queue));
      return Status'Enum_Val(cl_code);
   end Finish;

end opencl;
