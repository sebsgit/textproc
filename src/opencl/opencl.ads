with System; use System;
with System.Address_To_Access_Conversions;
with cl_h;
with Interfaces.C;

package opencl is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type cl_int is new Interfaces.C.int;
   type cl_uint is new Interfaces.C.unsigned;
   type cl_uchar is new Interfaces.C.unsigned_char;
   type cl_float is new Interfaces.C.C_float;
   type cl_bool is new cl_uint;
   type cl_ulong is mod 2 ** 64;
   type cl_mem_flags is new cl_ulong;

   type Status is (INVALID_GLOBAL_WORK_SIZE, INVALID_OPERATION, INVALID_EVENT_WAIT_LIST, INVALID_WORK_GROUP_SIZE, INVALID_KERNEL_ARGS, INVALID_ARG_SIZE, INVALID_PROGRAM_EXECUTABLE, INVALID_MEM_OBJECT, INVALID_CONTEXT, INVALID_PLATFORM, INVALID_VALUE, BUILD_PROGRAM_FAILURE, OUT_OF_HOST_MEMORY, SUCCESS);
   for Status use (INVALID_GLOBAL_WORK_SIZE => cl_h.CL_INVALID_GLOBAL_WORK_SIZE,
                   INVALID_OPERATION => cl_h.CL_INVALID_OPERATION,
                   INVALID_EVENT_WAIT_LIST => cl_h.CL_INVALID_EVENT_WAIT_LIST,
                   INVALID_WORK_GROUP_SIZE => cl_h.CL_INVALID_WORK_GROUP_SIZE,
                   INVALID_KERNEL_ARGS => cl_h.CL_INVALID_KERNEL_ARGS,
                   INVALID_ARG_SIZE => cl_h.CL_INVALID_ARG_SIZE,
                   INVALID_PROGRAM_EXECUTABLE => cl_h.CL_INVALID_PROGRAM_EXECUTABLE,
                   INVALID_MEM_OBJECT => cl_h.CL_INVALID_MEM_OBJECT,
                   INVALID_CONTEXT => cl_h.CL_INVALID_CONTEXT,
                   INVALID_PLATFORM => cl_h.CL_INVALID_PLATFORM,
                   INVALID_VALUE => cl_h.CL_INVALID_VALUE,
                   BUILD_PROGRAM_FAILURE => cl_h.CL_BUILD_PROGRAM_FAILURE,
                   OUT_OF_HOST_MEMORY => cl_h.CL_OUT_OF_HOST_MEMORY,
                   SUCCESS => cl_h.CL_SUCCESS);

   type Mem_Flag is (READ_WRITE, WRITE_ONLY, READ_ONLY, USE_HOST_PTR, ALLOC_HOST_PTR, COPY_HOST_PTR, HOST_WRITE_ONLY, HOST_READ_ONLY, HOST_NO_ACCESS, KERNEL_READ_WRITE);
   for Mem_Flag use (READ_WRITE => cl_h.CL_MEM_READ_WRITE,
                     WRITE_ONLY => cl_h.CL_MEM_WRITE_ONLY,
                     READ_ONLY => cl_h.CL_MEM_READ_ONLY,
                     USE_HOST_PTR => cl_h.CL_MEM_USE_HOST_PTR,
                     ALLOC_HOST_PTR => cl_h.CL_MEM_ALLOC_HOST_PTR,
                     COPY_HOST_PTR => cl_h.CL_MEM_COPY_HOST_PTR,
                     HOST_WRITE_ONLY => cl_h.CL_MEM_HOST_WRITE_ONLY,
                     HOST_READ_ONLY => cl_h.CL_MEM_HOST_READ_ONLY,
                     HOST_NO_ACCESS => cl_h.CL_MEM_HOST_NO_ACCESS,
                     KERNEL_READ_WRITE => cl_h.CL_MEM_KERNEL_READ_AND_WRITE);

   type Mem_Flags is array (Mem_Flag) of Boolean;

   type Raw_Address is mod System.Memory_Size;

   pragma Convention (Convention => C,
                      Entity     => Raw_Address);

   type Platform_ID is new Raw_Address;
   type Platforms is array (Natural range <>) of Platform_ID;

   type Device_ID is new Raw_Address;
   type Devices is array (Natural range <>) of Device_ID;

   type Dimensions is array (Positive range<>) of Natural;
   type Offsets is array (Positive range<>) of Natural;

   type Context_ID is new Raw_Address;

   type Program_ID is new Raw_Address;
   type Kernel_ID is new Raw_Address;

   type Event_ID is new Raw_Address;
   type Events is array (Positive range<>) of Event_ID;

   type Mem_ID is new Raw_Address;

   type Command_Queue is new Raw_Address;

   type Platform_Info is (PLATFORM_PROFILE, PLATFORM_VERSION, PLATFORM_NAME, PLATFORM_VENDOR, PLATFORM_EXTENSIONS);
   for Platform_Info use (PLATFORM_PROFILE => cl_h.CL_PLATFORM_PROFILE,
                          PLATFORM_VERSION => cl_h.CL_PLATFORM_VERSION,
                          PLATFORM_NAME => cl_h.CL_PLATFORM_NAME,
                          PLATFORM_VENDOR => cl_h.CL_PLATFORM_VENDOR,
                          PLATFORM_EXTENSIONS => cl_h.CL_PLATFORM_EXTENSIONS);

   type Device_Type is (DEVICE_TYPE_DEFAULT, DEVICE_TYPE_CPU, DEVICE_TYPE_GPU, DEVICE_TYPE_ACCELERATOR, DEVICE_TYPE_CUSTOM, DEVICE_TYPE_ALL);
   for Device_Type use (DEVICE_TYPE_DEFAULT => cl_h.CL_DEVICE_TYPE_DEFAULT,
                        DEVICE_TYPE_CPU => cl_h.CL_DEVICE_TYPE_CPU,
                        DEVICE_TYPE_GPU => cl_h.CL_DEVICE_TYPE_GPU,
                        DEVICE_TYPE_ACCELERATOR => cl_h.CL_DEVICE_TYPE_ACCELERATOR,
                        DEVICE_TYPE_CUSTOM => cl_h.CL_DEVICE_TYPE_CUSTOM,
                        DEVICE_TYPE_ALL => cl_h.CL_DEVICE_TYPE_ALL);

   type Device_Info_Bool is (DEVICE_AVAILABLE);
   type Device_Info_String is (DEVICE_NAME);
   for Device_Info_Bool use (DEVICE_AVAILABLE => cl_h.CL_DEVICE_AVAILABLE);
   for Device_Info_String use (DEVICE_NAME => cl_h.CL_DEVICE_NAME);

   type Program_Build_Info_String is (PROGRAM_BUILD_LOG);
   for Program_Build_Info_String use (PROGRAM_BUILD_LOG => cl_h.CL_PROGRAM_BUILD_LOG);

   type Context_Properties is (CONTEXT_PROP_PLATFORM);
   for Context_Properties use (CONTEXT_PROP_PLATFORM => cl_h.CL_CONTEXT_PLATFORM);

   type Arch_Type is (ARCH_32, ARCH_64);
   --TODO better check
   function Get_OpenCL_Path(arch: Arch_Type) return String;

   function Init(path: String) return Status;

   function Get_Platforms(result_status: out Status) return Platforms;
   function Get_Platform_Info(id: in Platform_ID; info: Platform_Info; result_status: out Status) return String
     with Pre => id /= 0;

   function Get_Devices(id: in Platform_ID; dev_type: in Device_Type; result_status: out Status) return Devices
     with Pre => id /= 0;
   function Get_Device_Info(id: in Device_ID; info: in Device_Info_Bool; result_status: out Status) return Boolean
     with Pre => id /= 0;
   function Get_Device_Info(id: in Device_ID; info: in Device_Info_String; result_status: out Status) return String
     with Pre => id /= 0;

   function Create_Context(context_platform: in Platform_ID; context_device: in Device_ID; result_status: out Status) return Context_ID
     with Pre => context_platform /= 0 and context_device /= 0;
   function Release_Context(id: in Context_ID) return Status
     with Pre => id /= 0;

   function Create_Program(ctx: in Context_ID; source: in String; result_status: out Status) return Program_ID
     with Pre => ctx /= 0 and source'Length > 0;
   function Build_Program(id: in Program_ID; device: in Device_ID; options: in String) return Status
     with Pre => id /= 0 and device /= 0;
   function Get_Program_Build_Log(id: in Program_ID; device: in Device_ID; result_status: out Status) return String
     with Pre => id /= 0 and device /= 0;
   function Release_Program(id: in Program_ID) return Status
     with Pre => id /= 0;

   function Create_Kernel(program: in Program_ID; name: in String; result_status: out Status) return Kernel_ID
     with Pre => program /= 0 and name'Length > 0;
   function Enqueue_Kernel(queue: in Command_Queue;
                           kernel: in Kernel_ID;
                           global_offset: in Offsets;
                           global_work_size,
                           local_work_size: in Dimensions;
                           event_wait_list: in Events;
                           event: out Event_ID) return Status
     with Pre => queue /= 0 and kernel /= 0 and (global_work_size'Length = local_work_size'Length);
   function Release_Kernel(id: in Kernel_ID) return Status
     with Pre => id /= 0;
   function Set_Kernel_Arg(id: in Kernel_ID; index: Natural; size: Positive; address: System.Address) return Status
     with Pre => id /= 0;

   function Create_Command_Queue(ctx: in Context_ID; dev: in Device_ID; result_status: out Status) return Command_Queue
     with Pre => ctx /= 0 and dev /= 0;
   function Release_Command_Queue(id: in Command_Queue) return Status
     with Pre => id /= 0;

   function Wait_For_Events(ev_list: Events) return Status
     with Pre => ev_list'Length > 0;
   function Retain_Event(ev: in Event_ID) return Status
     with Pre => ev /= 0;
   function Release_Event(ev: Event_ID) return Status
     with Pre => ev /= 0;

   function Finish(queue: in Command_Queue) return Status
     with Pre => queue /= 0;

   function Create_Buffer(ctx: in Context_ID; flags: in Mem_Flags; size: Positive; host_ptr: System.Address; result_status: out Status) return Mem_ID
     with Pre => ctx /= 0;
   function Enqueue_Read(queue: in Command_Queue; mem_ob: in Mem_ID; block_read: Boolean; offset: Natural; size: Positive; ptr: System.Address; events_to_wait_for: in Events; event: out Event_ID) return Status
     with Pre => queue /= 0 and mem_ob /= 0;
   function Enqueue_Write(queue: in Command_Queue; mem_ob: in Mem_ID; block_write: Boolean; offset: Natural; size: Positive; ptr: System.Address; events_to_wait_for: in Events; event: out Event_ID) return Status
     with Pre => queue /= 0 and mem_ob /= 0;
   function Release(mem_ob: in Mem_ID) return Status
     with Pre => mem_ob /= 0;

end opencl;
