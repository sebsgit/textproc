with System; use System;
with cl_h;

package opencl is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Status is (INVALID_PLATFORM, INVALID_VALUE, OUT_OF_HOST_MEMORY, SUCCESS);
   for Status use (SUCCESS => cl_h.CL_SUCCESS,
                   INVALID_VALUE => cl_h.CL_INVALID_VALUE,
                   INVALID_PLATFORM => cl_h.CL_INVALID_PLATFORM,
                   OUT_OF_HOST_MEMORY => cl_h.CL_OUT_OF_HOST_MEMORY);

   type Raw_Address is mod System.Memory_Size;

   pragma Convention (Convention => C,
                      Entity     => Raw_Address);

   type Platform_ID is new Raw_Address;
   type Platforms is array (Natural range <>) of Platform_ID;

   type Device_ID is new Raw_Address;
   type Devices is array (Natural range <>) of Device_ID;

   type Context_ID is new Raw_Address;

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

   type Context_Properties is (CONTEXT_PROP_PLATFORM);
   for Context_Properties use (CONTEXT_PROP_PLATFORM => cl_h.CL_CONTEXT_PLATFORM);

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

end opencl;
