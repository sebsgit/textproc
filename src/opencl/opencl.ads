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

   type Platform_Info is (PLATFORM_PROFILE, PLATFORM_VERSION, PLATFORM_NAME, PLATFORM_VENDOR, PLATFORM_EXTENSIONS);
   for Platform_Info use (PLATFORM_PROFILE => cl_h.CL_PLATFORM_PROFILE,
                          PLATFORM_VERSION => cl_h.CL_PLATFORM_VERSION,
                          PLATFORM_NAME => cl_h.CL_PLATFORM_NAME,
                          PLATFORM_VENDOR => cl_h.CL_PLATFORM_VENDOR,
                          PLATFORM_EXTENSIONS => cl_h.CL_PLATFORM_EXTENSIONS);

   function Init(path: String) return Status;
   function Get_Platforms(result_status: out Status) return Platforms;
   function Get_Platform_Info(id: in Platform_ID; info: Platform_Info; result_status: out Status) return String
     with Pre => id /= 0;

end opencl;
