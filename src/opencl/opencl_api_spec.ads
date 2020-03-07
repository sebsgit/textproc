with System;
with Interfaces.C;

with dl_loader;

package opencl_api_spec is
   function Load_From(h: dl_loader.Handle) return Boolean;

   clGetPlatformIDs: System.Address;
   clGetPlatformInfo: System.Address;
   clGetDeviceIDs: System.Address;
   clGetDeviceInfo: System.Address;
   clCreateContext: System.Address;
   clReleaseContext: System.Address;

end opencl_api_spec;
