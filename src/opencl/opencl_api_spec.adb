with System;

package body opencl_api_spec is
   function Load_From(h: dl_loader.Handle) return Boolean is
   begin
      clGetPlatformIDs := dl_loader.Get_Symbol(h, "clGetPlatformIDs");
      clGetPlatformInfo := dl_loader.Get_Symbol(h, "clGetPlatformInfo");
      clGetDeviceIDs := dl_loader.Get_Symbol(h, "clGetDeviceIDs");
      clGetDeviceInfo := dl_loader.Get_Symbol(h, "clGetDeviceInfo");
      clCreateContext := dl_loader.Get_Symbol(h, "clCreateContext");
      clReleaseContext := dl_loader.Get_Symbol(h, "clReleaseContext");

      return True;
   end;
end opencl_api_spec;
