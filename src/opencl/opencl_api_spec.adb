with System; use System;

package body opencl_api_spec is
   function Load_From(h: dl_loader.Handle) return Boolean is
   begin
      clGetPlatformIDs := dl_loader.Get_Symbol(h, "clGetPlatformIDs");
      clGetPlatformInfo := dl_loader.Get_Symbol(h, "clGetPlatformInfo");
      clGetDeviceIDs := dl_loader.Get_Symbol(h, "clGetDeviceIDs");
      clGetDeviceInfo := dl_loader.Get_Symbol(h, "clGetDeviceInfo");
      clCreateContext := dl_loader.Get_Symbol(h, "clCreateContext");
      clReleaseContext := dl_loader.Get_Symbol(h, "clReleaseContext");

      clCreateProgramWithSource := dl_loader.Get_Symbol(h, "clCreateProgramWithSource");
      clBuildProgram := dl_loader.Get_Symbol(h, "clBuildProgram");
      clGetProgramBuildInfo := dl_loader.Get_Symbol(h, "clGetProgramBuildInfo");
      clReleaseProgram := dl_loader.Get_Symbol(h, "clReleaseProgram");

      clCreateKernel := dl_loader.Get_Symbol(h, "clCreateKernel");
      clReleaseKernel := dl_loader.Get_Symbol(h, "clReleaseKernel");
      clEnqueueNDRangeKernel := dl_loader.Get_Symbol(h, "clEnqueueNDRangeKernel");
      clSetKernelArg := dl_loader.Get_Symbol(h, "clSetKernelArg");

      clReleaseMemObject := dl_loader.Get_Symbol(h, "clReleaseMemObject");

      clCreateBuffer := dl_loader.Get_Symbol(h, "clCreateBuffer");
      clEnqueueReadBuffer := dl_loader.Get_Symbol(h, "clEnqueueReadBuffer");
      clEnqueueWriteBuffer := dl_loader.Get_Symbol(h, "clEnqueueWriteBuffer");

      clCreateCommandQueueWithProperties := dl_loader.Get_Symbol(h, "clCreateCommandQueueWithProperties");
      clReleaseCommandQueue := dl_loader.Get_Symbol(h, "clReleaseCommandQueue");

      clWaitForEvents := dl_loader.Get_Symbol(h, "clWaitForEvents");
      clReleaseEvent := dl_loader.Get_Symbol(h, "clReleaseEvent");

      clFinish := dl_loader.Get_Symbol(h, "clFinish");

      return clCreateBuffer /= System.Null_Address; --TODO
   end;
end opencl_api_spec;
