with System;
with Interfaces.C;

with dl_loader;

package opencl_api_spec is
   pragma Elaborate_Body;
   function Load_From(h: dl_loader.Handle) return Boolean;

   clGetPlatformIDs: System.Address;
   clGetPlatformInfo: System.Address;
   clGetDeviceIDs: System.Address;
   clGetDeviceInfo: System.Address;
   clCreateContext: System.Address;
   clReleaseContext: System.Address;

   clCreateProgramWithSource: System.Address;
   clBuildProgram: System.Address;
   clGetProgramBuildInfo: System.Address;
   clReleaseProgram: System.Address;

   clCreateKernel: System.Address;
   clReleaseKernel: System.Address;
   clSetKernelArg: System.Address;
   clEnqueueNDRangeKernel: System.Address;

   clReleaseMemObject: System.Address;

   clCreateBuffer: System.Address;
   clEnqueueReadBuffer: System.Address;
   clEnqueueWriteBuffer: System.Address;

   clCreateCommandQueueWithProperties: System.Address;
   clReleaseCommandQueue: System.Address;

   clWaitForEvents: System.Address;
   clRetainEvent: System.Address;
   clReleaseEvent: System.Address;

   clFinish: System.Address;

end opencl_api_spec;
