with cl_objects; use cl_objects;
with opencl;

with System;

package PixelArray.Gpu is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type GpuImage is tagged limited private;
   type GpuImage_Access is access all GpuImage;

   function Get_Width(img: in GpuImage) return Natural;
   function Get_Height(img: in GpuImage) return Natural;

   function Get_Address(img: in out GpuImage) return System.Address;

   function Create(ctx: in out Context'Class; flags: opencl.Mem_Flags; width, height: in Positive; status: out opencl.Status) return GpuImage;
   function Upload(ctx: in out Context'Class; flags: opencl.Mem_Flags; image: in ImagePlane; status: out opencl.Status) return GpuImage;
   function Download(queue: in out Command_Queue'Class; source: in out GpuImage; target: in out ImagePlane; status: out opencl.Status) return Event
     with Pre => source.Get_Width = target.width and source.Get_Height = target.height;
   function Download(queue: in out Command_Queue'Class; source: in out GpuImage; target: in out ImagePlane; event_to_wait: in opencl.Events; status: out opencl.Status) return Event
     with Pre => source.Get_Width = target.width and source.Get_Height = target.height;

   procedure Set_Size(source: in out GpuImage; context: in out cl_objects.Context'Class; width, height: in Positive);
   function Upload_Image(source: in out GpuImage; ctx: in out Context'Class; queue: in out Command_Queue; image: in ImagePlane) return opencl.Status;

private
   type GpuImage is tagged limited record
      data: cl_objects.Buffer;
      width, height: Natural;
      max_size: Natural;
   end record;
end PixelArray.Gpu;
