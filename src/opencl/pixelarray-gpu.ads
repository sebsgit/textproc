with cl_objects; use cl_objects;
with opencl;

package PixelArray.Gpu is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type GpuImage is tagged limited private;

   function Get_Width(img: in GpuImage) return Natural;
   function Get_Height(img: in GpuImage) return Natural;

   function Upload(ctx: in out Context'Class; flags: opencl.Mem_Flags; image: in out ImagePlane; status: out opencl.Status) return GpuImage;
   function Download(queue: in out Command_Queue'Class; source: in out GpuImage; target: in out ImagePlane; status: out opencl.Status) return Event
     with Pre => source.Get_Width = target.width and source.Get_Height = target.height;

private
   type GpuImage is tagged limited record
      data: cl_objects.Buffer;
      width, height: Natural;
   end record;
end PixelArray.Gpu;
