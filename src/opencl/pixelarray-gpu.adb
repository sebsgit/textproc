with cl_objects;
with opencl;
with System;
with System.Address_To_Access_Conversions;

package body PixelArray.Gpu is
   pragma Warnings(Off);
   package  Pixel_Buffer_Conv is new System.Address_To_Access_Conversions(Object => Pixel_Buffer);
   pragma Warnings(On);

   function Get_Width(img: in GpuImage) return Natural is
   begin
      return img.width;
   end Get_Width;

   function Get_Height(img: in GpuImage) return Natural is
   begin
      return img.height;
   end Get_Height;

   function Get_Address(img: in out GpuImage) return System.Address is
   begin
      return img.data.Get_Address;
   end Get_Address;

   function Upload(ctx: in out Context'Class; flags: opencl.Mem_Flags; image: in ImagePlane; status: out opencl.Status) return GpuImage is
      final_flags: opencl.Mem_Flags := flags;
   begin
      final_flags(opencl.COPY_HOST_PTR) := True;
      final_flags(opencl.ALLOC_HOST_PTR) := False;
      return res: constant GpuImage := (data => ctx.Create_Buffer(flags         => final_flags,
                                                                  size          => image.width * image.height,
                                                                  host_ptr      => image.data(0)'Address,
                                                                  result_status => status),
                                        width => image.width,
                                        height => image.height) do
         null;
      end return;
   end Upload;

   function Download(queue: in out Command_Queue'Class; source: in out GpuImage; target: in out ImagePlane; status: out opencl.Status) return Event is
      ev_data: opencl.Events(1 .. 0);
   begin
      return Download(queue         => queue,
                      source        => source,
                      target        => target,
                      event_to_wait => ev_data,
                      status        => status);
   end Download;
   function Download(queue: in out Command_Queue'Class; source: in out GpuImage; target: in out ImagePlane; event_to_wait: in opencl.Events; status: out opencl.Status) return Event is
   begin
      return queue.Enqueue_Read(mem_ob             => source.data,
                                offset             => 0,
                                size               => source.width * source.height,
                                ptr                => Pixel_Buffer_Conv.To_Address(Pixel_Buffer_Conv.Object_Pointer(target.data)),
                                events_to_wait_for => event_to_wait,
                                code               => status);
   end Download;

end PixelArray.Gpu;
