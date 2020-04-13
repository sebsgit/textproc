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

   function Create(ctx: in out Context'Class; flags: opencl.Mem_Flags; width, height: in Positive; status: out opencl.Status) return GpuImage is
      final_flags: opencl.Mem_Flags := flags;
   begin
      final_flags(opencl.COPY_HOST_PTR) := False;
      final_flags(opencl.ALLOC_HOST_PTR) := True;
      return res: constant GpuImage := (data => ctx.Create_Buffer(flags         => final_flags,
                                                                  size          => width * height,
                                                                  host_ptr      => System.Null_Address,
                                                                  result_status => status),
                                        width => width,
                                        height => height,
                                        max_size => width * height) do
         null;
      end return;
   end Create;

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
                                        height => image.height,
                                        max_size => image.width * image.height) do
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
                                ptr                => target.data(0)'Address,
                                events_to_wait_for => event_to_wait,
                                code               => status);
   end Download;

   procedure Set_Size(source: in out GpuImage; context: in out cl_objects.Context'Class; width, height: in Positive) is
      cl_code: opencl.Status;
   begin
      if width /= source.Get_Width or height /= source.Get_Height then
         if width * height > source.max_size then
            cl_code := opencl.Release(source.data.Get_ID);
            source.data.Set_ID(opencl.Create_Buffer(ctx           => context.Get_ID,
                                                    flags         => (opencl.ALLOC_HOST_PTR => True, others => False),
                                                    size          => width * height,
                                                    host_ptr      => System.Null_Address,
                                                    result_status => cl_code));
            source.max_size := width * height;
         end if;
         source.width := width;
         source.height := height;
      end if;
   end Set_Size;

   function Upload_Image(source: in out GpuImage; ctx: in out Context'Class; queue: in out Command_Queue; image: in ImagePlane) return opencl.Status is
      cl_code: opencl.Status := opencl.SUCCESS;
      do_upload: Boolean := True;
   begin
      if image.width /= source.Get_Width or image.height /= source.Get_Height then
         if image.width * image.height > source.max_size then
            cl_code := opencl.Release(source.data.Get_ID);
            source.data.Set_ID(opencl.Create_Buffer(ctx           => ctx.Get_ID,
                                                    flags         => (opencl.COPY_HOST_PTR => True, others => False),
                                                    size          => image.width * image.height,
                                                    host_ptr      => image.data(0)'Address,
                                                    result_status => cl_code));
            do_upload := False;
            source.max_size := image.width * image.height;
         end if;
         source.width := image.width;
         source.height := image.height;
      end if;

      if do_upload then
         declare
            ev: cl_objects.Event := queue.Enqueue_Write(mem_ob             => source.data,
                                                        offset             => 0,
                                                        size               => image.width * image.height,
                                                        ptr                => image.data(0)'Address,
                                                        events_to_wait_for => opencl.no_events,
                                                        code               => cl_code);
         begin
            cl_code := ev.Wait;
         end;
      end if;
      return cl_code;
   end Upload_Image;

end PixelArray.Gpu;
