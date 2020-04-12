with opencl; use opencl;
with cl_objects; use cl_objects;
with PixelArray.Gpu;
with Interfaces.C;
with System;
with System.Address_To_Access_Conversions;
with ImageFilters;

with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body GpuImageProc is
   NL: constant Character := Ada.Characters.Latin_1.LF;

   px_sample_proc_text: constant String :=
     "uchar get_px(__global const uchar *image, int w, int h, int x, int y) {" & NL &
     "   return image[x + y * w];" & NL &
     "}" & NL &
     "void set_px(__global uchar *image, int w, int h, int x, int y, uchar px) {" & NL &
     "   image[x + y * w] = px;" & NL &
     "}";

   circle_min_max_procedure_text: constant String :=
     "void circle_min_max(__global const uchar *image, int w, int h, int x, int y, int radius, uchar *min_max)" & NL &
     "{" & NL &
     "   uchar tmp[2]; tmp[0] = 255; tmp[1] = 0;" & NL &
     "   for (int curr_x = x - radius; curr_x < x + radius; ++curr_x) {" & NL &
     "      if (curr_x < 0 || curr_x >= w) continue;" & NL &
     "      for (int curr_y = y - radius; curr_y < y + radius; ++curr_y) {" & NL &
     "         if (curr_y < 0 || curr_y >= h) continue;" & NL &
     "         if ( (x - curr_x) * (x - curr_x) + (y - curr_y) * (y - curr_y) < radius * radius ) {" & NL &
     "            const uchar px = get_px(image, w, h, curr_x, curr_y);" & NL &
     "            tmp[0] = (tmp[0] > px ? px : tmp[0]);" & NL &
     "            tmp[1] = (tmp[1] < px ? px : tmp[1]);" & NL &
     "         }" & NL &
     "      }" & NL &
     "   }" & NL &
     "   min_max[0] = tmp[0]; min_max[1] = tmp[1];" & NL &
     "}" & NL;

   patch_min_procedure_text: constant String :=
     "uchar patch_min(__global const uchar *image, int w, int h, int x, int y, int size)" & NL &
     "{" & NL &
     "   uchar result = get_px(image, w, h, x, y);" & NL &
     "   for (int yp = y - size / 2; yp <= y + size / 2 ; ++yp) {" & NL &
     "      if (yp < 0 || yp >= h) continue;" & NL &
     "      for (int xp = x - size / 2 ; xp <= x + size / 2; ++xp) {" & NL &
     "         if (xp < 0 || xp >= w) continue;" & NL &
     "         const uchar curr = get_px(image, w, h, xp, yp);" & NL &
     "         if (curr == 0) return 0;" & NL &
     "         if (curr < result) result = curr;" & NL &
     "      }" & NL &
     "   }" & NL &
     "   return result;" & NL &
     "}" & NL;

   patch_max_procedure_text: constant String :=
     "uchar patch_max(__global const uchar *image, int w, int h, int x, int y, int size)" & NL &
     "{" & NL &
     "   uchar result = get_px(image, w, h, x, y);" & NL &
     "   for (int yp = y - size / 2; yp <= y + size / 2 ; ++yp) {" & NL &
     "      if (yp < 0 || yp >= h) continue;" & NL &
     "      for (int xp = x - size / 2 ; xp <= x + size / 2; ++xp) {" & NL &
     "         if (xp < 0 || xp >= w) continue;" & NL &
     "         const uchar curr = get_px(image, w, h, xp, yp);" & NL &
     "         if (curr == 255) return 255;" & NL &
     "         if (curr > result) result = curr;" & NL &
     "      }" & NL &
     "   }" & NL &
     "   return result;" & NL &
     "}" & NL;

   bernsen_threshold_procedure_text: constant String :=
     "__kernel void bernsen_adaptative_threshold(__global const uchar *input, __global uchar *output, int w, int h, int radius, uchar c_min)" & NL &
     "{" & NL &
     "   uchar min_max[2];" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int px_y = get_global_id(1);" & NL &
     "   circle_min_max(input, w, h, px_x, px_y, radius, min_max);" & NL &
     "   const uchar threshold = (min_max[1] - min_max[0] >= c_min) ? (min_max[0] + min_max[1]) / 2 : 0;" & NL &
     "   const uchar px_out = (get_px(input, w, h, px_x, px_y) > threshold) ? 255 : 0;" & NL &
     "   set_px(output, w, h, px_x, px_y, px_out);" & NL &
     "}" & NL;

   gaussian_filter_procedure_text: constant String :=
     "__kernel void gaussian_filter(__global const uchar *input, __global uchar *output, int w, int h, int size, __global const float *gauss_kernel)" & NL &
     "{" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int px_y = get_global_id(1);" & NL &
     "   float px_sum = 0.0;" & NL &
     "   for (int x=-size; x<=size; ++x) {" & NL &
     "      for (int y=-size; y<=size; ++y) {" & NL &
     "         const int index = (x + size) + (y + size) * (2 * size + 1);" & NL &
     "         if ((px_x + x >= 0) && (px_x + x < w) && (px_y + y >= 0) && (px_y + y < h)) {" & NL &
     "            px_sum += gauss_kernel[index] * (float)(get_px(input, w, h, px_x + x, px_y + y));" & NL &
     "         } else {" & NL &
     "            px_sum += gauss_kernel[index] * (float)(get_px(input, w, h, px_x, px_y));" & NL &
     "         }" & NL &
     "      }" & NL &
     "   }" & NL &
     "   set_px(output, w, h, px_x, px_y, (px_sum > 255.0 ? 255 : (uchar)(px_sum)));" & NL &
     "}" & NL;

   erode_procedure_text: constant String :=
     "__kernel void erode(__global const uchar *input, __global uchar *output, int w, int h, int size)" & NL &
     "{" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int px_y = get_global_id(1);" & NL &
     "   set_px(output, w, h, px_x, px_y, patch_min(input, w, h, px_x, px_y, size));" & NL &
     "}" & NL;

   dilate_procedure_text: constant String :=
     "__kernel void dilate(__global const uchar *input, __global uchar *output, int w, int h, int size)" & NL &
     "{" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int px_y = get_global_id(1);" & NL &
     "   set_px(output, w, h, px_x, px_y, patch_max(input, w, h, px_x, px_y, size));" & NL &
     "}" & NL;

   combined_processing_kernel_source: constant String :=
     px_sample_proc_text & NL &
     patch_min_procedure_text & NL &
     patch_max_procedure_text & NL &
     circle_min_max_procedure_text & NL &
     gaussian_filter_procedure_text & NL &
     bernsen_threshold_procedure_text & NL &
     erode_procedure_text & NL &
     dilate_procedure_text & NL;

   procedure Finalize(This: in out Processor) is
      procedure Free_Queue is new Ada.Unchecked_Deallocation(Object => cl_objects.Command_Queue,
                                                             Name   => cl_objects.Command_Queue_Access);
      procedure Free_Program is new Ada.Unchecked_Deallocation(Object => cl_objects.Program,
                                                               Name   => cl_objects.Program_Access);
      procedure Free_Kernel is new Ada.Unchecked_Deallocation(Object => cl_objects.Kernel,
                                                              Name   => cl_objects.Kernel_Access);
   begin
      if This.queue /= null then
         Free_Queue(This.queue);
      end if;
      if This.bernsen_threshold_kernel /= null then
         Free_Kernel(This.bernsen_threshold_kernel);
      end if;
      if This.gaussian_blur_kernel /= null then
         Free_Kernel(This.gaussian_blur_kernel);
      end if;
      if This.erode_kernel /= null then
         Free_Kernel(This.erode_kernel);
      end if;
      if This.dilate_kernel /= null then
         Free_Kernel(This.dilate_kernel);
      end if;
      if This.program /= null then
         Free_Program(This.program);
      end if;
   end Finalize;

   function Create_Processor(context: in out cl_objects.Context; status: out opencl.Status) return Processor is
   begin
      return proc: Processor do
         proc.queue := new cl_objects.Command_Queue'(context.Create_Command_Queue(status));
         if status = opencl.SUCCESS then
            proc.program := new cl_objects.Program'(context.Create_Program(source        => combined_processing_kernel_source,
                                                                           result_status => status));
            if status = opencl.SUCCESS then
               status := context.Build(prog    => proc.program.all,
                                       options => "-w -Werror");
               if status = opencl.BUILD_PROGRAM_FAILURE then
                  Ada.Text_IO.Put_Line(context.Get_Build_Log(proc.program.all));
               else
                  proc.bernsen_threshold_kernel := new cl_objects.Kernel'(proc.program.all.Create_Kernel("bernsen_adaptative_threshold", status));
                  if status /= opencl.SUCCESS then
                     return;
                  end if;

                  proc.gaussian_blur_kernel := new cl_objects.Kernel'(proc.program.all.Create_Kernel("gaussian_filter", status));
                  if status /= opencl.SUCCESS then
                     return;
                  end if;

                  proc.erode_kernel := new cl_objects.Kernel'(proc.program.Create_Kernel("erode", status));
                  if status /= opencl.SUCCESS then
                     return;
                  end if;

                  proc.dilate_kernel := new cl_objects.Kernel'(proc.program.Create_Kernel("dilate", status));
               end if;
            end if;
         end if;
      end return;
   end Create_Processor;

   function Get_Command_Queue(proc: in out Processor) return cl_objects.Command_Queue_Access is
   begin
      return proc.queue;
   end Get_Command_Queue;

   function Bernsen_Adaptative_Threshold(proc: in out Processor;
                                         ctx: in out cl_objects.Context;
                                         source: in out PixelArray.Gpu.GpuImage;
                                         target: in out PixelArray.Gpu.GpuImage;
                                         radius: in Positive;
                                         c_min: in PixelArray.Pixel;
                                         cl_code: out opencl.Status) return cl_objects.Event is
      ev_data: opencl.Events(1 .. 0);
   begin
      return Bernsen_Adaptative_Threshold(proc           => proc,
                                          ctx            => ctx,
                                          source         => source,
                                          target         => target,
                                          radius         => radius,
                                          c_min          => c_min,
                                          events_to_wait => ev_data,
                                          cl_code        => cl_code);
   end Bernsen_Adaptative_Threshold;

   function Bernsen_Adaptative_Threshold(proc: in out Processor;
                                         ctx: in out cl_objects.Context;
                                         source: in out PixelArray.Gpu.GpuImage;
                                         target: in out PixelArray.Gpu.GpuImage;
                                         radius: in Positive;
                                         c_min: in PixelArray.Pixel;
                                         events_to_wait: in opencl.Events;
                                         cl_code: out opencl.Status) return cl_objects.Event is
      width_arg: aliased cl_int := cl_int(source.Get_Width);
      height_arg: aliased cl_int := cl_int(source.Get_Height);
      radius_arg: aliased cl_int := cl_int(radius);
      c_min_arg: aliased cl_uchar := cl_uchar(c_min);
   begin
      cl_code := proc.bernsen_threshold_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, source.Get_Address);
      cl_code := proc.bernsen_threshold_kernel.Set_Arg(1, opencl.Raw_Address'Size / 8, target.Get_Address);
      cl_code := proc.bernsen_threshold_kernel.Set_Arg(2, 4, width_arg'Address);
      cl_code := proc.bernsen_threshold_kernel.Set_Arg(3, 4, height_arg'Address);
      cl_code := proc.bernsen_threshold_kernel.Set_Arg(4, 4, radius_arg'Address);
      cl_code := proc.bernsen_threshold_kernel.Set_Arg(5, 1, c_min_arg'Address);
      return proc.queue.Enqueue_Kernel(kern    => proc.bernsen_threshold_kernel.all,
                                       glob_ws => (1 => source.Get_Width, 2 => source.Get_Height),
                                       loc_ws  => Get_Local_Work_Size(source.Get_Width, source.Get_Height),
                                       events_to_wait_for => events_to_wait,
                                       code    => cl_code);
   end Bernsen_Adaptative_Threshold;

   function Gaussian_Filter(proc: in out Processor;
                            ctx: in out cl_objects.Context;
                            source: in out PixelArray.Gpu.GpuImage;
                            target: in out PixelArray.Gpu.GpuImage;
                            size: in Positive;
                            sigma: in Float;
                            cl_code: out opencl.Status) return cl_objects.Event is
      pragma Warnings(Off);
      package Kernel_Arr_Address_Conv is new System.Address_To_Access_Conversions(Object => ImageFilters.Kernel);
      pragma Warnings(On);

      width_arg: aliased cl_int := cl_int(source.Get_Width);
      height_arg: aliased cl_int := cl_int(source.Get_Height);
      size_arg: aliased cl_int := cl_int(size);
      gauss_kernel: aliased ImageFilters.Kernel := ImageFilters.generateKernel(size  => size,
                                                                               sigma => sigma);
      gauss_kernel_buffer: cl_objects.Buffer := ctx.Create_Buffer(flags         => (opencl.COPY_HOST_PTR => True, others => False),
                                                                  size          => gauss_kernel'Length * 4,
                                                                  host_ptr      => Kernel_Arr_Address_Conv.To_Address(gauss_kernel'Access),
                                                                  result_status => cl_code);
   begin
      cl_code := proc.gaussian_blur_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, source.Get_Address);
      cl_code := proc.gaussian_blur_kernel.Set_Arg(1, opencl.Raw_Address'Size / 8, target.Get_Address);
      cl_code := proc.gaussian_blur_kernel.Set_Arg(2, 4, width_arg'Address);
      cl_code := proc.gaussian_blur_kernel.Set_Arg(3, 4, height_arg'Address);
      cl_code := proc.gaussian_blur_kernel.Set_Arg(4, 4, size_arg'Address);
      cl_code := proc.gaussian_blur_kernel.Set_Arg(5, opencl.Raw_Address'Size / 8, gauss_kernel_buffer.Get_Address);
      return proc.queue.Enqueue_Kernel(kern    => proc.gaussian_blur_kernel.all,
                                       glob_ws => (1 => source.Get_Width, 2 => source.Get_Height),
                                       loc_ws  => Get_Local_Work_Size(source.Get_Width, source.Get_Height),
                                       code    => cl_code);
   end Gaussian_Filter;

   function Erode(proc: in out Processor;
                  ctx: in out cl_objects.Context;
                  source: in out PixelArray.Gpu.GpuImage;
                  target: in out PixelArray.Gpu.GpuImage;
                  size: in Positive;
                  events_to_wait: in opencl.Events;
                  cl_code: out opencl.Status) return cl_objects.Event is
      width_arg: aliased cl_int := cl_int(source.Get_Width);
      height_arg: aliased cl_int := cl_int(source.Get_Height);
      size_arg: aliased cl_int := cl_int(size);
   begin
      cl_code := proc.erode_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, source.Get_Address);
      cl_code := proc.erode_kernel.Set_Arg(1, opencl.Raw_Address'Size / 8, target.Get_Address);
      cl_code := proc.erode_kernel.Set_Arg(2, 4, width_arg'Address);
      cl_code := proc.erode_kernel.Set_Arg(3, 4, height_arg'Address);
      cl_code := proc.erode_kernel.Set_Arg(4, 4, size_arg'Address);
      return proc.queue.Enqueue_Kernel(kern    => proc.erode_kernel.all,
                                       glob_ws => (1 => source.Get_Width, 2 => source.Get_Height),
                                       loc_ws  => Get_Local_Work_Size(source.Get_Width, source.Get_Height),
                                       events_to_wait_for => events_to_wait,
                                       code    => cl_code);
   end Erode;

   function Dilate(proc: in out Processor;
                   ctx: in out cl_objects.Context;
                   source: in out PixelArray.Gpu.GpuImage;
                   target: in out PixelArray.Gpu.GpuImage;
                   size: in Positive;
                   events_to_wait: in opencl.Events;
                   cl_code: out opencl.Status) return cl_objects.Event is
      width_arg: aliased cl_int := cl_int(source.Get_Width);
      height_arg: aliased cl_int := cl_int(source.Get_Height);
      size_arg: aliased cl_int := cl_int(size);
   begin
      cl_code := proc.dilate_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, source.Get_Address);
      cl_code := proc.dilate_kernel.Set_Arg(1, opencl.Raw_Address'Size / 8, target.Get_Address);
      cl_code := proc.dilate_kernel.Set_Arg(2, 4, width_arg'Address);
      cl_code := proc.dilate_kernel.Set_Arg(3, 4, height_arg'Address);
      cl_code := proc.dilate_kernel.Set_Arg(4, 4, size_arg'Address);
      return proc.queue.Enqueue_Kernel(kern    => proc.dilate_kernel.all,
                                       glob_ws => (1 => source.Get_Width, 2 => source.Get_Height),
                                       loc_ws  => Get_Local_Work_Size(source.Get_Width, source.Get_Height),
                                       events_to_wait_for => events_to_wait,
                                       code    => cl_code);
   end Dilate;

   procedure Circle_Min_Max(proc: in out Processor; ctx: in out cl_objects.Context; image: in out PixelArray.Gpu.GpuImage; x, y: Natural; radius: Positive; min, max: out PixelArray.Pixel) is
      cl_code: opencl.Status;
      wrapped_min_max_kernel: constant String :=
        "__kernel void circle_min_max_wrap(__global uchar *image, int w, int h, int x, int y, int radius, __global uchar *min_max)" & NL &
        "{" & NL &
        "   uchar min_max_loc[2];" & NL &
        "   circle_min_max(image, w, h, x, y, radius, min_max_loc);" & NL &
        "   min_max[0] = min_max_loc[0]; min_max[1] = min_max_loc[1];" & NL &
        "}" & NL;
      prog: cl_objects.Program := ctx.Create_Program(source        => px_sample_proc_text & circle_min_max_procedure_text & wrapped_min_max_kernel,
                                                     result_status => cl_code);
   begin
      min := 0;
      max := 0;
      if cl_code /= opencl.SUCCESS then
         Ada.Text_IO.Put_Line("Processor::Circle_Min_Max error: " & cl_code'Image);
         return;
      end if;

      cl_code := ctx.Build(prog    => prog,
                           options => "-w -Werror");

      if cl_code /= opencl.SUCCESS then
         Ada.Text_IO.Put_Line("Processor::Circle_Min_Max build error: " & cl_code'Image & ", log: " & ctx.Get_Build_Log(prog));
         return;
      end if;

      declare
         kern: cl_objects.Kernel := prog.Create_Kernel(name          => "circle_min_max_wrap",
                                                       result_status => cl_code);
         buffer_status: opencl.Status;

         type Min_Max_Buff is array (Positive range <>) of Interfaces.C.unsigned_char;
         pragma Warnings(Off);
         package Min_Max_Buff_Addr_Conv is new System.Address_To_Access_Conversions(Min_Max_Buff);
         pragma Warnings(On);
         host_buff: aliased Min_Max_Buff := (1 => 0, 2 => 0);

         buff: cl_objects.Buffer := ctx.Create_Buffer(flags         => (opencl.ALLOC_HOST_PTR => True, others => False),
                                                      size          => 2 * 8,
                                                      host_ptr      => System.Null_Address,
                                                      result_status => buffer_status);
         x_p: aliased cl_int := cl_int(x);
         y_p: aliased cl_int := cl_int(y);
         img_w_p: aliased cl_int := cl_int(image.Get_Width);
         img_h_p: aliased cl_int := cl_int(image.Get_Height);
         rad_p: aliased cl_int := cl_int(radius);
      begin
         if cl_code /= opencl.SUCCESS then
            Ada.Text_IO.Put_Line("Processor::Circle_Min_Max kernel create: " & cl_code'Image);
            return;
         end if;

         if buffer_status /= opencl.SUCCESS then
            Ada.Text_IO.Put_Line("Processor::Circle_Min_Max buffer create: " & buffer_status'Image);
            return;
         end if;

         cl_code := kern.Set_Arg(0, opencl.Raw_Address'Size / 8, image.Get_Address);
         cl_code := kern.Set_Arg(1, 4, img_w_p'Address);
         cl_code := kern.Set_Arg(2, 4, img_h_p'Address);
         cl_code := kern.Set_Arg(3, 4, x_p'Address);
         cl_code := kern.Set_Arg(4, 4, y_p'Address);
         cl_code := kern.Set_Arg(5, 4, rad_p'Address);
         cl_code := kern.Set_Arg(6, opencl.Raw_Address'Size / 8, buff.Get_Address);

         declare
            run_ev: constant cl_objects.Event := proc.queue.Enqueue_Kernel(kern               => kern,
                                                                           glob_ws            => (1 => 1),
                                                                           loc_ws             => (1 => 1),
                                                                           code               => cl_code);
         begin
            if cl_code /= opencl.SUCCESS then
               Ada.Text_IO.Put_Line("Processor::Circle_Min_Max enq kernel: " & cl_code'Image);
               return;
            end if;

            declare
               downl_ev: cl_objects.Event := cl_objects.Enqueue_Read(queue => proc.queue.all,
                                                                     mem_ob             => buff,
                                                                     offset             => 0,
                                                                     size               => 2,
                                                                     ptr                => Min_Max_Buff_Addr_Conv.To_Address(host_buff'Access),
                                                                     events_to_wait_for => (1 => run_ev.Get_Handle),
                                                                     code               => cl_code);
            begin
               if cl_code /= opencl.SUCCESS then
                  Ada.Text_IO.Put_Line("Processor::Circle_Min_Max read: " & cl_code'Image);
                  return;
               end if;

               cl_code := downl_ev.Wait;
               if cl_code /= opencl.SUCCESS then
                  Ada.Text_IO.Put_Line("Processor::Circle_Min_Max wait for read: " & cl_code'Image);
                  return;
               end if;
            end;

         end;

         min := PixelArray.Pixel(host_buff(1));
         max := PixelArray.Pixel(host_buff(2));
      end;
   end Circle_Min_Max;

end GpuImageProc;
