with opencl; use opencl;
with cl_objects; use cl_objects;
with PixelArray.Gpu;
with Interfaces.C;
with System;
with System.Address_To_Access_Conversions;

with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body GpuImageProc is

   NL: constant Character := Ada.Characters.Latin_1.LF;

   px_sample_proc_text: constant String :=
     "uchar get_px(__global uchar *image, int w, int h, int x, int y) {" & NL &
     "   return image[x + y * w];" & NL &
     "}" & NL;

   circle_min_max_procedure_text: constant String :=
     "void circle_min_max(__global uchar *image, int w, int h, int x, int y, int radius, __global uchar *min_max)" & NL &
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

   procedure Finalize(This: in out Processor) is
      procedure Free_Queue is new Ada.Unchecked_Deallocation(Object => cl_objects.Command_Queue,
                                                             Name   => cl_objects.Command_Queue_Access);
   begin
      if This.queue /= null then
         Free_Queue(This.queue);
      end if;
   end Finalize;

   function Create_Processor(context: in out cl_objects.Context; status: out opencl.Status) return Processor is
   begin
      return proc: Processor do
         proc.queue := new cl_objects.Command_Queue'(context.Create_Command_Queue(status));
      end return;
   end Create_Processor;

   procedure Circle_Min_Max(proc: in out Processor; ctx: in out cl_objects.Context; image: in out PixelArray.Gpu.GpuImage; x, y: Natural; radius: Positive; min, max: out PixelArray.Pixel) is
      cl_code: opencl.Status;
      prog: cl_objects.Program := ctx.Create_Program(source        => px_sample_proc_text & " __kernel " & circle_min_max_procedure_text,
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
         kern: cl_objects.Kernel := prog.Create_Kernel(name          => "circle_min_max",
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
         x_p: aliased Interfaces.C.int := Interfaces.C.int(x);
         y_p: aliased Interfaces.C.int := Interfaces.C.int(y);
         img_w_p: aliased Interfaces.C.int := Interfaces.C.int(image.Get_Width);
         img_h_p: aliased Interfaces.C.int := Interfaces.C.int(image.Get_Height);
         rad_p: aliased Interfaces.C.int := Interfaces.C.int(radius);

         package Int_Addr_Conv is new System.Address_To_Access_Conversions(Interfaces.C.int);
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
         cl_code := kern.Set_Arg(1, 4, Int_Addr_Conv.To_Address(img_w_p'Access));
         cl_code := kern.Set_Arg(2, 4, Int_Addr_Conv.To_Address(img_h_p'Access));
         cl_code := kern.Set_Arg(3, 4, Int_Addr_Conv.To_Address(x_p'Access));
         cl_code := kern.Set_Arg(4, 4, Int_Addr_Conv.To_Address(y_p'Access));
         cl_code := kern.Set_Arg(5, 4, Int_Addr_Conv.To_Address(rad_p'Access));
         cl_code := kern.Set_Arg(6, opencl.Raw_Address'Size / 8, buff.Address);

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
