with cl_objects;
with opencl; use opencl;

with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Numerics.Generic_Elementary_Functions;
with System;
with Ada.Text_IO;

package body GpuComponentLabeling is
   NL: constant Character := Ada.Characters.Latin_1.LF;

   common_declarations_program_source: constant String :=
     "typedef struct {" & NL &
     "   uint label;" & NL &
     "} ccl_data;" & NL &
     "" & NL;

   init_kernel_source: constant String :=
     "__kernel void init(__global uchar *image, int width, int height, __global ccl_data *output) {" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int px_y = get_global_id(1);" & NL &
     "   const int px_i = px_x + width * px_y;" & NL &
     "   output[px_i].label = (image[px_i] == 0) ? px_i + 1 : 0;" & NL &
     "}" & NL;

   vpass_kernel_source: constant String :=
     "__kernel void vertical_pass(__global ccl_data *data, int width, int height) {" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   ccl_data prev = data[px_x];" & NL &
     "   for (int r = 1; r < height ; ++r) {" & NL &
     "      ccl_data curr = data[px_x + r * width];" & NL &
     "      if (prev.label > 0 && curr.label > 0) {" & NL &
     "         curr.label = prev.label;" & NL &
     "      }" & NL &
     "      data[px_x + r * width] = curr;" & NL &
     "      prev = curr;" & NL &
     "   }" & NL &
     "}" & NL;

   merge_pass_source: constant String :=
     "uint find_root(__global ccl_data *data, int width, int height, int x, int y) {" & NL &
     "   uint root = width * y + x + 1;" & NL &
     "   while (root != data[root - 1].label) {" & NL &
     "      root = data[root - 1].label;" & NL &
     "   }" & NL &
     "   return root;" & NL &
     "}" & NL &
     "__kernel void merge_pass(__global ccl_data *data, int width, int height, int w_div) {" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int col_id = w_div * px_x + w_div / 2 - 1;" & NL &
     "   const int r = get_global_id(1);" & NL &
     "   const int next_col = col_id + 1;" & NL &
     "   if (next_col < width) {" & NL &
   -- "      for (int r = 0; r < height ; ++r) {" & NL &
     "         ccl_data left = data[col_id + r * width];" & NL &
     "         ccl_data right = data[next_col + r * width];" & NL &
     "         if (left.label > 0 && right.label > 0) {" & NL &
     "            const uint root_left = find_root(data, width, height, col_id, r);" & NL &
     "            const uint root_right = find_root(data, width, height, next_col, r);" & NL &
     "            data[max(root_left, root_right) - 1] = data[min(root_left, root_right) - 1];" & NL &
     "         }" & NL &
   -- "      }" & NL &
     "   }" & NL &
     "}" & NL;

   label_pass_source: constant String :=
     "__kernel void label_pass() {" & NL &
     "}" & NL;

   processing_program_source: constant String :=
     common_declarations_program_source & NL &
     vpass_kernel_source & NL &
     merge_pass_source & NL &
     label_pass_source & NL &
     init_kernel_source & NL;

   function Get_Width(proc: in Processor) return Natural is
   begin
      return Natural(proc.width);
   end Get_Width;

   function Get_Height(proc: in Processor) return Natural is
   begin
      return Natural(proc.height);
   end Get_Height;

   function Create(ctx: cl_objects.Context_Access; width, height: in Positive; cl_code: out opencl.Status) return Processor is
   begin
      return res: Processor do
         res.context := ctx;
         res.width := opencl.cl_int(width);
         res.height := opencl.cl_int(height);

         res.gpu_queue := new cl_objects.Command_Queue'(ctx.Create_Command_Queue(cl_code));
         if cl_code /= opencl.SUCCESS then return; end if;

         res.ccl_data := new cl_objects.Buffer'(ctx.Create_Buffer(flags         => (opencl.READ_WRITE => True, opencl.ALLOC_HOST_PTR => True, others => false),
                                                                  size          => width * height * Pixel_CCL_Data'Size / 8,
                                                                  host_ptr      => System.Null_Address,
                                                                  result_status => cl_code));
         if cl_code /= opencl.SUCCESS then return; end if;

         res.processing_program := new cl_objects.Program'(ctx.Create_Program(source        => processing_program_source,
                                                                              result_status => cl_code));
         if cl_code /= opencl.SUCCESS then return; end if;

         cl_code := ctx.Build(prog    => res.processing_program.all,
                              options => "-w -Werror");
         if cl_code /= opencl.SUCCESS then
            if cl_code = opencl.BUILD_PROGRAM_FAILURE then
               Ada.Text_IO.Put_Line(ctx.Get_Build_Log(res.processing_program.all));
            end if;
            return;
         end if;

         res.initialization_kernel := new cl_objects.Kernel'(res.processing_program.Create_Kernel("init", cl_code));
         if cl_code /= opencl.SUCCESS then return; end if;

         res.vertical_pass_kernel := new cl_objects.Kernel'(res.processing_program.Create_Kernel("vertical_pass", cl_code));
         if cl_code /= opencl.SUCCESS then return; end if;

         res.merge_pass_kernel := new cl_objects.Kernel'(res.processing_program.Create_Kernel("merge_pass", cl_code));
      end return;
   end Create;

   procedure Finalize(This: in out Processor) is
      use cl_objects;
   begin
      Free(This.gpu_queue);
      Free(This.ccl_data);
      Free(This.initialization_kernel);
      Free(This.vertical_pass_kernel);
      Free(This.merge_pass_kernel);
      Free(This.processing_program);
   end Finalize;

   function Init_CCL_Data(proc: in out Processor; gpu_image: in out PixelArray.Gpu.GpuImage; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
   begin
      --TODO input events
      cl_code := proc.initialization_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, gpu_image.Get_Address);
      cl_code := proc.initialization_kernel.Set_Arg(1, 4, proc.width'Address);
      cl_code := proc.initialization_kernel.Set_Arg(2, 4, proc.height'Address);
      cl_code := proc.initialization_kernel.Set_Arg(3, opencl.Raw_Address'Size / 8, proc.ccl_data.Get_Address);
      return proc.gpu_queue.Enqueue_Kernel(kern               => proc.initialization_kernel.all,
                                           glob_ws            => (1 => proc.Get_Width, 2 => proc.Get_Height),
                                           loc_ws             => (1, 1), --TODO
                                           events_to_wait_for => events_to_wait,
                                           code               => cl_code);
   end Init_CCL_Data;

   function Vertical_Pass(proc: in out Processor; events_to_wait: opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
   begin
      cl_code := proc.vertical_pass_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, proc.ccl_data.Get_Address);
      cl_code := proc.vertical_pass_kernel.Set_Arg(1, 4, proc.width'Address);
      cl_code := proc.vertical_pass_kernel.Set_Arg(2, 4, proc.height'Address);
      return proc.gpu_queue.Enqueue_Kernel(kern               => proc.vertical_pass_kernel.all,
                                           glob_ws            => (1 => proc.Get_Width, 2 => 1),
                                           loc_ws             => (1, 1), --TODO
                                           events_to_wait_for => events_to_wait,
                                           code               => cl_code);
   end Vertical_Pass;

   function Merge_Pass(proc: in out Processor; width_div: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
      width_div_arg: aliased opencl.cl_int := opencl.cl_int(width_div);
   begin
      cl_code := proc.merge_pass_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, proc.ccl_data.Get_Address);
      cl_code := proc.merge_pass_kernel.Set_Arg(1, 4, proc.width'Address);
      cl_code := proc.merge_pass_kernel.Set_Arg(2, 4, proc.height'Address);
      cl_code := proc.merge_pass_kernel.Set_Arg(3, 4, width_div_arg'Address);
      return proc.gpu_queue.Enqueue_Kernel(kern               => proc.merge_pass_kernel.all,
                                           glob_ws            => (1 => Natural(Float'Ceiling(Float(proc.Get_Width) / Float(width_div))), 2 => proc.Get_Height),
                                           loc_ws             => (1, 1), --TODO
                                           events_to_wait_for => events_to_wait,
                                           code               => cl_code);
   end Merge_Pass;

   function Merge_Pass(proc: in out Processor; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
      package Math_Fnc is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => Float);

      width_div: Positive := 2;
      idx: Float := 0.0;
      loop_limit: constant Float := Math_Fnc.Log(Float(proc.Get_Width), 2.0);
      previous_event: opencl.Events(1 .. 1);
      is_first_step: Boolean := True;
   begin
      previous_event(1) := 0;
      while idx < loop_limit loop
         idx := idx + 1.0;
         declare
            is_last_step: constant Boolean := not (idx < loop_limit);
            ev: constant cl_objects.Event := proc.Merge_Pass(width_div => width_div,
                                                             events_to_wait => (if is_first_step then events_to_wait else previous_event),
                                                             cl_code    => cl_code);
         begin
            if cl_code /= opencl.SUCCESS then
               exit;
            end if;
            if previous_event(1) /= 0 then
               cl_code := opencl.Release_Event(previous_event(1));
            end if;
            if is_last_step then
               return cl_objects.Create_Event(ev.Get_Handle);
            else
               cl_code := opencl.Retain_Event(ev.Get_Handle);
               previous_event(1) := ev.Get_Handle;
            end if;
         end;
         width_div := width_div * 2;
         is_first_step := False;
      end loop;
      return cl_objects.Create_Empty;
   end Merge_Pass;

   function Run_CCL(proc: in out Processor; gpu_image: in out PixelArray.Gpu.GpuImage; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
      init_ev: constant cl_objects.Event := proc.Init_CCL_Data(gpu_image => gpu_image,
                                                               events_to_wait => events_to_wait,
                                                               cl_code   => cl_code);
   begin
      if cl_code = opencl.SUCCESS then
         declare
            vpass_ev: constant cl_objects.Event := proc.Vertical_Pass((1 => init_ev.Get_Handle), cl_code);
         begin
            if cl_code = opencl.SUCCESS then
               return proc.Merge_Pass((1 => vpass_ev.Get_Handle), cl_code);
            end if;
         end;
      end if;
      return cl_objects.Create_Empty;
   end Run_CCL;

   function Get_CCL_Data(proc: in out Processor; host_buff: in System.Address; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
   begin
      return proc.gpu_queue.Enqueue_Read(mem_ob             => proc.ccl_data.all,
                                         offset             => 0,
                                         size               => proc.Get_Width * proc.Get_Height * Pixel_CCL_Data'Size / 8,
                                         ptr                => host_buff,
                                         events_to_wait_for => events_to_wait,
                                         code               => cl_code);
   end Get_CCL_Data;

end GpuComponentLabeling;
