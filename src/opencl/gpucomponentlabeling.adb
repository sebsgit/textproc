with cl_objects;
with opencl; use opencl;

with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Elementary_Functions;
with System;
with Ada.Text_IO;

package body GpuComponentLabeling is
   NL: constant Character := Ada.Characters.Latin_1.LF;

   common_declarations_program_source: constant String :=
     "typedef struct {" & NL &
     "   uint label;" & NL &
     "   int min_x, min_y;" & NL &
     "   int max_x, max_y;" & NL &
     "} ccl_data;" & NL &
     "" & NL &
     "uint find_root_by_label(__global const ccl_data *data, uint label) {" & NL &
     "   uint root = label;" & NL &
     "   while (root != data[root - 1].label) {" & NL &
     "      root = data[root - 1].label;" & NL &
     "   }" & NL &
     "   return root;" & NL &
     "}" & NL;

   init_kernel_source: constant String :=
     "__kernel void init(__global uchar * restrict image, int width, int height, __global ccl_data * restrict output) {" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int px_y = get_global_id(1);" & NL &
     "   const int px_i = px_x + width * px_y;" & NL &
     "   output[px_i].label = (image[px_i] == 0) ? px_i + 1 : 0;" & NL &
     "   output[px_i].min_x = px_x;" & NL &
     "   output[px_i].min_y = px_y;" & NL &
     "   output[px_i].max_x = px_x;" & NL &
     "   output[px_i].max_y = px_y;" & NL &
     "}" & NL;

   vpass_kernel_source: constant String :=
     "__kernel void vertical_pass(__global ccl_data *data, int width, int height) {" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   ccl_data prev = data[px_x];" & NL &
     "   for (int r = 1; r < height ; ++r) {" & NL &
     "      ccl_data curr = data[px_x + r * width];" & NL &
     "      if (prev.label > 0 && curr.label > 0) {" & NL &
     "         const int root_idx = find_root_by_label(data, prev.label) - 1;" & NL &
     "         atomic_min(&data[root_idx].min_x, px_x);" & NL &
     "         atomic_min(&data[root_idx].min_y, r - 1);" & NL &
     "         atomic_max(&data[root_idx].max_x, px_x);" & NL &
     "         atomic_max(&data[root_idx].max_y, r);" & NL &
     "         curr.label = prev.label;" & NL &
     "         data[px_x + r * width].label = prev.label;" & NL &
     "      }" & NL &
     "      prev.label = curr.label;" & NL &
     "   }" & NL &
     "}" & NL;

   merge_pass_source: constant String :=
     "__kernel void merge_pass(__global ccl_data *data, int width, int height, int w_div) {" & NL &
     "   const int px_x = get_global_id(0);" & NL &
     "   const int col_id = w_div * px_x + w_div / 2 - 1;" & NL &
     "   const int next_col = col_id + 1;" & NL &
     "   if (next_col < width) {" & NL &
     "      const int px_y = get_global_id(1);" & NL &
     "      const uint left = data[col_id + px_y * width].label;" & NL &
     "      const uint right = data[next_col + px_y * width].label;" & NL &
     "      if (left > 0 && right > 0) {" & NL &
     "         const uint root_left = find_root_by_label(data, left);" & NL &
     "         const uint root_right = find_root_by_label(data, right);" & NL &
     "         const int root_idx = min(root_left, root_right) - 1;" & NL &
     "         const ccl_data old_root = data[max(root_left, root_right) - 1];" & NL &
     "         data[max(root_left, root_right) - 1].label = data[root_idx].label;" & NL &
     "         atomic_min(&data[root_idx].min_x, min(col_id, old_root.min_x));" & NL &
     "         atomic_min(&data[root_idx].min_y, min(px_y, old_root.min_y));" & NL &
     "         atomic_max(&data[root_idx].max_x, max(next_col, old_root.max_x));" & NL &
     "         atomic_max(&data[root_idx].max_y, max(px_y, old_root.max_y));" & NL &
     "      }" & NL &
     "   }" & NL &
     "}" & NL;

   processing_program_source: constant String :=
     common_declarations_program_source & NL &
     vpass_kernel_source & NL &
     merge_pass_source & NL &
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
         res.max_ccl_size := res.width * res.height;

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
                              options => "-w -Werror -cl-fast-relaxed-math -cl-strict-aliasing -cl-mad-enable");
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

   function Prepare_Size(proc: in out Processor; w, h: in Natural) return opencl.Status is
      cl_code: opencl.Status := opencl.SUCCESS;
   begin
      if w * h > Natural(proc.max_ccl_size) then
         cl_objects.Free(proc.ccl_data);
         proc.ccl_data := new cl_objects.Buffer'(proc.context.Create_Buffer(flags         => (opencl.READ_WRITE => True, opencl.ALLOC_HOST_PTR => True, others => false),
                                                                            size          => w * h * Pixel_CCL_Data'Size / 8,
                                                                            host_ptr      => System.Null_Address,
                                                                            result_status => cl_code));
         proc.max_ccl_size := opencl.cl_int(w * h);
      end if;
      proc.width := opencl.cl_int(w);
      proc.height := opencl.cl_int(h);
      return cl_code;
   end Prepare_Size;

   function Init_CCL_Data(proc: in out Processor; gpu_image: in out PixelArray.Gpu.GpuImage; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
   begin
      cl_code := proc.Prepare_Size(gpu_image.Get_Width, gpu_image.Get_Height);
      if cl_code /= opencl.SUCCESS then
         return cl_objects.Create_Empty;
      end if;
      cl_code := proc.initialization_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, gpu_image.Get_Address);
      cl_code := proc.initialization_kernel.Set_Arg(1, 4, proc.width'Address);
      cl_code := proc.initialization_kernel.Set_Arg(2, 4, proc.height'Address);
      cl_code := proc.initialization_kernel.Set_Arg(3, opencl.Raw_Address'Size / 8, proc.ccl_data.Get_Address);
      return proc.gpu_queue.Enqueue_Kernel(kern               => proc.initialization_kernel.all,
                                           glob_ws            => (1 => proc.Get_Width, 2 => proc.Get_Height),
                                           loc_ws             => opencl.Get_Local_Work_Size(proc.Get_Width, proc.Get_Height),
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
                                           loc_ws             => opencl.Get_Local_Work_Size(proc.Get_Width, 1),
                                           events_to_wait_for => events_to_wait,
                                           code               => cl_code);
   end Vertical_Pass;

   function Merge_Pass(proc: in out Processor; width_div: in Positive; events_to_wait: in opencl.Events; cl_code: out opencl.Status) return cl_objects.Event is
      width_div_arg: aliased opencl.cl_int := opencl.cl_int(width_div);
      kernel_size_x: constant Natural := Natural(Float'Ceiling(Float(proc.Get_Width) / Float(width_div)));
   begin
      cl_code := proc.merge_pass_kernel.Set_Arg(0, opencl.Raw_Address'Size / 8, proc.ccl_data.Get_Address);
      cl_code := proc.merge_pass_kernel.Set_Arg(1, 4, proc.width'Address);
      cl_code := proc.merge_pass_kernel.Set_Arg(2, 4, proc.height'Address);
      cl_code := proc.merge_pass_kernel.Set_Arg(3, 4, width_div_arg'Address);
      return proc.gpu_queue.Enqueue_Kernel(kern               => proc.merge_pass_kernel.all,
                                           glob_ws            => (1 => kernel_size_x, 2 => proc.Get_Height),
                                           loc_ws             => opencl.Get_Local_Work_Size(kernel_size_x, proc.Get_Height),
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

   package Region_Data_Vec is new Ada.Containers.Vectors(Index_Type   => Natural,
                                                         Element_Type => opencl.cl_uint);

   function Find_Unique_Labels(host_ccl_buffer: in GpuComponentLabeling.CCL_Data) return Region_Data_Vec.Vector is
      current_idx: opencl.cl_uint := 0;
      unique_root_labels: Region_Data_Vec.Vector;
   begin
      for lbl of host_ccl_buffer loop
         current_idx := current_idx + 1;
         if lbl.label > 0 and then lbl.label = current_idx then
            unique_root_labels.Append(lbl.label);
         end if;
      end loop;
      return unique_root_labels;
   end Find_Unique_Labels;

   function Detect_Regions(proc: in out Processor; preprocessed_cpu_image: in PixelArray.ImagePlane; cl_code: out opencl.Status) return ImageRegions.RegionVector.Vector is
      result: ImageRegions.RegionVector.Vector;
      host_ccl_data: aliased CCL_Data(1 .. preprocessed_cpu_image.width * preprocessed_cpu_image.height);
      gpu_image: PixelArray.Gpu.GpuImage := PixelArray.Gpu.Upload(ctx    => proc.context.all,
                                                                  flags  => (opencl.COPY_HOST_PTR => True, others => False),
                                                                  image  => preprocessed_cpu_image,
                                                                  status => cl_code);
      ccl_ev: constant cl_objects.Event := proc.Run_CCL(gpu_image      => gpu_image,
                                                        events_to_wait => opencl.no_events,
                                                        cl_code        => cl_code);
      down_ev: cl_objects.Event := proc.Get_CCL_Data(host_buff      => host_ccl_data'Address,
                                                     events_to_wait => (1 => ccl_ev.Get_Handle),
                                                     cl_code        => cl_code);
   begin
      cl_code := down_ev.Wait;
      if cl_code = opencl.SUCCESS then
         declare
            use ImageRegions;

            new_rg: ImageRegions.Region;
            label: ImageRegions.RegionLabel := 1;
            unique_labels: constant Region_Data_Vec.Vector := Find_Unique_Labels(host_ccl_data);
            gpu_rg: Pixel_CCL_Data;
         begin
            for lbl of unique_labels loop
               gpu_rg := host_ccl_data(Natural(lbl));
               new_rg.label := label;
               new_rg.area.x := Natural(gpu_rg.min_x);
               new_rg.area.y := Natural(gpu_rg.min_y);
               new_rg.area.width := Natural(gpu_rg.max_x - gpu_rg.min_x);
               new_rg.area.height := Natural(gpu_rg.max_y - gpu_rg.min_y);

               --TODO not really
               new_rg.pixelCount := new_rg.area.width * new_rg.area.height;
               new_rg.center.x := Float(new_rg.area.x + new_rg.area.width / 2);
               new_rg.center.y := Float(new_rg.area.y + new_rg.area.height / 2);

               result.Append(new_rg);
               label := label + 1;
            end loop;
         end;
      end if;
      return result;
   end Detect_Regions;

   function Detect_Regions_And_Assign_Labels(proc: in out Processor; preprocessed_cpu_image: in out PixelArray.ImagePlane; cl_code: out opencl.Status) return ImageRegions.RegionVector.Vector is
      result: ImageRegions.RegionVector.Vector;
      host_ccl_data: aliased CCL_Data(1 .. preprocessed_cpu_image.width * preprocessed_cpu_image.height);
      gpu_image: PixelArray.Gpu.GpuImage := PixelArray.Gpu.Upload(ctx    => proc.context.all,
                                                                  flags  => (opencl.COPY_HOST_PTR => True, others => False),
                                                                  image  => preprocessed_cpu_image,
                                                                  status => cl_code);
      ccl_ev: constant cl_objects.Event := proc.Run_CCL(gpu_image      => gpu_image,
                                                        events_to_wait => opencl.no_events,
                                                        cl_code        => cl_code);
      down_ev: cl_objects.Event := proc.Get_CCL_Data(host_buff      => host_ccl_data'Address,
                                                     events_to_wait => (1 => ccl_ev.Get_Handle),
                                                     cl_code        => cl_code);
   begin
      cl_code := down_ev.Wait;
      if cl_code = opencl.SUCCESS then
         declare
            use ImageRegions;

            new_rg: ImageRegions.Region;
            label: ImageRegions.RegionLabel := 1;
            reg_lbl: Natural := 1;
            unique_labels: constant Region_Data_Vec.Vector := Find_Unique_Labels(host_ccl_data);
            gpu_rg: Pixel_CCL_Data;
         begin
            for lbl of unique_labels loop
               gpu_rg := host_ccl_data(Natural(lbl));
               new_rg.label := label;
               new_rg.area.x := Natural(gpu_rg.min_x);
               new_rg.area.y := Natural(gpu_rg.min_y);
               new_rg.area.width := Natural(gpu_rg.max_x - gpu_rg.min_x);
               new_rg.area.height := Natural(gpu_rg.max_y - gpu_rg.min_y);

               new_rg.pixelCount := 0;
               for rg of host_ccl_data loop
                  if rg.label = lbl then
                     new_rg.pixelCount := new_rg.pixelCount + 1;
                  end if;
               end loop;

               new_rg.center.x := Float(new_rg.area.x + new_rg.area.width / 2);
               new_rg.center.y := Float(new_rg.area.y + new_rg.area.height / 2);

               if new_rg.pixelCount > 10 then
                  result.Append(new_rg);
                  label := label + 1;
               end if;
            end loop;

            reg_lbl := 0;
            for rg of host_ccl_data loop
               reg_lbl := reg_lbl + 1;
               if rg.label /= 0 then
                  label := 1;
                  for d of unique_labels loop
                     if Natural(d) = Natural(rg.label) then
                        exit;
                     end if;
                     label := label + 1;
                  end loop;

                  declare
                     px_x, px_y: Natural := 0;
                  begin
                     px_x := Natural(reg_lbl - 1) mod preprocessed_cpu_image.width;
                     px_y := Natural(reg_lbl - 1) / preprocessed_cpu_image.width;
                     preprocessed_cpu_image.set(px_x, px_y, PixelArray.Pixel(label));
                  end;
               end if;
            end loop;
         end;
      end if;
      return result;
   end Detect_Regions_And_Assign_Labels;

end GpuComponentLabeling;
