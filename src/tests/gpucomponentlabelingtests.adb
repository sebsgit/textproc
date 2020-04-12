with opencl; use opencl;
with cl_objects;
with GpuComponentLabeling;
with PixelArray.Gpu;
with PixelArray; use PixelArray;
with ImageIO;
with ShapeDatabase;
with ImageRegions; use ImageRegions;

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors; use Ada.Containers;

with Ada.Text_IO;
with Timer;

package body GpuComponentLabelingTests is

   test_image: PixelArray.ImagePlane := ImageIO.load("../training_set_test_cases/11197400569264973.1.jpg");
   test_image_gpu: PixelArray.Gpu.GpuImage_Access;
   gpu_context: cl_objects.Context_Access;
   cl_code: opencl.Status;

   procedure cleanup(T: in out Test_Cases.Test_Case'Class) is
      procedure Free_Image is new Ada.Unchecked_Deallocation(Object => PixelArray.Gpu.GpuImage,
                                                             Name   => PixelArray.Gpu.GpuImage_Access);
      procedure Free_Context is new Ada.Unchecked_Deallocation(Object => cl_objects.Context,
                                                               Name   => cl_objects.Context_Access);
   begin
      Free_Image(test_image_gpu);
      Free_Context(gpu_context);
   end cleanup;

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, initOpenCL'Access, "init opencl");
      Register_Routine (T, testCreateContext'Access, "create context");
      Register_Routine (T, testDetection'Access, "GPU region labeling");
      Register_Routine (T, cleanup'Access, "cleanup");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("GPU Component Labeling Tests");
   end Name;

   procedure initOpenCL(T: in out Test_Cases.Test_Case'Class) is
   begin
      cl_code := opencl.Init(opencl.Get_OpenCL_Path(opencl.ARCH_32));
      Assert(cl_code = opencl.SUCCESS, "init opencl failed: " & cl_code'Image);

      gpu_context := new cl_objects.Context'(cl_objects.Create_Gpu(cl_code));
      Assert(cl_code = opencl.SUCCESS, "create gpu context failed: " & cl_code'Image);

      test_image.assign(ShapeDatabase.preprocess(image => test_image));
      test_image.assign(test_image.cut(x => 0,
                                       y => 0,
                                       w => test_image.width - 1,
                                       h => test_image.height));
      test_image_gpu := new PixelArray.Gpu.GpuImage'(PixelArray.Gpu.Upload(ctx    => gpu_context.all,
                                                                           flags  => (opencl.COPY_HOST_PTR => True, others => False),
                                                                           image  => test_image,
                                                                           status => cl_code));
      Assert(cl_code = opencl.SUCCESS, "upload test image failed: " & cl_code'Image);
   end initOpenCL;

   package Region_Data_Vec is new Ada.Containers.Vectors(Index_Type   => Natural,
                                                         Element_Type => opencl.cl_uint);

   function Find_Unique_Labels(host_ccl_buffer: in GpuComponentLabeling.CCL_Data) return Region_Data_Vec.Vector is
      current_label: opencl.cl_uint := 0;
      root_x: opencl.cl_ushort := 0;
      root_y: opencl.cl_ushort := 0;
      label_found: Boolean := False;
      unique_root_labels: Region_Data_Vec.Vector;
      label_counts: Region_Data_Vec.Vector;
      package bboxvec is new Ada.Containers.Vectors(Index_Type   => Natural,
                                                    Element_Type => ImageRegions.Rect);
      bbox: bboxvec.Vector;
      current_idx: Natural := 0;

      new_x, new_y : Natural;

      function find_root(x, y: Natural) return opencl.cl_uint is
         label: opencl.cl_uint;
      begin
         label := opencl.cl_uint(x + y * test_image.width + 1);
         while label /= host_ccl_buffer(Natural(label)).label loop
            label := host_ccl_buffer(Natural(label)).label;
         end loop;
         return label;
      end find_root;

   begin
      for y in 0 .. test_image.height - 1 loop
         for x in 0 .. test_image.width - 1 loop
            current_label := host_ccl_buffer(x + y * test_image.width + 1).label;
            if current_label > 0 then
               current_label := find_root(x, y);



               label_found := False;
               current_idx := 0;
               for lbl of unique_root_labels loop
                  if lbl = current_label then
                     label_found := True;
                     exit;
                  end if;
                  current_idx := current_idx + 1;
               end loop;
               if not label_found then
                  unique_root_labels.Append(current_label);
                  label_counts.Append(1);
                  bbox.Append(ImageRegions.Rect'(x, y, 1, 1));
               else
                  label_counts(current_idx) := label_counts(current_idx) + 1;
                  new_x := Natural'Min(x, bbox(current_idx).x);
                  new_y := Natural'Min(y, bbox(current_idx).y);
                  bbox(current_idx) := ImageRegions.Rect'(new_x,
                                                          new_y,
                                                          Natural'Max(abs(x - bbox(current_idx).x), bbox(current_idx).width),
                                                          Natural'Max(abs(y - bbox(current_idx).y), bbox(current_idx).height));
               end if;
            end if;
         end loop;
      end loop;

      return unique_root_labels;
   end Find_Unique_Labels;

   procedure testCreateContext(T: in out Test_Cases.Test_Case'Class) is
      proc: GpuComponentLabeling.Processor := GpuComponentLabeling.Create(ctx     => gpu_context.all'Unchecked_Access,
                                                                          width   => test_image_gpu.Get_Width,
                                                                          height  => test_image_gpu.Get_Height,
                                                                          cl_code => cl_code);
      host_ccl_buffer: aliased GpuComponentLabeling.CCL_Data(1 .. proc.Get_Width * proc.Get_Height);
      unique_root_labels: Region_Data_Vec.Vector;
      cpu_input_image: PixelArray.ImagePlane := test_image.clone;
      cpu_regions: constant ImageRegions.RegionVector.Vector := ImageRegions.detectRegions(cpu_input_image);
   begin
      Assert(cl_code = opencl.SUCCESS, "init context failed: " & cl_code'Image);
      declare
         init_ev: cl_objects.Event := proc.Init_CCL_Data(gpu_image => test_image_gpu.all,
                                                         events_to_wait => opencl.no_events,
                                                         cl_code   => cl_code);
      begin
         Assert(cl_code = opencl.SUCCESS, "init kernel: " & cl_code'Image);
         cl_code := init_ev.Wait;
         Assert(cl_code = opencl.SUCCESS, "init wait: " & cl_code'Image);

         declare
            dwn_ev: cl_objects.Event := proc.Get_CCL_Data(host_buff => host_ccl_buffer'Address,
                                                          events_to_wait => opencl.no_events,
                                                          cl_code   => cl_code);
            curr_idx: Natural := 1;
            expected_label: opencl.cl_uint := 0;
         begin
            Assert(cl_code = opencl.SUCCESS, "downl init data: " & cl_code'Image);
            cl_code := dwn_ev.Wait;
            Assert(cl_code = opencl.SUCCESS, "downl wait: " & cl_code'Image);
            for y in 0 .. test_image.height - 1 loop
               for x in 0 .. test_image.width - 1 loop
                  curr_idx := x + y * test_image.width + 1;
                  expected_label := opencl.cl_uint(curr_idx);
                  if test_image.get(x, y) /= 0 then
                     Assert(host_ccl_buffer(curr_idx).label = 0, "wrong null label");
                  else
                     Assert(host_ccl_buffer(curr_idx).label = expected_label, "wrong label: " & host_ccl_buffer(curr_idx).label'Image & " at " & curr_idx'Image);
                  end if;
               end loop;
            end loop;
         end;

         -- vertical pass
         declare
            vpass_ev: cl_objects.Event := proc.Vertical_Pass(opencl.no_events, cl_code);
         begin
            Assert(cl_code = opencl.SUCCESS, "vpass kernel: " & cl_code'Image);
            cl_code := vpass_ev.Wait;
            Assert(cl_code = opencl.SUCCESS, "vpass wait: " & cl_code'Image);
            declare
               dwn_ev: cl_objects.Event := proc.Get_CCL_Data(host_buff => host_ccl_buffer'Address,
                                                             events_to_wait => opencl.no_events,
                                                             cl_code   => cl_code);
               prev_d, curr_d: GpuComponentLabeling.Pixel_CCL_Data;
            begin
               Assert(cl_code = opencl.SUCCESS, "downl init data: " & cl_code'Image);
               cl_code := dwn_ev.Wait;
               Assert(cl_code = opencl.SUCCESS, "downl wait: " & cl_code'Image);
               for x in 0 .. test_image.width - 1 loop
                  for y in 0 .. test_image.height - 2 loop
                     prev_d := host_ccl_buffer(x + y * test_image.width + 1);
                     curr_d := host_ccl_buffer(x + (y + 1) * test_image.width + 1);
                     if prev_d.label > 0 and curr_d.label > 0 then
                        Assert(prev_d.label = curr_d.label, "label at " & y'Image & " " & prev_d.label'Image & " " & curr_d.label'Image);
                     end if;
                  end loop;
               end loop;
            end;
         end;
         -- vpass end

         -- full merge pass
         declare
            merge_pass_ev: cl_objects.Event := proc.Merge_Pass(opencl.no_events, cl_code);
         begin
            Assert(cl_code = opencl.SUCCESS, "merge kernel: " & cl_code'Image);
            declare
               dwn_ev: cl_objects.Event := proc.Get_CCL_Data(host_buff => host_ccl_buffer'Address,
                                                             events_to_wait => opencl.no_events,
                                                             cl_code   => cl_code);
            begin
               Assert(cl_code = opencl.SUCCESS, "downl init data: " & cl_code'Image);
               cl_code := dwn_ev.Wait;
               Assert(cl_code = opencl.SUCCESS, "downl wait: " & cl_code'Image);
            end;

            unique_root_labels := Find_Unique_Labels(host_ccl_buffer);

            Ada.Text_IO.Put_Line("Gpu found " & unique_root_labels.Length'Image & " regions");
            Assert(cpu_regions.Length = unique_root_labels.Length, "cpu count /= gpu count");

         end;
         -- full merge pass end
      end;
   end testCreateContext;

   procedure testDetection(T: in out Test_Cases.Test_Case'Class) is
      proc: GpuComponentLabeling.Processor := GpuComponentLabeling.Create(ctx     => gpu_context.all'Unchecked_Access,
                                                                          width   => test_image_gpu.Get_Width,
                                                                          height  => test_image_gpu.Get_Height,
                                                                          cl_code => cl_code);
      host_ccl_buffer: aliased GpuComponentLabeling.CCL_Data(1 .. proc.Get_Width * proc.Get_Height);
      unique_root_labels: Region_Data_Vec.Vector;
      cpu_input_image: PixelArray.ImagePlane := test_image.clone;
      tmr: Timer.T := Timer.start;
      cpu_regions: constant ImageRegions.RegionVector.Vector := ImageRegions.detectRegions(cpu_input_image);
   begin
      tmr.report("CPU labeling");
      Assert(cl_code = opencl.SUCCESS, "init context failed: " & cl_code'Image);
      tmr.reset;
      declare
         ccl_ev: constant cl_objects.Event := proc.Run_CCL(gpu_image      => test_image_gpu.all,
                                                           events_to_wait => opencl.no_events,
                                                           cl_code        => cl_code);
      begin
         Assert(cl_code = opencl.SUCCESS, "ccl kernel: " & cl_code'Image);
         declare
            dwn_ev: cl_objects.Event := proc.Get_CCL_Data(host_buff => host_ccl_buffer'Address,
                                                          events_to_wait => (1 => ccl_ev.Get_Handle),
                                                          cl_code   => cl_code);
         begin
            Assert(cl_code = opencl.SUCCESS, "downl init data: " & cl_code'Image);
            cl_code := dwn_ev.Wait;
            Assert(cl_code = opencl.SUCCESS, "downl wait: " & cl_code'Image);
         end;

         tmr.report("GPU labeling");
         unique_root_labels := Find_Unique_Labels(host_ccl_buffer);

         Ada.Text_IO.Put_Line("Gpu found " & unique_root_labels.Length'Image & " regions");
         Assert(cpu_regions.Length = unique_root_labels.Length, "cpu count /= gpu count");

         for lbl of unique_root_labels loop
            for rg of host_ccl_buffer loop
               if lbl = rg.label then
                  declare
                     region_found: Boolean := False;
                  begin
                     for cpu_rg of cpu_regions loop
                        if cpu_rg.area.x = Natural(rg.min_x) and
                          cpu_rg.area.y = Natural(rg.min_y) and
                          cpu_rg.area.width = Natural(rg.max_x - rg.min_x) and
                          cpu_rg.area.height = Natural(rg.max_y - rg.min_y)
                        then
                           region_found := True;
                           exit;
                        end if;
                     end loop;
                     Assert(region_found, "GPU region not found");
                  end;
                  exit;
               end if;
            end loop;
         end loop;
      end;
   end testDetection;
end GpuComponentLabelingTests;
