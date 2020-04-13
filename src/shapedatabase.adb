with PixelArray;
with ImageIO;
with ImageRegions;
with ImageMoments;
with ImageFilters;
with ImageThresholds;
with Morphology;
with Histogram;
with HistogramDescriptor;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

use Ada.Containers;

with GpuImageProc; use GpuImageProc;
with cl_objects;
with opencl; use opencl;
with PixelArray.Gpu;
with GpuComponentLabeling; use GpuComponentLabeling;

package body ShapeDatabase is

   Gauss_Filter_Kernel_Size: constant Positive := 7;
   Gauss_Filter_Sigma: constant Float := 2.4;
   Bernsen_Threshold_Circle_Radius: constant Positive := 10;
   Bernsen_Threshold_Contrast_Limit: constant PixelArray.Pixel := 35;
   Erode_Kernel_Size: constant Positive := 7;
   Dilate_Kernel_Size: constant Positive := 7;

   gpu_context: cl_objects.Context_Access;
   gpu_queue: cl_objects.Command_Queue_Access;
   gpu_processor: GpuImageProc.Processor_Access;
   gpu_detector: GpuComponentLabeling.Processor_Access;
   gpu_source_cache: PixelArray.Gpu.GpuImage_Access;
   gpu_target_cache: PixelArray.Gpu.GpuImage_Access;

   procedure Find_Gpu_Device(platf: out Platform_ID; dev: out Device_ID) is
      cl_code: opencl.Status;
      platform_ids: constant opencl.Platforms := opencl.Get_Platforms(cl_code);
   begin
      if cl_code = opencl.SUCCESS then
         for p_id of platform_ids loop
            declare
               device_ids: constant opencl.Devices := opencl.Get_Devices(id            => p_id,
                                                                         dev_type      => opencl.DEVICE_TYPE_GPU,
                                                                         result_status => cl_code);
            begin
               if cl_code = opencl.SUCCESS and device_ids'Length > 0 then
                  platf := p_id;
                  dev := device_ids(1);
                  exit;
               end if;
            end;
         end loop;
      end if;
   end Find_Gpu_Device;

   procedure Init_Gpu is
      cl_status: opencl.Status := opencl.Init(opencl.Get_OpenCL_Path(opencl.ARCH_32));
      platf_id: opencl.Platform_ID := 0;
      dev_id: opencl.Device_ID := 0;
   begin
      if cl_status /= opencl.SUCCESS then
         Ada.Text_IO.Put_Line("Cannot init OpenCL: " & cl_status'Image);
         return;
      end if;

      Find_Gpu_Device(platf_id, dev_id);
      if dev_id = 0 then
         Ada.Text_IO.Put_Line("Cannot find GPU device");
         return;
      end if;

      gpu_context := new cl_objects.Context'(cl_objects.Create(context_platform => platf_id,
                                                               context_device   => dev_id,
                                                               result_status    => cl_status));
      if cl_status /= opencl.SUCCESS then
         Ada.Text_IO.Put_Line("Cannot init GPU context: " & cl_status'Image);
         return;
      end if;

      gpu_queue := new cl_objects.Command_Queue'(gpu_context.Create_Command_Queue(cl_status));
      if cl_status /= opencl.SUCCESS then
         Ada.Text_IO.Put_Line("Cannot init GPU queue: " & cl_status'Image);
         return;
      end if;

      gpu_processor := new GpuImageProc.Processor'(GpuImageProc.Create_Processor(context => gpu_context.all,
                                                                                 status  => cl_status));

      if cl_status /= opencl.SUCCESS then
         Ada.Text_IO.Put_Line("Cannot init GPU processor: " & cl_status'Image);
         gpu_processor := null;
         return;
      end if;

      gpu_detector := new GpuComponentLabeling.Processor'(GpuComponentLabeling.Create(ctx     => gpu_context,
                                                                                      width   => 256,
                                                                                      height  => 256,
                                                                                      cl_code => cl_status));
      if cl_status /= opencl.SUCCESS then
         Ada.Text_IO.Put_Line("Cannot init GPU detector: " & cl_status'Image);
         gpu_detector := null;
         return;
      end if;

      gpu_source_cache := new PixelArray.Gpu.GpuImage'(PixelArray.Gpu.Create(ctx    => gpu_context.all,
                                                                             flags  => (others => False),
                                                                             width  => 256,
                                                                             height => 256,
                                                                             status => cl_status));
      gpu_target_cache := new PixelArray.Gpu.GpuImage'(PixelArray.Gpu.Create(ctx    => gpu_context.all,
                                                                             flags  => (others => False),
                                                                             width  => 256,
                                                                             height => 256,
                                                                             status => cl_status));
   end Init_Gpu;

   function Ensure_Cache_Size(gpu_image: in out PixelArray.Gpu.GpuImage; context: in out cl_objects.Context; width, height: in Positive) return Boolean is
   begin
      if gpu_image.Get_Width /= width or gpu_image.Get_Height /= height then
         gpu_image.Set_Size(context => context,
                            width  => width,
                            height => height);
      end if;
      return True;
   end Ensure_Cache_Size;

   function Ensure_Cache_Size(gpu_image: in out PixelArray.Gpu.GpuImage; context: in out cl_objects.Context; image: in PixelArray.ImagePlane) return Boolean is
      cl_code: constant opencl.Status := gpu_image.Upload_Image(context, gpu_queue.all, image);
   begin
      return (cl_code = opencl.SUCCESS);
   end Ensure_Cache_Size;

   procedure Detect_Image_Regions(image: in out PixelArray.ImagePlane; res: out ImageRegions.RegionVector.Vector) is
      cl_code: opencl.Status;
   begin
      if gpu_detector /= null then
         res := gpu_detector.Detect_Regions_And_Assign_Labels(preprocessed_cpu_image => image,
                                                              cl_code                => cl_code);
      else
         res := ImageRegions.detectRegions(image);
      end if;
   end Detect_Image_Regions;

   function Preprocess_Gpu(gpuSource: in out PixelArray.Gpu.GpuImage; cl_code: out opencl.Status) return cl_objects.Event is
      cache_status: Boolean := Ensure_Cache_Size(gpu_target_cache.all, gpu_context.all, gpuSource.Get_Width, gpuSource.Get_Height);
      gauss_proc_event: constant cl_objects.Event := gpu_processor.Gaussian_Filter(ctx     => gpu_context.all,
                                                                                   source  => gpuSource,
                                                                                   target  => gpu_target_cache.all,
                                                                                   size    => Gauss_Filter_Kernel_Size,
                                                                                   sigma   => Gauss_Filter_Sigma,
                                                                                   cl_code => cl_code);
      proc_event: constant cl_objects.Event := gpu_processor.Bernsen_Adaptative_Threshold(ctx     => gpu_context.all,
                                                                                          source  => gpu_target_cache.all,
                                                                                          target  => gpuSource,
                                                                                          radius  => Bernsen_Threshold_Circle_Radius,
                                                                                          c_min   => Bernsen_Threshold_Contrast_Limit,
                                                                                          events_to_wait => (1 => gauss_proc_event.Get_Handle),
                                                                                          cl_code => cl_code);
      erode_event: constant cl_objects.Event := gpu_processor.Erode(ctx            => gpu_context.all,
                                                                    source         => gpuSource,
                                                                    target         => gpu_target_cache.all,
                                                                    size           => Erode_Kernel_Size,
                                                                    events_to_wait => (1 => proc_event.Get_Handle),
                                                                    cl_code        => cl_code);
   begin
      return gpu_processor.Dilate(ctx            => gpu_context.all,
                                  source         => gpu_target_cache.all,
                                  target         => gpuSource,
                                  size           => Dilate_Kernel_Size,
                                  events_to_wait => (1 => erode_event.Get_Handle),
                                  cl_code        => cl_code);
   end Preprocess_Gpu;

   function Preprocess_And_Detect_Regions(image: in PixelArray.ImagePlane; res: out ImageRegions.RegionVector.Vector) return PixelArray.ImagePlane is
   begin
      if gpu_detector = null or gpu_processor = null then
         return processed: PixelArray.ImagePlane := preprocess(image) do
            Detect_Image_Regions(processed, res);
         end return;
      else
         return result: PixelArray.ImagePlane := PixelArray.allocate(width  => image.width,
                                                                     height => image.height) do
            declare
               cl_code: opencl.Status;
               cache_status: Boolean := Ensure_Cache_Size(gpu_source_cache.all, gpu_context.all, image);
               preproc_event: constant cl_objects.Event := Preprocess_Gpu(gpuSource => gpu_source_cache.all,
                                                                          cl_code   => cl_code);
            begin
               res := gpu_detector.Detect_Regions_And_Assign_Labels(preprocessed_gpu_image => gpu_source_cache.all,
                                                                    target_cpu_image       => result,
                                                                    events_to_wait         => (1 => preproc_event.Get_Handle),
                                                                    cl_code                => cl_code);
            end;
         end return;
      end if;
   end Preprocess_And_Detect_Regions;

   function preprocess(image: PixelArray.ImagePlane) return PixelArray.ImagePlane is
      fallback_to_cpu: Boolean := True;
   begin
      --TODO implement the whole preprocessing pipeline in opencl
      return result: PixelArray.ImagePlane := PixelArray.allocate(width  => image.width,
                                                                  height => image.height) do
         -- threshold adaptative
         if gpu_processor /= null then
            declare
               cl_code: opencl.Status;
               cache_status: Boolean := Ensure_Cache_Size(gpu_source_cache.all, gpu_context.all, image);
               preproc_event: constant cl_objects.Event := Preprocess_Gpu(gpuSource => gpu_source_cache.all,
                                                                          cl_code   => cl_code);
               downl_ev: cl_objects.Event := PixelArray.Gpu.Download(gpu_processor.Get_Command_Queue.all, gpu_source_cache.all, result, (1 => preproc_event.Get_Handle), cl_code);
            begin
               cl_code := downl_ev.Wait;
               if cl_code = opencl.SUCCESS then
                  fallback_to_cpu := False;
               end if;
            end;
         end if;

         if fallback_to_cpu then
            Ada.Text_IO.Put_Line("GPU download failed, fallback to CPU");
            result.assign(ImageFilters.gaussian(image, Gauss_Filter_Kernel_Size, Gauss_Filter_Sigma));
            result.assign(ImageThresholds.bernsenAdaptative(result,
                          radius => Bernsen_Threshold_Circle_Radius,
                          c_min  => Bernsen_Threshold_Contrast_Limit));
            -- apply morphology to strenghten shapes
            result.assign(Morphology.erode(result, Erode_Kernel_Size));
            result.assign(Morphology.dilate(result, Dilate_Kernel_Size));
         end if;
      end return;
   end preprocess;

   function processRegion(image: PixelArray.ImagePlane; rect: ImageRegions.Region) return Descriptor is
      result: Descriptor;
   begin
      result.moments := ImageMoments.calculateMoments(image, rect);
      result.orientation := ImageMoments.orientationAngle(image, rect);
      result.histogram := HistogramDescriptor.create(image  => image,
                                                     region => rect.area);
      return result;
   end processRegion;

   function loadShape(cc: Character; path: String) return CharacterDescriptor is
      result: CharacterDescriptor;
      regions: ImageRegions.RegionVector.Vector;
      image: constant PixelArray.ImagePlane := Preprocess_And_Detect_Regions(ImageIO.load(path), regions);
   begin
      result.c := cc;
      result.d := processRegion(image, regions.First_Element);

      return result;
   end loadShape;

   procedure add(database: in out DB; desc: CharacterDescriptor) is
   begin
      database.shapes.Append(desc);
   end add;

   function getDB return DB is
   begin
      return staticDB;
   end getDB;

   function match(database: DB; image: PixelArray.ImagePlane; region: ImageRegions.Region) return MatchScore is
      result: MatchScore;
      tmp: Character;
      tmpScore: Float;
      regionDescriptor: ImageMoments.HuMoments;
      d1, d2, d3, d4: Float;
   begin
      result.cc := '-';
      result.score := 99999.0;
      regionDescriptor := ImageMoments.calculateMoments(image, region);
      tmpScore := ImageMoments.orientationAngle(image, region);
      -- Ada.Text_IO.Put_Line("orient: " & tmpScore'Image);
      for i in 0 .. Integer(database.shapes.Length - 1) loop
         d1 := ImageMoments.match_I1(regionDescriptor, database.shapes.Element(i).d.moments);
         d2 := ImageMoments.match_I2(regionDescriptor, database.shapes.Element(i).d.moments);
         d3 := ImageMoments.match_I3(regionDescriptor, database.shapes.Element(i).d.moments);
         d4 := (d1 + d3) / 2.0;
         tmpScore := d4;
         tmp := database.shapes.Element(i).c;

         --  Ada.Text_IO.Put_Line("match with " & tmp'Image & " => " & tmpScore'Image);
         if tmpScore < result.score then
            result.score := tmpScore;
            result.cc := tmp;
         end if;
      end loop;
      return result;
   end match;

   function getCharacterString(imagePath: String) return Ada.Strings.Unbounded.Unbounded_String is
      firstDotPosition: Natural := 0;
      lastSlashPosition: Natural := 0;
      startIndex: Natural := 1;
      result: Ada.Strings.Unbounded.Unbounded_String;
   begin
      lastSlashPosition := Ada.Strings.Fixed.Index(imagePath, "/", Ada.Strings.Backward);
      if lastSlashPosition /= 0 then
         startIndex := lastSlashPosition + 1;
      end if;
      firstDotPosition := Ada.Strings.Fixed.Index(imagePath, ".", startIndex);
      result := Ada.Strings.Unbounded.To_Unbounded_String(imagePath);
      result := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Strings.Unbounded.Slice(result, startIndex, firstDotPosition - 1));
      return result;
   end getCharacterString;

   function loadShapes(imagePath: String) return ShapeVector.Vector is
      result: ShapeVector.Vector;
      r: ImageRegions.RegionVector.Vector;
      image: constant PixelArray.ImagePlane := Preprocess_And_Detect_Regions(ImageIO.load(imagePath), r);
      basePath: constant Ada.Strings.Unbounded.Unbounded_String := getCharacterString(imagePath);
   begin
      if Integer(r.Length) /= Ada.Strings.Unbounded.Length(basePath) then
         raise Capacity_Error with imagePath & " --> " & Ada.Strings.Unbounded.To_String(basePath) & ": processing error";
      end if;
      ImageRegions.sortRegions(r);
      for i in 0 .. r.Length - 1 loop
         declare
            desc: CharacterDescriptor;
         begin
            desc.c := imagePath(Integer(i + 1));
            desc.d := processRegion(image, r(Integer(i)));
            result.Append(desc);
         end;
      end loop;
      return result;
   end loadShapes;

   procedure insertShapes(result: out DB; imagePath: String) is
      shapes: ShapeVector.Vector;
   begin
      shapes := loadShapes(imagePath);
      for d in 0 .. shapes.Length - 1 loop
         result.add(shapes(Integer(d)));
      end loop;
   end insertShapes;

   function init return DB is
      result: DB;
   begin
      result.add(loadShape('0', "0.jpg"));
      result.add(loadShape('1', "1.jpg"));
      result.add(loadShape('1', "1_1.jpg"));
      result.add(loadShape('2', "2.jpg"));
      result.add(loadShape('2', "2_.jpg"));
      result.add(loadShape('3', "3.jpg"));
      result.add(loadShape('4', "4.jpg"));

      result.add(loadShape('7', "7.jpg"));
      result.add(loadShape('8', "8.jpg"));
      result.add(loadShape('9', "9.jpg"));

      insertShapes(result, "20180501.1.jpg");

      return result;
   end init;
begin
   Init_Gpu;
   staticDB := init;
end ShapeDatabase;
