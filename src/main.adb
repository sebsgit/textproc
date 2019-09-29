with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;

with ImageIO;
with PixelArray;
with ImageRegions;
with ImageMoments;
with ImageThresholds;
with Morphology;
with ImageFilters;
with Histogram;
with HistogramGenerator;
with ShapeDatabase;

with MainTestSuite;

procedure Main is
   package C renames Interfaces.C;
begin
   declare
      testImage: PixelArray.ImagePlane;
      saveOk: Boolean;
      regions: ImageRegions.RegionVector.Vector;
      finalResult: Ada.Strings.Unbounded.Unbounded_String;

      database: ShapeDatabase.DB;

   begin
      MainTestSuite.runAll;

      testImage := ImageIO.load(Ada.Command_Line.Argument(1));
      testImage := ShapeDatabase.preprocess(image => testImage);
      saveOk := ImageIO.save("Input_morphology.png", testImage);

      -- detect regions
      regions := ImageRegions.detectRegions(testImage);

      ImageRegions.sortRegions(regions);

      for it in 0 .. Integer(regions.Length - 1) loop
         Ada.Text_IO.Put_Line("-- histograms for region: " & it'Image);
         declare
            h0: Histogram.Data := HistogramGenerator.verticalProjection(testImage, regions.Element(it).area);
            h1: Histogram.Data := HistogramGenerator.horizontalProjection(testImage, regions.Element(it).area);
            h0_res: Histogram.Data(20);
            h1_res: Histogram.Data(20);

            h0_best: Float := 1000.0;
            h1_best: Float := 1000.0;
            h0_id: Integer := 0;
            h1_id: Integer := 0;
         begin
            h0_res := h0.resized(h0_res.size);
            h1_res := h1.resized(h1_res.size);
            h0_res.normalize;
            h1_res.normalize;
            for j in 0 .. Integer(regions.Length - 1) loop
               if j /= it then
                  declare
                     h0_j: Histogram.Data := HistogramGenerator.verticalProjection(testImage, regions.Element(j).area);
                     h1_j: Histogram.Data := HistogramGenerator.horizontalProjection(testImage, regions.Element(j).area);
                     h0_j_res: Histogram.Data(h0_res.size);
                     h1_j_res: Histogram.Data(h1_res.size);
                     method: Histogram.CompareMethod := Histogram.Bhattacharyya;
                     dd1, dd2: Float;
                  begin
                     h0_j_res := h0_j.resized(h0_res.size);
                     h1_j_res := h1_j.resized(h1_res.size);
                     dd1 := h0_res.compare(h0_j_res.normalized, method);
                     dd2 := h1_res.compare(h1_j_res.normalized, method);
                     if dd1 < h0_best then
                        h0_best := dd1;
                        h0_id := j;
                     end if;
                     if dd2 < h1_best then
                        h1_best := dd2;
                        h1_id := j;
                     end if;
                  end;
               end if;
            end loop;
            Ada.Text_IO.Put_Line(it'Image & " matches best: " & h0_id'Image & ", " & h1_id'Image & " -> " & h0_best'Image & ", " & h1_best'Image);
         end;
      end loop;

   end;

end Main;
