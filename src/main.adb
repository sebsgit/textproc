with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers.Generic_Sort;
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

      procedure sortImageRegions(input: in out ImageRegions.RegionVector.Vector) is
         function regionBefore(i1, i2: Natural) return Boolean is
         begin
            if input(i1).area.x = input(i2).area.x then
               return input(i1).area.y < input(i2).area.y;
            end if;
            return input(i1).area.x < input(i2).area.x;
         end regionBefore;

         procedure regionSwap(i1, i2: Natural) is
            tmp: ImageRegions.Region;
         begin
            tmp := input(i1);
            input(i1) := input(i2);
            input(i2) := tmp;
         end regionSwap;

         procedure doTheSorting is new Ada.Containers.Generic_Sort(Index_Type => Natural,
                                                                   Before     => regionBefore,
                                                                   Swap => regionSwap);
      begin
         doTheSorting(0, Integer(input.Length - 1));
      end sortImageRegions;

   begin
      MainTestSuite.runAll;
      --return ;

      testImage := ImageIO.load(Ada.Command_Line.Argument(1));
      testImage := ImageFilters.gaussian(testImage, 7, 2.4);

      saveOk := ImageIO.save("Input_filtered.png", testImage);

      -- threshold adaptative
      testImage := ImageThresholds.bernsenAdaptative(testImage,
                                                     radius => 10,
                                                     c_min  => 35);
      saveOk := ImageIO.save("Input_threshold.png", testImage);

      -- apply morphology to strenghten shapes
      testImage := Morphology.erode(testImage, 7);
      testImage := Morphology.dilate(testImage, 7);
      saveOk := ImageIO.save("Input_morphology.png", testImage);

      -- detect regions
      regions := ImageRegions.detectRegions(testImage);

      sortImageRegions(regions);


      -- create unit test => should be 11 labels
      for r of regions loop
         Ada.Text_IO.Put_Line("region: " & r.label'Image & " => bounds: " & r.area.toString & ", pixels: " & r.pixelCount'Image & ", center: " & r.center.x'Image & "," & r.center.y'Image);
         declare
            moments: ImageMoments.HuMoments;
         begin
            moments := ImageMoments.calculateMoments(testImage, r);
            Ada.Text_IO.Put_Line("moments => h1: " & moments.h1'Image & ", h2: " & moments.h2'Image & ", h3: " & moments.h3'Image & ", h4: " & moments.h4'Image & ", h5: " & moments.h5'Image & ", h6: " & moments.h6'Image & ", h7: " & moments.h7'Image);
            Ada.Text_IO.Put_Line("orientation => " & ImageMoments.orientationAngle(testImage, r)'Image);
         end;
      end loop;

      for it in 0 .. Integer(regions.Length - 1) loop
         Ada.Text_IO.Put_Line("-- region match -- " & it'Image);
         declare
            moments: ImageMoments.HuMoments;
         begin
            moments := ImageMoments.calculateMoments(testImage, regions.Element(it));
            for it2 in 0 .. Integer(regions.Length - 1) loop
               Ada.Text_IO.Put("-- match with -- " & it2'Image);
               declare
                  moments2: ImageMoments.HuMoments;
                  d1, d2, d3, d4: Float;
               begin
                  moments2 := ImageMoments.calculateMoments(testImage, regions.Element(it2));
                  d1 := ImageMoments.match_I1(moments, moments2);
                  d2 := ImageMoments.match_I2(moments, moments2);
                  d3 := ImageMoments.match_I3(moments, moments2);
                  d4 := (d1 + d3) / 2.0;
                  Ada.Text_IO.Put_Line(", matches => d1: " & d1'Image & ", d2: " & d2'Image & ", d3: " & d3'Image & ", d4: " & d4'Image);
               end;
            end loop;
         end;
      end loop;

      Ada.Text_IO.Put_Line("total regions: " & regions.Length'Image);

      database := ShapeDatabase.init;

      Ada.Text_IO.Put_Line("-- region match with DB -- ");
      for it in 0 .. Integer(regions.Length - 1) loop
         declare
            score: ShapeDatabase.MatchScore;
         begin
            score := database.match(testImage, regions.Element(it));
            Ada.Text_IO.Put_Line(score.cc'Image & " (" & score.score'Image & ")");
            finalResult := finalResult & score.cc;
         end;
      end loop;
      Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(finalResult));

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
