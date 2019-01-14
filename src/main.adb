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

      function regionBefore(i1, i2: Natural) return Boolean is
      begin
         if regions(i1).area.x = regions(i2).area.x then
            return regions(i1).area.y < regions(i2).area.y;
         end if;
         return regions(i1).area.x < regions(i2).area.x;
      end regionBefore;

      procedure regionSwap(i1, i2: Natural) is
         tmp: ImageRegions.Region;
      begin
         tmp := regions(i1);
         regions(i1) := regions(i2);
         regions(i2) := tmp;
      end regionSwap;

      procedure regionSort is new Ada.Containers.Generic_Sort(Index_Type => Natural,
                                                              Before     => regionBefore,
                                                              Swap => regionSwap);
   begin
      MainTestSuite.runAll;

      testImage := ImageIO.load(C.Strings.New_String(Ada.Command_Line.Argument(1)));
      testImage := ImageFilters.gaussian(testImage, 7, 2.4);

      saveOk := ImageIO.save(C.Strings.New_String("Input_filtered.png"), testImage);

      -- threshold adaptative
      testImage := ImageThresholds.bernsenAdaptative(testImage,
                                                     radius => 10,
                                                     c_min  => 35);
      saveOk := ImageIO.save(C.Strings.New_String("Input_threshold.png"), testImage);

      -- apply morphology to strenghten shapes
      testImage := Morphology.erode(testImage, 7);
      testImage := Morphology.dilate(testImage, 7);
      saveOk := ImageIO.save(C.Strings.New_String("Input_morphology.png"), testImage);

      -- detect regions
      regions := ImageRegions.detectRegions(testImage);

      regionSort(0, Integer(regions.Length - 1));


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

   end;

end Main;
