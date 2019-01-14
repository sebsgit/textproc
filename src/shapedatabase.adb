with PixelArray;
with ImageIO;
with ImageRegions;
with ImageMoments;
with ImageThresholds;
with Morphology;

with Ada.Text_IO;

with Interfaces.C.Strings;
with Ada.Containers.Vectors;

use Ada.Containers;

package body ShapeDatabase is
   function loadShape(cc: Character; path: Interfaces.C.Strings.chars_ptr) return CharacterDescriptor is
      result: CharacterDescriptor;
      image: PixelArray.ImagePlane;
      regions: ImageRegions.RegionVector.Vector;
   begin
      result.c := cc;

      image := ImageIO.load(path);
      -- threshold adaptative
      image := ImageThresholds.bernsenAdaptative(image,
                                                 radius => 10,
                                                 c_min  => 35);
      -- apply morphology to strenghten shapes
      image := Morphology.erode(image, 7);
      image := Morphology.dilate(image, 7);

      -- detect regions
      regions := ImageRegions.detectRegions(image);

      result.d.moments := ImageMoments.calculateMoments(image, regions.First_Element);
      result.d.orientation := ImageMoments.orientationAngle(image, regions.First_Element);

      return result;
   end loadShape;

   function init return DB is
      result: DB;
   begin
      result.shapes.Append(loadShape('0', Interfaces.C.Strings.New_String("0.jpg")));
      result.shapes.Append(loadShape('1', Interfaces.C.Strings.New_String("1.jpg")));
      result.shapes.Append(loadShape('1', Interfaces.C.Strings.New_String("1_1.jpg")));
      result.shapes.Append(loadShape('2', Interfaces.C.Strings.New_String("2.jpg")));
      result.shapes.Append(loadShape('2', Interfaces.C.Strings.New_String("2_.jpg")));
      result.shapes.Append(loadShape('3', Interfaces.C.Strings.New_String("3.jpg")));
      result.shapes.Append(loadShape('4', Interfaces.C.Strings.New_String("4.jpg")));

      result.shapes.Append(loadShape('7', Interfaces.C.Strings.New_String("7.jpg")));
      result.shapes.Append(loadShape('8', Interfaces.C.Strings.New_String("8.jpg")));
      result.shapes.Append(loadShape('9', Interfaces.C.Strings.New_String("9.jpg")));
      return result;
   end init;

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
      Ada.Text_IO.Put_Line("orient: " & tmpScore'Image);
      for i in 0 .. Integer(database.shapes.Length - 1) loop
         d1 := ImageMoments.match_I1(regionDescriptor, database.shapes.Element(i).d.moments);
         d2 := ImageMoments.match_I2(regionDescriptor, database.shapes.Element(i).d.moments);
         d3 := ImageMoments.match_I3(regionDescriptor, database.shapes.Element(i).d.moments);
         d4 := (d1 + d3) / 2.0;
         tmpScore := d4;
         tmp := database.shapes.Element(i).c;

         Ada.Text_IO.Put_Line("match with " & tmp'Image & " => " & tmpScore'Image);
         if tmpScore < result.score then
            result.score := tmpScore;
            result.cc := tmp;
         end if;
      end loop;
      return result;
   end match;

end ShapeDatabase;
