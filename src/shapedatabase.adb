with PixelArray;
with ImageIO;
with ImageRegions;
with ImageMoments;
with ImageFilters;
with ImageThresholds;
with Morphology;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

use Ada.Containers;

package body ShapeDatabase is

   function preprocess(image: PixelArray.ImagePlane) return PixelArray.ImagePlane is
      result: PixelArray.ImagePlane;
   begin
      result := ImageFilters.gaussian(image, 7, 2.4);
      -- threshold adaptative
      result := ImageThresholds.bernsenAdaptative(result,
                                                  radius => 10,
                                                  c_min  => 35);
      -- apply morphology to strenghten shapes
      result := Morphology.erode(result, 7);
      result := Morphology.dilate(result, 7);
      return result;
   end preprocess;

   function processRegion(image: PixelArray.ImagePlane; rect: ImageRegions.Region) return Descriptor is
      result: Descriptor;
   begin
      result.moments := ImageMoments.calculateMoments(image, rect);
      result.orientation := ImageMoments.orientationAngle(image, rect);
      return result;
   end processRegion;

   function loadShape(cc: Character; path: String) return CharacterDescriptor is
      result: CharacterDescriptor;
      image: PixelArray.ImagePlane;
      regions: ImageRegions.RegionVector.Vector;
   begin
      result.c := cc;

      image := preprocess(ImageIO.load(path));
      -- detect regions
      regions := ImageRegions.detectRegions(image);

      result.d := processRegion(image, regions.First_Element);

      return result;
   end loadShape;

   function init return DB is
      result: DB;
      reg: ShapeVector.Vector;
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

      reg := loadShapes("20180501.1.jpg");
      for d in 0 .. reg.Length - 1 loop
         result.add(reg(Integer(d)));
      end loop;

      return result;
   end init;

   procedure add(database: in out DB; desc: CharacterDescriptor) is
   begin
      database.shapes.Append(desc);
   end add;

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

   function loadShapes(imagePath: String) return ShapeVector.Vector is
      result: ShapeVector.Vector;
      image: PixelArray.ImagePlane;
      r: ImageRegions.RegionVector.Vector;
      dotPosition: Integer := -1;
   begin
      image := preprocess(ImageIO.load(imagePath));
      dotPosition := Index(imagePath, ".");
      r := ImageRegions.detectRegions(image);
      if Integer(r.Length) /= dotPosition -1 then
         raise Capacity_Error with imagePath & ": processing error";
      end if;
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

end ShapeDatabase;
