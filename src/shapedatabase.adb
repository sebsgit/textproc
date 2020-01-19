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
      result.histogram := HistogramDescriptor.create(image  => image,
                                                     region => rect.area);
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
      image: PixelArray.ImagePlane;
      r: ImageRegions.RegionVector.Vector;
      basePath: Ada.Strings.Unbounded.Unbounded_String := getCharacterString(imagePath);
   begin
      image := preprocess(ImageIO.load(imagePath));
      r := ImageRegions.detectRegions(image);
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
   staticDB := init;
end ShapeDatabase;
