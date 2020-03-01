with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada.Containers;

with ShapeDatabase;
with ImageIO;
with PixelArray;
with ImageRegions;

use Ada.Containers;
use Ada.Strings.Unbounded;

package body TrainingData is
   function size(data: in Set) return Natural is
   begin
      return data.values.size;
   end size;

   procedure add(data: in out Set; label: Natural; vec: MathUtils.Vector) is
   begin
      data.labels.Append(label);
      data.values.append(vec);
   end add;

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

   procedure filterRegions(regions: in out ImageRegions.RegionVector.Vector) is
   begin
      for idx in reverse regions.First_Index .. regions.Last_Index loop
         if regions(idx).pixelCount < 512 then
            regions.Delete(idx);
         end if;
      end loop;
   end filterRegions;

   procedure loadFrom(data: in out Set; path: in Ada.Strings.Unbounded.Unbounded_String) is
      searchObj: Ada.Directories.Search_Type;
      dirEnt: Ada.Directories.Directory_Entry_Type;
      image: PixelArray.ImagePlane;
      regions: ImageRegions.RegionVector.Vector;
      expectedChars: Ada.Strings.Unbounded.Unbounded_String;
      saveStatus: Boolean;
   begin
      Ada.Directories.Start_Search(Search    => searchObj,
                                   Directory => Ada.Strings.Unbounded.To_String(path),
                                   Pattern   => "*.jpg");

      while Ada.Directories.More_Entries(Search => searchObj) loop
         Ada.Directories.Get_Next_Entry(Search          => searchObj,
                                        Directory_Entry => dirEnt);

         expectedChars := getCharacterString(Ada.Directories.Simple_Name(dirEnt));

         image := ImageIO.load(Ada.Directories.Full_Name(dirEnt));
         image := ShapeDatabase.preprocess(image);
         regions := ImageRegions.detectRegions(image);
         filterRegions(regions);
         ImageRegions.sortRegions(regions);

         if Natural(regions.Length) /= Ada.Strings.Unbounded.Length(expectedChars) then
            saveStatus := ImageIO.save(filename => "debug_region_error.jpg",
                                       image    => image);
            raise Ada.IO_Exceptions.Data_Error with "Training set error - expected " & Ada.Strings.Unbounded.Length(expectedChars)'Image & " regions, got " & Natural(regions.Length)'Image;
         end if;

         -- cut and rescale the detected regions
         declare
            id: Positive := 1;
            cut: PixelArray.ImagePlane;
            label: Natural;
         begin
            for r of regions loop
               cut := image.cut(x => r.area.x,
                                y => r.area.y,
                                w => r.area.width,
                                h => r.area.height);
               cut := cut.rescale(blockSize, blockSize);
               label := Natural'Value((1 => Ada.Strings.Unbounded.Element(expectedChars, id)));
               data.labels.Append(label);
               data.values.append(toDataVector(cut));
               id := id + 1;
            end loop;
         end;

      end loop;
      Ada.Directories.End_Search(Search => searchObj);
   end loadFrom;

   function toDataVector(img: in PixelArray.ImagePlane) return MathUtils.Vector is
      result: MathUtils.Vector;
   begin
      for y in 0 .. img.height - 1 loop
         for x in 0 .. img.width - 1 loop
            result.Append(Float(img.get(x, y)) / 255.0);
         end loop;
      end loop;
      return result;
   end toDataVector;

end TrainingData;
