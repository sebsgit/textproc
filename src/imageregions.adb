with PixelArray; use PixelArray;
with Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Generic_Sort;

use Ada.Containers;

-- keep track of (min, max) coords for each label
--
package body ImageRegions is

   function toString(r: Rect) return String is
   begin
      return r.x'Image & ", " & r.y'Image & "; " & r.width'Image & "x" & r.height'Image;
   end toString;

   type Bounds is tagged record
      minX, minY, maxX, maxY: Natural;
   end record;

   function toRect(bound: Bounds) return Rect is
      result: Rect;
   begin
      result.x := bound.minX;
      result.y := bound.minY;
      result.width := bound.maxX - bound.minX;
      result.height := bound.maxY - bound.minY;
      return result;
   end toRect;

   type RegionTempData is tagged record
      box: Bounds;
      pixelCount: Natural;
      sumX, sumY: Natural;
   end record;

   function fromPoint(x, y: Natural) return RegionTempData is
   begin
      return RegionTempData'(box => Bounds'(minX => x, maxX => x, minY => y, maxY => y), pixelCount => 1, sumX => x, sumY => y);
   end fromPoint;

   procedure includePoint(d: in out RegionTempData; x, y: Natural) is
   begin
      d.box.minX := Natural'Min(x, d.box.minX);
      d.box.maxX := Natural'Max(x, d.box.maxX);
      d.box.minY := Natural'Min(y, d.box.minY);
      d.box.maxY := Natural'Max(y, d.box.maxY);
      d.pixelCount := d.pixelCount + 1;
      d.sumX := d.sumX + x;
      d.sumY := d.sumY + y;
   end includePoint;

   package LabelMapPkg is new Ada.Containers.Indefinite_Ordered_Maps(Key_Type => RegionLabel, Element_Type => RegionTempData);
   use LabelMapPkg;

   procedure assignLabel(image: in out PixelArray.ImagePlane; x, y: Natural; label: RegionLabel; labelData: in out LabelMapPkg.Map) is
   begin
      if image.isInside(x, y) then
         if image.get(x, y) = 0 then
            image.set(x, y, PixelArray.Pixel(label));
            if not labelData.Contains(label) then
               labelData.Include(label, fromPoint(x, y));
            else
               includePoint(labelData(label), x, y);
            end if;
            assignLabel(image, x, y + 1, label, labelData);
            assignLabel(image, x, y - 1, label, labelData);
            assignLabel(image, x + 1, y, label, labelData);
            assignLabel(image, x - 1, y, label, labelData);
         end if;
      end if;
   end assignLabel;

   procedure assignLabels(image: in out PixelArray.ImagePlane; labelData: in out LabelMapPkg.Map) is
      label: RegionLabel;
   begin
      label := 1;
      for y in 0 .. image.height - 1 loop
         for x in 0 .. image.width - 1 loop
            if image.get(x, y) = 0 then
               assignLabel(image, x, y, label, labelData);
               label := label + 1;
            end if;
         end loop;
      end loop;
   end assignLabels;

   procedure drawCross(image: out PixelArray.ImagePlane; x, y: Natural; color: PixelArray.Pixel; size: Positive) is
   begin
      for px in x - size .. x + size loop
         image.set(px, y, color);
      end loop;
      for py in y - size .. y + size loop
         image.set(x, py, color);
      end loop;
   end drawCross;

   function detectRegions(image: in out PixelArray.ImagePlane) return RegionVector.Vector is
      result: RegionVector.Vector;
      labelData: LabelMapPkg.Map;
   begin
      assignLabels(image, labelData);
      for it in labelData.Iterate loop
         declare
            newRegion: Region;
            data: RegionTempData;
         begin
            data := labelData(it);
            newRegion.label := Key(it);
            newRegion.area := data.box.toRect;
            newRegion.pixelCount := data.pixelCount;
            newRegion.center := Centroid'(x => Float(data.sumX) / Float(data.pixelCount),
                                          y => Float(data.sumY) / Float(data.pixelCount));
            if newRegion.pixelCount > 10 then
               result.Append(newRegion);
            end if;
         end;
      end loop;
      return result;
   end detectRegions;

   procedure sortRegions(input: in out RegionVector.Vector) is
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
   end sortRegions;

end ImageRegions;
