with PixelArray;
with Ada.Text_IO;

use PixelArray;

package body HistogramGenerator is

   function verticalProjection(image: PixelArray.ImagePlane; r: ImageRegions.Rect) return Histogram.Data is
      result: Histogram.Data := Histogram.createEmpty(r.height);
   begin
      for h in 0 .. r.height - 1 loop
         declare
            total: Float := 0.0;
         begin
            for w in 0 .. r.width - 1 loop
               if image.get(r.x + w, r.y + h) /= 255 then
                  total := total + 1.0;
               end if;
            end loop;
            result.set(h, total);
         end;
      end loop;
      return result;
   end verticalProjection;

   function horizontalProjection(image:PixelArray.ImagePlane; r: ImageRegions.Rect) return Histogram.Data is
      result: Histogram.Data := Histogram.createEmpty(r.width);
   begin
      for h in 0 .. r.height - 1 loop
         for w in 0 .. r.width - 1 loop
            if image.get(r.x + w, r.y + h) /= 255 then
               result.set(w, result.get(w) + 1.0);
            end if;
         end loop;
      end loop;
      return result;
   end horizontalProjection;

end HistogramGenerator;
