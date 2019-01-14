with PixelArray; use PixelArray;

package body Morphology is
   function maxPixel(image: PixelArray.ImagePlane; x, y: Natural; size: Positive) return PixelArray.Pixel is
      result: PixelArray.Pixel;
   begin
      result := image.get(x, y);
      for py in y - size / 2 .. y + size / 2 loop
         for px in x - size / 2 .. x + size / 2 loop
            if image.isInside(px, py) then
               result := PixelArray.Pixel'Max(result, image.get(px, py));
               if result = PixelArray.Pixel'Last then
                  return result;
               end if;
            end if;
         end loop;
      end loop;
      return result;
   end maxPixel;

   function minPixel(image: PixelArray.ImagePlane; x, y: Natural; size: Positive) return PixelArray.Pixel is
      result: PixelArray.Pixel;
   begin
      result := image.get(x, y);
      for py in y - size / 2 .. y + size / 2 loop
         for px in x - size / 2 .. x + size / 2 loop
            if image.isInside(px, py) then
               result := PixelArray.Pixel'Min(result, image.get(px, py));
               if result = PixelArray.Pixel'First then
                  return result;
               end if;
            end if;
         end loop;
      end loop;
      return result;
   end minPixel;

   function dilate(image: PixelArray.ImagePlane; size: Positive) return PixelArray.ImagePlane is
      result: PixelArray.ImagePlane;
   begin
      result := PixelArray.allocate(image.width, image.height);
      for py in 0 .. image.height - 1 loop
         for px in 0 .. image.width - 1 loop
            result.set(px, py, maxPixel(image, px, py, size));
         end loop;
      end loop;
      return result;
   end dilate;

   function erode(image: PixelArray.ImagePlane; size: Positive) return PixelArray.ImagePlane is
      result: PixelArray.ImagePlane;
   begin
      result := PixelArray.allocate(image.width, image.height);
      for py in 0 .. image.height - 1 loop
         for px in 0 .. image.width - 1 loop
            result.set(px, py, minPixel(image, px, py, size));
         end loop;
      end loop;
      return result;
   end erode;
end Morphology;
