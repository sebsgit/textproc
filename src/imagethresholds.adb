with PixelArray;
with Ada.Containers.Vectors;
with Ada.Text_IO;

package body ImageThresholds is
   function isBinary(image: PixelArray.ImagePlane) return Boolean is
      function checkPixel(px: Pixel) return Boolean is
      begin
         return (px = 255 or px = 0);
      end checkPixel;
   begin
      return image.allPixels(checkPixel'Access);
   end isBinary;

   procedure simple(image: in out PixelArray.ImagePlane; threshold: PixelArray.Pixel) is
   begin
      for y in 0 .. image.width - 1 loop
         for x in 0 .. image.height - 1 loop
            image.set(x, y, (if image.get(x, y) > threshold then 255 else 0));
         end loop;
      end loop;
   end simple;

   function simple(image: PixelArray.ImagePlane; threshold: PixelArray.Pixel) return PixelArray.ImagePlane is
   begin
      return result: ImagePlane := PixelArray.allocate(width => image.width, height => image.height) do
         simple(result, threshold);
      end return;
   end simple;

   function bernsenAdaptative(image: PixelArray.ImagePlane; radius: Positive; c_min: PixelArray.Pixel) return PixelArray.ImagePlane is
      type MinMaxIntensity is record
         min, max: PixelArray.Pixel;
      end record;

      function circleMinMax(image: PixelArray.ImagePlane; x, y: Natural; radius: Positive) return MinMaxIntensity is
         result: MinMaxIntensity;
      begin
         result.max := 0;
         result.min := 255;
         for px in x - radius .. x + radius loop
            if px > 0 and px < image.width then
               for py in y - radius .. y + radius loop
                  if py > 0 and py < image.height then
                     if abs((x - px) ** 2 + (y - py) ** 2) < radius * radius then
                        result.min := PixelArray.Pixel'Min(result.min, image.get(px, py));
                        result.max := PixelArray.Pixel'Max(result.max, image.get(px, py));
                     end if;
                  end if;
               end loop;
            end if;
         end loop;
         return result;
      end circleMinMax;

      minMax: MinMaxIntensity;
      threshold: PixelArray.Pixel;
   begin
      return result: PixelArray.ImagePlane := PixelArray.allocate(image.width, image.height) do
         for py in 0 .. image.height - 1 loop
            for px in 0 .. image.width - 1 loop

               minMax := circleMinMax(image  => image,
                                      x      => px,
                                      y      => py,
                                      radius => radius);
               threshold := (if minMax.max - minMax.min >= c_min then (minMax.min + minMax.max) / 2 else 0);
               result.set(px, py, (if image.get(px, py) > threshold then 255 else 0));

            end loop;
         end loop;
      end return;
   end bernsenAdaptative;

end ImageThresholds;
