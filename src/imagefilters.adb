with PixelArray;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body ImageFilters is
   function gaussian(image: PixelArray.ImagePlane; size: Positive; sigma: Float) return PixelArray.ImagePlane is
      type Kernel is array (Natural range <>) of Float;
      k: Kernel (0 .. (2 * size + 1) * (2 * size + 1));
      result: PixelArray.ImagePlane;
      sigma2: Float := sigma * sigma;
      index: Natural;
      tmpSum: Float;

      package FloatFunc is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => Float);
   begin
      for y in -size .. size loop
         for x in -size .. size loop
            index := (x + size) + (y + size) * (2 * size + 1);
            k(index) := (1.0 / (2.0 * Ada.Numerics.Pi * sigma2)) * (FloatFunc.Exp(- (Float(x * x + y * y)) / (2.0 * sigma2)));
         end loop;
      end loop;
      result := PixelArray.allocate(image.width, image.height);
      for py in 0 .. image.height - 1 loop
         for px in 0 .. image.width - 1 loop
            tmpSum := 0.0;
            for y in -size .. size loop
               for x in -size .. size loop
                  index := (x + size) + (y + size) * (2 * size + 1);
                  if image.isInside(px + x, py + y) then
                     tmpSum := tmpSum + k(index) * Float(image.get(px + x, py + y));
                  else
                     tmpSum := tmpSum + k(index) * Float(image.get(px, py));
                  end if;
               end loop;
            end loop;
            result.set(px, py, PixelArray.Pixel(if tmpSum > 255.0 then 255.0 else tmpSum));
         end loop;
      end loop;
      return result;
   end gaussian;
end ImageFilters;
