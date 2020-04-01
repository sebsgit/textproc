with PixelArray;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body ImageFilters is
   package FloatFunc is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => Float);

   function generateKernel(size: Positive; sigma: Float) return Kernel is
      k: Kernel (0 .. (2 * size + 1) * (2 * size + 1));
      sigma2: constant Float := sigma * sigma;
      index: Natural;
   begin
      for y in -size .. size loop
         for x in -size .. size loop
            index := (x + size) + (y + size) * (2 * size + 1);
            k(index) := (1.0 / (2.0 * Ada.Numerics.Pi * sigma2)) * (FloatFunc.Exp(- (Float(x * x + y * y)) / (2.0 * sigma2)));
         end loop;
      end loop;
      return k;
   end generateKernel;

   function gaussian(image: PixelArray.ImagePlane; size: Positive; sigma: Float) return PixelArray.ImagePlane is
      k: constant Kernel := generateKernel(size, sigma);
      result: PixelArray.ImagePlane;
      index: Natural;
      tmpSum: Float;
   begin
      return result: PixelArray.ImagePlane := PixelArray.allocate(image.width, image.height) do
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
      end return;
   end gaussian;
end ImageFilters;
