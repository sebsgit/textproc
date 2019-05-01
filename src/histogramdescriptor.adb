with Ada.Numerics.Generic_Elementary_Functions;

with PixelArray;
with Histogram;
with HistogramGenerator;
with ImageRegions;

use PixelArray;
use ImageRegions;

package body HistogramDescriptor is

   package FloatFunctions is new Ada.Numerics.Generic_Elementary_Functions(Float);

   function create(image: PixelArray.ImagePlane; region: ImageRegions.Rect) return Data is
      result: Data;
   begin
      result.horizontal := HistogramGenerator.horizontalProjection(image, region).resized(BinCount).normalized;
      result.vertical := HistogramGenerator.verticalProjection(image, region).resized(BinCount).normalized;
      return result;
   end create;

   function kld(h0, h1: Histogram.Data) return Float is
      result: Float := 0.0;
   begin
      for i in 0 .. h0.size - 1 loop
         if h1.get(i) /= 0.0 then
            result := result + h0.get(i) * FloatFunctions.Log(h0.get(i) / h1.get(i));
         end if;
      end loop;
      return result;
   end kld;

   function jsd(h0, h1: Histogram.Data) return Float is
      m: Histogram.Data(h0.size);
   begin
      m := h0.add(h1);
      m.multiply(0.5);
      return 0.5 * kld(h0, m) + 0.5 * kld(h1, m);
   end jsd;

   function computeDivergence(h0, h1: Histogram.Data; method: Divergence) return Float is
   begin
      case method is
         when JensenShannon =>
            return jsd(h0, h1);
         when KullbackLeibler =>
            return kld(h0, h1);
      end case;
   end computeDivergence;

end HistogramDescriptor;
