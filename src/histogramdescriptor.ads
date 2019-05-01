with Histogram;
with PixelArray;
with ImageRegions;

package HistogramDescriptor is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   BinCount: Natural := 20;

   type Divergence is (JensenShannon, KullbackLeibler);

   type Data is tagged record
      horizontal: Histogram.Data(BinCount);
      vertical: Histogram.Data(BinCount);
   end record;

   function create(image: PixelArray.ImagePlane; region: ImageRegions.Rect) return Data
     with Pre => region.width /= 0 and region.height /= 0;

   function computeDivergence(h0, h1: Histogram.Data; method: Divergence) return Float
     with Pre => (h0.Size = h1.Size);
end HistogramDescriptor;
