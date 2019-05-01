with Histogram;
with PixelArray;
with ImageRegions;

package HistogramDescriptor is

   BinCount: Natural := 20;

   type Data is tagged record
      horizontal: Histogram.Data(BinCount);
      vertical: Histogram.Data(BinCount);
   end record;

   function create(image: PixelArray.ImagePlane; region: ImageRegions.Rect) return Data;
end HistogramDescriptor;
