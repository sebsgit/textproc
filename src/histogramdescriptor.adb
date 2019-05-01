with PixelArray;
with Histogram;
with HistogramGenerator;
with ImageRegions;

use PixelArray;
use ImageRegions;

package body HistogramDescriptor is

   function create(image: PixelArray.ImagePlane; region: ImageRegions.Rect) return Data is
      result: Data;
   begin
      result.horizontal := HistogramGenerator.horizontalProjection(image, region).resized(BinCount).normalized;
      result.vertical := HistogramGenerator.verticalProjection(image, region).resized(BinCount).normalized;
      return result;
   end create;

end HistogramDescriptor;
