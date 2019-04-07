with Histogram;
with PixelArray;
with ImageRegions;
with ImageThresholds;

package HistogramGenerator is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   function verticalProjection(image: PixelArray.ImagePlane; r: ImageRegions.Rect) return Histogram.Data
     with
       Pre => (ImageThresholds.isBinary(image)),
       Post => verticalProjection'Result.size = r.height;

   function horizontalProjection(image: PixelArray.ImagePlane; r: ImageRegions.Rect) return Histogram.Data
     with
       Pre => (ImageThresholds.isBinary(image)),
       Post => horizontalProjection'Result.size = r.width;

end HistogramGenerator;
