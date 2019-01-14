with PixelArray;

package ImageFilters is
   function gaussian(image: PixelArray.ImagePlane; size: Positive; sigma: Float) return PixelArray.ImagePlane;
end ImageFilters;
