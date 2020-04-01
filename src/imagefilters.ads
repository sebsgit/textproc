with PixelArray;

package ImageFilters is
   type Kernel is array (Natural range <>) of Float;

   function generateKernel(size: Positive; sigma: Float) return Kernel;
   function gaussian(image: PixelArray.ImagePlane; size: Positive; sigma: Float) return PixelArray.ImagePlane;
end ImageFilters;
