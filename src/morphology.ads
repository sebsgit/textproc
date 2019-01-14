with PixelArray;

package Morphology is
   function erode(image: PixelArray.ImagePlane; size: Positive) return PixelArray.ImagePlane
     with Pre => size mod 2 /= 0;
   function dilate(image: PixelArray.ImagePlane; size: Positive) return PixelArray.ImagePlane
     with Pre => size mod 2 /= 0;
end Morphology;
