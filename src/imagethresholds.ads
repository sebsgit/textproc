with PixelArray; use PixelArray;
with Ada.Containers; use Ada.Containers;

package ImageThresholds is
   type MinMaxIntensity is record
      min, max: PixelArray.Pixel;
   end record;

   function isBinary(image: PixelArray.ImagePlane) return Boolean;

   procedure simple(image: in out PixelArray.ImagePlane; threshold: PixelArray.Pixel)
     with
       Pre => (image.width > 0 and image.height > 0),
     Post => (isBinary(image));
   function simple(image: PixelArray.ImagePlane; threshold: PixelArray.Pixel) return PixelArray.ImagePlane
     with Post => (isBinary(simple'Result));

   function circleMinMax(image: PixelArray.ImagePlane; x, y: Natural; radius: Positive) return MinMaxIntensity;

   function bernsenAdaptative(image: PixelArray.ImagePlane; radius: Positive; c_min: PixelArray.Pixel) return PixelArray.ImagePlane
     with Post => (isBinary(bernsenAdaptative'Result));
end ImageThresholds;
