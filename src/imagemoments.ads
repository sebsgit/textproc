with ImageRegions;
with PixelArray;

package ImageMoments is
   type HuMoments is tagged record
      h1, h2, h3, h4, h5, h6, h7: Float;
   end record;

   function match_I1(a, b: HuMoments) return Float;
   function match_I2(a, b: HuMoments) return Float;
   function match_I3(a, b: HuMoments) return Float;
   function match_rms(a, b: HuMoments) return Float;

   function calculateMoments(image: PixelArray.ImagePlane; region: ImageRegions.Region) return HuMoments;

   function orientationAngle(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float;
end ImageMoments;
