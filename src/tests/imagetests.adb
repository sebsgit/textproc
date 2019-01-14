with AUnit.Assertions; use AUnit.Assertions;

with PixelArray; use PixelArray;
with ImageThresholds;

package body ImageTests is

   procedure Register_Tests (T: in out ImageTest) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, testPixelArray'Access, "pixel array");
      Register_Routine(T, testImageThresholding'Access, "image thresholds");
   end Register_Tests;

   function Name(T: ImageTest) return Test_String is
   begin
      return Format("Image tests");
   end Name;

   procedure testPixelArray(T : in out Test_Cases.Test_Case'Class) is
      testImage: PixelArray.ImagePlane;
   begin
      testImage := PixelArray.allocate(120, 120);
      Assert(testImage.isInside(0, 0), "");
      Assert(not testImage.isInside(120, 120), "");
      Assert(not testImage.isInside(-1, -2), "");
      testImage.set(10, 10, Pixel(99));
      Assert(testImage.get(10, 10) = Pixel(99), "");
   end testPixelArray;

   procedure testImageThresholding(T: in out Test_Cases.Test_Case'Class) is
      testImage: PixelArray.ImagePlane;
   begin
      testImage := PixelArray.allocate(20, 20);
      testImage.set(Pixel(0));
      testImage.set(10, 10, Pixel(45));
      testImage.set(11, 11, Pixel(46));
      testImage.set(12, 16, Pixel(99));
      ImageThresholds.simple(testImage, threshold => Pixel(45));
      Assert(ImageThresholds.isBinary(testImage), "binary output");
      Assert(testImage.get(10, 10) = Pixel(0), "");
      Assert(testImage.get(11, 11) = Pixel(255), "");
      Assert(testImage.get(12, 16) = Pixel(255), "");
      Assert(testImage.get(15, 15) = Pixel(0), "");
   end testImageThresholding;

end ImageTests;
