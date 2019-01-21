with AUnit.Assertions; use AUnit.Assertions;

with PixelArray; use PixelArray;
with ImageThresholds;
with ImageIO;

package body ImageTests is

   procedure Register_Tests (T: in out ImageTest) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, testPixelArray'Access, "pixel array");
      Register_Routine(T, testImageThresholding'Access, "image thresholds");
      Register_Routine(T, testImageIO'Access, "image IO");
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

   procedure testImageIO(T: in out Test_Cases.Test_Case'Class) is
      testImage: PixelArray.ImagePlane;
      saveResult: Boolean;
   begin
      testImage := PixelArray.allocate(width  => 128,
                                       height => 128);
      testImage.set(Pixel(56));
      for i in 0 .. testImage.width - 1 loop
         testImage.set(i, i, Pixel(i));
      end loop;
      saveResult := ImageIO.save(filename => "test_image_IO.png",
                                 image    => testImage);
      testImage.set(Pixel(0));
      testImage := ImageIO.load("test_image_IO.png");
      for i in 0 .. testImage.height - 1 loop
         for j in 0 .. testImage.width - 1 loop
            if i = j then
               Assert(testImage.get(j, i) = Pixel(i), "diagonal pixel");
            else
               Assert(testImage.get(j, i) = Pixel(56), "background pixel");
            end if;
         end loop;
      end loop;
   end testImageIO;

end ImageTests;
