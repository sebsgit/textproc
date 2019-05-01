with AUnit.Assertions; use AUnit.Assertions;
with Interfaces.C.Strings;
with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

with ImageIO;
with PixelArray;
with ImageRegions;
with Histogram;
with HistogramDescriptor;
with ShapeDatabase;

use PixelArray;
use ImageRegions;
use HistogramDescriptor;

package body HistogramDescriptorTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testBasicDescriptor'Access, "basic descriptor");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Histogram Descriptor Tests");
   end Name;

   function loadImage(path: String) return Data is
      image: PixelArray.ImagePlane;
      regions: ImageRegions.RegionVector.Vector;
   begin
      image := ShapeDatabase.preprocess(ImageIO.load("2.jpg"));
      regions := ImageRegions.detectRegions(image);
      Assert(regions.Length = 1, "1 region");

      return HistogramDescriptor.create(image, regions.Element(0).area);
   end loadImage;

   procedure testBasicDescriptor(T : in out Test_Cases.Test_Case'Class) is
      d0, d1: Data;
      distance: Float;
   begin
      d0 := loadImage("1.jpg");
      d1 := loadImage("1_1.jpg");

      distance := Histogram.compare(d0.horizontal, d1.horizontal, Histogram.Bhattacharyya);
      Assert(distance < 0.01, "distance horizontal");

      distance := Histogram.compare(d0.vertical, d1.vertical, Histogram.Bhattacharyya);
      Assert(distance < 0.01, "distance vertical");

      distance := Histogram.compare(d0.vertical, d1.horizontal, Histogram.Bhattacharyya);
      Assert(distance > 0.2, "vertical != horizontal");
   end testBasicDescriptor;

end HistogramDescriptorTests;
