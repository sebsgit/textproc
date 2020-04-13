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
      Register_Routine (T, testDivergence'Access, "divergence");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Histogram Descriptor Tests");
   end Name;

   function loadImage(path: String) return Data is
      regions: ImageRegions.RegionVector.Vector;
      image: constant PixelArray.ImagePlane := ShapeDatabase.Preprocess_And_Detect_Regions(ImageIO.load("2.jpg"), regions);
   begin
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

   procedure testDivergence(T : in out Test_Cases.Test_Case'Class) is
      h0, h1: Histogram.Data(3);
      result: Float;
   begin
      h0.set(0, 0.36);
      h0.set(1, 0.48);
      h0.set(2, 0.16);
      h1.set(0, 0.333);
      h1.set(1, 0.333);
      h1.set(2, 0.333);

      result := HistogramDescriptor.computeDivergence(h0, h1, HistogramDescriptor.KullbackLeibler);
      Assert(abs (result - 0.0863) < 0.001, "D(P||Q)");

      result := HistogramDescriptor.computeDivergence(h1, h0, HistogramDescriptor.KullbackLeibler);
      Assert(abs (result - 0.0964) < 0.001, "D(Q||P)");

      result := HistogramDescriptor.computeDivergence(h0, h1, HistogramDescriptor.JensenShannon);
      Assert(abs (result - 0.0224) < 0.001, "JSD(P||Q)");
      declare
         resulth1h0: Float;
      begin
         resulth1h0 := HistogramDescriptor.computeDivergence(h1, h0, HistogramDescriptor.JensenShannon);
         Assert(result = resulth1h0, "JSD symmetry");
      end;
   end testDivergence;

end HistogramDescriptorTests;
