with AUnit.Assertions; use AUnit.Assertions;
with Interfaces.C.Strings;
with Ada.Text_IO;

with ImageIO;
with PixelArray;
with ImageRegions;
with Histogram;
with HistogramGenerator;

use PixelArray;

package body HistogramTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testBasicHistograms'Access, "basic histograms");
      Register_Routine (T, testRescale'Access, "resizing histograms");
      Register_Routine (T, testMultiplication'Access, "multiplication");
      Register_Routine (T, testProjections'Access, "projecting images");
      Register_Routine (T, testDistance'Access, "histogram distances");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Histogram Tests");
   end Name;

   procedure testBasicHistograms(T : in out Test_Cases.Test_Case'Class) is
      d: Histogram.Data(5);
      d1: Histogram.Data(5);
   begin
      Assert(d.sum = 0.0, "test 1");
      Assert(d.size = 5, "test size");
      d.set(0, 5.0);
      d.set(1, 4.0);
      d.set(2, 3.0);
      d.set(3, 2.0);
      d.set(4, 1.0);
      Assert(d.get(3) = 2.0, "get");
      Assert(d.sum = 15.0, "test sum");
      Assert(d.average = 3.0, "avg");
      d1 := d.normalized;
      Assert(d1.sum = 1.0, "test normalized sum");
      Assert(d.sum = 15.0, "control sum");
      d.normalize;
      Assert(d.sum = 1.0, "test normalized sum");
   end testBasicHistograms;

   procedure testRescale(T : in out Test_Cases.Test_Case'Class) is
      d: Histogram.Data(3);
      resized: Histogram.Data(4);
   begin
      d.set(0, 7.0);
      d.set(1, 3.0);
      d.set(2, 1.0);
      resized := d.resized(4);
      Assert(resized.get(0) = d.get(0), "0");
      Assert(resized.get(1) < d.get(0) and resized.get(1) > d.get(1), "1");
      Assert(resized.get(2) < d.get(1) and resized.get(2) > d.get(2), "2");
      Assert(resized.get(3) = d.get(2), "3");

      resized.set(0, 4.0);
      resized.set(1, 1.0);
      resized.set(2, 0.0);
      resized.set(3, 4.0);
      d := resized.resized(3);

      Assert(d.get(0) = resized.get(0), "0");
      Assert(d.get(1) < resized.get(0) and d.get(1) > resized.get(2), "1");
      Assert(d.get(2) = resized.get(3), "2");
   end testRescale;

   procedure testMultiplication(T: in out Test_Cases.Test_Case'Class) is
      h0, h1: Histogram.Data(3);
   begin
      h0.set(0, 1.0);
      h0.set(1, 2.0);
      h0.set(2, 3.0);
      h1 := h0;
      h1.multiply(1.5);
      Assert(h1.get(0) = 1.5, "0");
      Assert(h1.get(1) = 3.0, "1");
      Assert(h1.get(2) = 4.5, "2");
      Assert(h1.compare(h0, Histogram.ChiSquare) /= 0.0, "distance not equal");
      Assert(h1.compare(h0.multiplied(1.5), Histogram.ChiSquare) = 0.0, "distance equal");
      h1 := h0.add(h0);
      Assert(h1.compare(h0.multiplied(2.0), Histogram.ChiSquare) = 0.0, "distance equal");
   end testMultiplication;

   procedure testProjections(T: in out Test_Cases.Test_Case'Class) is
      image: PixelArray.ImagePlane := PixelArray.allocate(width  => 5, height => 5);
      r: ImageRegions.Rect;
   begin
      r.x := 0;
      r.y := 0;
      r.width := 5;
      r.height := 5;
      image.set(Pixel(255));
      image.set(2, 0, 0);
      image.set(2, 1, 0);
      image.set(2, 2, 0);
      image.set(2, 3, 0);
      image.set(2, 4, 0);
      -- horizontal and vertical projections of a straight vertical line
      declare
         hist: Histogram.Data := HistogramGenerator.horizontalProjection(image, r);
      begin
         Assert(hist.size = r.width, "hist w");
         Assert(hist.sum = 5.0, "hist sum");
         Assert(hist.get(0) = 0.0, "hist 0");
         Assert(hist.get(1) = 0.0, "hist 1");
         Assert(hist.get(2) = 5.0, "hist 2");
         Assert(hist.get(3) = 0.0, "hist 3");
         Assert(hist.get(4) = 0.0, "hist 4");

         hist := HistogramGenerator.verticalProjection(image, r);
         Assert(hist.size = r.height, "hist w");
         Assert(hist.sum = 5.0, "hist sum");
         Assert(hist.get(0) = 1.0, "hist 0");
         Assert(hist.get(1) = 1.0, "hist 1");
         Assert(hist.get(2) = 1.0, "hist 2");
         Assert(hist.get(3) = 1.0, "hist 3");
         Assert(hist.get(4) = 1.0, "hist 4");
      end;

      -- projections of y = x
      image.set(Pixel(255));
      image.set(0, 0, 0);
      image.set(1, 1, 0);
      image.set(2, 2, 0);
      image.set(3, 3, 0);
      image.set(4, 4, 0);

      declare
         hist: Histogram.Data := HistogramGenerator.horizontalProjection(image, r);
      begin
         Assert(hist.size = r.width, "hist w");
         Assert(hist.sum = 5.0, "hist sum");
         Assert(hist.get(0) = 1.0, "hist 0");
         Assert(hist.get(1) = 1.0, "hist 1");
         Assert(hist.get(2) = 1.0, "hist 2");
         Assert(hist.get(3) = 1.0, "hist 3");
         Assert(hist.get(4) = 1.0, "hist 4");

         hist := HistogramGenerator.verticalProjection(image, r);
         Assert(hist.size = r.height, "hist w");
         Assert(hist.sum = 5.0, "hist sum");
         Assert(hist.get(0) = 1.0, "hist 0");
         Assert(hist.get(1) = 1.0, "hist 1");
         Assert(hist.get(2) = 1.0, "hist 2");
         Assert(hist.get(3) = 1.0, "hist 3");
         Assert(hist.get(4) = 1.0, "hist 4");
      end;
   end testProjections;

   procedure testDistance(T: in out Test_Cases.Test_Case'Class) is
      h0, h1: Histogram.Data(5);
      dist, dist2: Float := 0.0;
      method: Histogram.CompareMethod;
   begin
      method := Histogram.Bhattacharyya;
      dist := h0.compare(h1, method);
      Assert(dist = 0.0, "compare id");
      h0.set(0, 1.0);
      h0.set(1, 2.0);
      h0.set(2, 3.0);
      h0.set(3, 4.0);
      h0.set(4, 5.0);
      dist := h0.compare(h1, method);
      Assert(dist > 0.0, "compare different");
      h1.set(0, 10.0);
      h1.set(1, 10.0);
      h1.set(2, 30.0);
      h1.set(3, 40.0);
      h1.set(4, 50.0);
      dist2 := h0.compare(h1, method);
      Assert(dist2 < dist, "similarity");
   end testDistance;

end HistogramTests;
