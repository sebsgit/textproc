with AUnit.Assertions; use AUnit.Assertions;
with Interfaces.C.Strings;
with Ada.Text_IO;

with ImageIO;
with Histogram;

package body HistogramTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testBasicHistograms'Access, "basic histograms");
      Register_Routine (T, testRescale'Access, "resizing histograms");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Histogram Tests");
   end Name;

   procedure testBasicHistograms(T : in out Test_Cases.Test_Case'Class) is
      d: Histogram.Data := Histogram.createEmpty(size => 5);
      d1: Histogram.Data := Histogram.createEmpty(size => 5);
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
      d := Histogram.createEmpty(3);
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

end HistogramTests;
