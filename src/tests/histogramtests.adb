with AUnit.Assertions; use AUnit.Assertions;
with Interfaces.C.Strings;

with ImageIO;
with Histogram;

package body HistogramTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testBasicHistograms'Access, "basic histograms");
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

end HistogramTests;
