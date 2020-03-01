with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Numerics.Float_Random;

with CSV;
with MathUtils;

use MathUtils.Float_Vec;

package body CSVTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testParse'Access, "parse csv");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("CSV parse Tests");
   end Name;

   procedure testParse(T : in out Test_Cases.Test_Case'Class) is
      r: CSV.Reader := CSV.open("../training_set_test_cases/csv_test.txt");
      vec: MathUtils.Vector;
   begin
      Assert(r.hasNext, "");

      vec := r.next;
      Assert(vec = (0.0 & 0.5 & 1.0 & 1.5), "");
      Assert(r.hasNext, "has next");

      vec := r.next;
      Assert(vec = (-2.5 & (-0.5)), "");
      Assert(r.hasNext = False, "has next");
   end testParse;

end CSVTests;
