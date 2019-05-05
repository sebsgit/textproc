with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers; use Ada.Containers;
with Interfaces.C.Strings;

with ImageIO;
with PixelArray;
with ShapeDatabase;
with Morphology;
with ImageFilters;
with ImageRegions;
with ImageThresholds;

package body ShapeDatabaseTest is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testLearningData'Access, "learning data");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Shape Database Tests");
   end Name;

   function toString(regions: ShapeDatabase.ShapeVector.Vector) return String is
      result: String(1 .. Integer(regions.Length));
   begin
      for i in 0 .. Integer(regions.Length - 1) loop
         result(i + 1) := regions(i).c;
      end loop;
      return result;
   end toString;

   procedure testLearningData(T : in out Test_Cases.Test_Case'Class) is
      result: ShapeDatabase.ShapeVector.Vector;
   begin
      result := ShapeDatabase.loadShapes("20180501.1.jpg");
      Assert(result.Length = 8, "shapes 1");
      Assert(toString(result) = "20180501", "");
   end testLearningData;

end ShapeDatabaseTest;
