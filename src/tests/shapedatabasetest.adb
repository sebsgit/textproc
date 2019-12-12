with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;
with Interfaces.C.Strings;

with ImageIO;
with PixelArray;
with ShapeDatabase;
with Morphology;
with ImageFilters;
with ImageRegions;
with ImageThresholds;
with HistogramDescriptor;

package body ShapeDatabaseTest is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testLearningData'Access, "learning data");
      Register_Routine (T, testHistogramDescriptors'Access, "histogram descriptors");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Shape Database Tests");
   end Name;

   function findBestHistogramMatch(db: in ShapeDatabase.DB; desc: in ShapeDatabase.CharacterDescriptor)
   return ShapeDatabase.CharacterDescriptor
   is
      result: ShapeDatabase.CharacterDescriptor;
      bestScoreH: Float := 9999.0;
      bestScoreV: Float := 9999.0;
   begin
      for i in 0 .. Integer(db.shapes.Length - 1) loop
         declare
            currentScoreH: Float;
            currentScoreV: Float;
         begin
            currentScoreH := HistogramDescriptor.computeDivergence(h0     => desc.d.histogram.horizontal,
                                                                  h1     => db.shapes.Element(i).d.histogram.horizontal,
                                                                   method => HistogramDescriptor.JensenShannon);
            currentScoreV := HistogramDescriptor.computeDivergence(h0     => desc.d.histogram.vertical,
                                                                  h1     => db.shapes.Element(i).d.histogram.vertical,
                                                                   method => HistogramDescriptor.JensenShannon);
            if currentScoreH < bestScoreH and currentScoreV < bestScoreV then
               bestScoreH := currentScoreH;
               bestScoreV := currentScoreV;
               result := db.shapes.Element(i);
               if bestScoreV = 0.0 and bestScoreH = 0.0 then
                  return result;
               end if;
            end if;
         end;
      end loop;
      return result;
   end findBestHistogramMatch;

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

   procedure testHistogramDescriptors(T : in out Test_Cases.Test_Case'Class) is
      db: ShapeDatabase.DB;
      shapes: ShapeDatabase.ShapeVector.Vector;
      resA: ShapeDatabase.CharacterDescriptor;
      resB: ShapeDatabase.CharacterDescriptor;
   begin
      db := ShapeDatabase.init;
      shapes := ShapeDatabase.loadShapes("22.jpg");
      Assert(shapes.Length = 2, "2 shapes");

      resA := findBestHistogramMatch(db, shapes.Element(0));
      resB := findBestHistogramMatch(db, shapes.Element(1));

      Assert(resA.c = '2', "");
      Assert(resB.c = '2', "");
   end testHistogramDescriptors;

end ShapeDatabaseTest;
