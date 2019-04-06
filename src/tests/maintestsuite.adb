with AUnit.Run;
with AUnit.Reporter.Text;

with ImageTests;
with ShapeMatchingTests;
with HistogramTests;

package body MainTestSuite is
   use AUnit.Test_Suites;

   suiteObject: aliased Test_Suite;

   imageTestCase: aliased ImageTests.ImageTest;
   shapeTestCase: aliased ShapeMatchingTests.TestCase;
   histogramTestCase: aliased HistogramTests.TestCase;

   function Suite return Access_Test_Suite is
   begin
      Add_Test(suiteObject'Access, imageTestCase'Access);
      Add_Test(suiteObject'Access, shapeTestCase'Access);
      Add_Test(suiteObject'Access, histogramTestCase'Access);
      return suiteObject'Access;
   end Suite;

   procedure runAll is
      procedure Run is new AUnit.Run.Test_Runner (MainTestSuite.Suite);
      reporter : AUnit.Reporter.Text.Text_Reporter;
   begin
      Run(reporter);
   end runAll;

end MainTestSuite;
