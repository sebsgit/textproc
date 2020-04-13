with Ada.Containers;
with AUnit.Assertions; use AUnit.Assertions;
with Interfaces.C.Strings;

with ImageIO;
with PixelArray;
with ShapeDatabase;
with Morphology;
with ImageFilters;
with ImageRegions;
with ImageThresholds;

use Ada.Containers;

package body ShapeMatchingTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testBasicShapes'Access, "basic shapes");
      Register_Routine (T, testComplexImage'Access, "complex image");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Shape Matcher Tests");
   end Name;

   function testShape(db: ShapeDatabase.DB; path: String) return ShapeDatabase.MatchScore is
      regions: ImageRegions.RegionVector.Vector;
      image: constant PixelArray.ImagePlane := ShapeDatabase.Preprocess_And_Detect_Regions(ImageIO.load(path), regions);
   begin
      return db.match(image, regions.First_Element);
   end testShape;

   procedure testBasicShapes(T : in out Test_Cases.Test_Case'Class) is
      db: ShapeDatabase.DB;
      score: ShapeDatabase.MatchScore;
   begin
      db := ShapeDatabase.getDB;
      score := testShape(db, "1.jpg");
      Assert(score.cc = '1', "test 1");
   end testBasicShapes;

   procedure testComplexImage(T: in out Test_Cases.Test_Case'Class) is
      db: ShapeDatabase.DB;
      testImage: PixelArray.ImagePlane := ImageIO.load("test_complex.jpg");
      regions: ImageRegions.RegionVector.Vector;
      score: ShapeDatabase.MatchScore;

      function matchAt(index: Integer) return ShapeDatabase.MatchScore is
      begin
         return db.match(image  => testImage,
                         region => regions(index));
      end matchAt;
   begin
      db := ShapeDatabase.getDB;
      testImage.assign(ShapeDatabase.Preprocess_And_Detect_Regions(testImage, regions));
      ImageRegions.sortRegions(regions);
      Assert(regions.Length = 11, "count of detected regions");

      score := matchAt(0);
      Assert(score.cc = '1', "match0: 1");
      score := matchAt(1);
      Assert(score.cc = '2', "match1: 2");
      score := matchAt(2);
      Assert(score.cc = '1', "match2: 1");
      score := matchAt(3);
      Assert(score.cc = '0', "match3: 0");
      score := matchAt(4);
      Assert(score.cc = '2', "match4: 2");
      score := matchAt(5);
      Assert(score.cc = '1', "match5: 1");
      score := matchAt(6);
      Assert(score.cc = '8', "match6: 8");
      score := matchAt(7);
      Assert(score.cc = '8', "match7: 8"); --fix with histograms
      score := matchAt(8);
      Assert(score.cc = '9', "match8: 9");
      score := matchAt(9);
      Assert(score.cc = '1', "match9: 1");
      score := matchAt(10);
      Assert(score.cc = '4', "match10: 4");
   end testComplexImage;

end ShapeMatchingTests;
