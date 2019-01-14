with AUnit.Assertions; use AUnit.Assertions;
with Interfaces.C.Strings;

with ImageIO;
with PixelArray;
with ShapeDatabase;
with Morphology;
with ImageFilters;
with ImageRegions;
with ImageThresholds;

package body ShapeMatchingTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testBasicShapes'Access, "basic shapes");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Shape Matcher Tests");
   end Name;

   function testShape(db: ShapeDatabase.DB; path: Interfaces.C.Strings.chars_ptr) return ShapeDatabase.MatchScore is
      image: PixelArray.ImagePlane;
      regions: ImageRegions.RegionVector.Vector;
   begin
      image := ImageIO.load(path);
      -- threshold adaptative
      image := ImageThresholds.bernsenAdaptative(image,
                                                 radius => 10,
                                                 c_min  => 35);
      -- apply morphology to strenghten shapes
      image := Morphology.erode(image, 7);
      image := Morphology.dilate(image, 7);

      -- detect regions
      regions := ImageRegions.detectRegions(image);

      return db.match(image, regions.First_Element);
   end testShape;

   procedure testBasicShapes(T : in out Test_Cases.Test_Case'Class) is
      db: ShapeDatabase.DB;
      score: ShapeDatabase.MatchScore;
   begin
      db := ShapeDatabase.init;
      score := testShape(db, Interfaces.C.Strings.New_Char_Array("1.jpg"));
      Assert(score.cc = '1', "test 1");
   end testBasicShapes;

end ShapeMatchingTests;
