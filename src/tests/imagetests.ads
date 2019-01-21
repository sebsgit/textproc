with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package ImageTests is
   type ImageTest is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out ImageTest);

   function Name(T: ImageTest) return Message_String;

   procedure testPixelArray(T : in out Test_Cases.Test_Case'Class);

   procedure testImageThresholding(T: in out Test_Cases.Test_Case'Class);

   procedure testImageIO(T: in out Test_Cases.Test_Case'Class);
end ImageTests;
