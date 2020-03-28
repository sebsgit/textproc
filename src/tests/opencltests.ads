with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package OpenCLTests is
   type TestCase is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out TestCase);

   function Name(T: TestCase) return Message_String;

   procedure testLoad(T : in out Test_Cases.Test_Case'Class);
   procedure testObjectAPI(T: in out Test_Cases.Test_Case'Class);
end OpenCLTests;
