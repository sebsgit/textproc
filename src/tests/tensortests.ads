with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package TensorTests is
   type TestCase is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out TestCase);

   function Name(T: TestCase) return Message_String;

   procedure testTensor(T : in out Test_Cases.Test_Case'Class);
   procedure testFlatten(T : in out Test_Cases.Test_Case'Class);
   procedure testDataGetter(T : in out Test_Cases.Test_Case'Class);
   procedure testDot(T : in out Test_Cases.Test_Case'Class);
   procedure testPlus(T : in out Test_Cases.Test_Case'Class);
   procedure testMinus(T : in out Test_Cases.Test_Case'Class);
end TensorTests;
