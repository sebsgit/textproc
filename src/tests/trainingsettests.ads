with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package TrainingSetTests is
   type TestCase is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T: in out TestCase);

   function Name(T: TestCase) return Message_String;

   procedure testDataGenerator(T : in out Test_Cases.Test_Case'Class);
   procedure testTrainInputMNIST(T : in out Test_Cases.Test_Case'Class);
   procedure testTrainInputImages(T: in out Test_Cases.Test_Case'Class);
end TrainingSetTests;
