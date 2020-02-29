with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers;

with TrainingData;
with PixelArray;
with MathUtils;
with NeuralNet;
with NNClassifier;

use Ada.Containers;
use MathUtils.Float_Vec;

package body TrainingSetTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testDataGenerator'Access, "float vec generator");
      Register_Routine (T, testTrainInput'Access, "input validation");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Training Set Tests");
   end Name;

   procedure testDataGenerator(T : in out Test_Cases.Test_Case'Class) is
      image: PixelArray.ImagePlane;
      output: MathUtils.Vector;
      expectedValues: MathUtils.Vector;
   begin
      image := PixelArray.allocate(16, 16);
      for x in 0 .. 15 loop
         for y in 0 .. 15 loop
            image.set(x, y, PixelArray.Pixel((x + 1) * (y + 1) - 1));
            expectedValues.Append(Float(image.get(x, y)) / 255.0);
         end loop;
      end loop;

      output := TrainingData.toDataVector(image);
      Assert(output.Length = 16 * 16, "data length");
      Assert(output = expectedValues, "");
   end testDataGenerator;

   function oneHotEncode(label: Natural; labelCount: Positive) return MathUtils.Vector
     with Pre => label < labelCount
   is
      result: MathUtils.Vector;
   begin
      result.Set_Length(Ada.Containers.Count_Type(labelCount));
      for x of result loop
         x := 0.0;
      end loop;
      result(label + 1) := 1.0;
      return result;
   end oneHotEncode;

   procedure testTrainInput(T : in out Test_Cases.Test_Case'Class) is
      set: TrainingData.Set;
      validationSet: TrainingData.Set;
      config: NeuralNet.Config(1);
      dnn: NNClassifier.DNN(config.size + 1);

      pred: MathUtils.Vector;
      di: Positive := 1;

      failedPredictions: Natural := 0;
   begin
      set.loadFrom(Ada.Strings.Unbounded.To_Unbounded_String("../training_set/"));
      validationSet.loadFrom(Ada.Strings.Unbounded.To_Unbounded_String("../training_set_test_cases/"));
      Assert(set.size > 100, "not enough train data: " & set.size'Image);

      config.act := NeuralNet.LOGISTIC;
      config.inputSize := TrainingData.blockArea;
      config.lr := 0.9;
      config.sizes := (1 => 1000);

      dnn := NNClassifier.create(config          => config,
                                 numberOfClasses => 10);

      for i in 0 .. 2 loop
         dnn.train(data   => set.values,
                   labels => set.labels);
      end loop;

      for lab of validationSet.labels loop
         pred := dnn.classify(validationSet.values.data(di));
         for idx in pred.First_Index .. pred.Last_Index loop
            if idx /= lab + 1 then
               if pred(idx) > pred(lab + 1) then
                  failedPredictions := failedPredictions + 1;
                  exit;
               end if;
            end if;
         end loop;
         di := di + 1;
      end loop;

      declare
         acc: Float;
      begin
         acc := Float(validationSet.size - failedPredictions) / Float(validationSet.size);
         Ada.Text_IO.Put_Line("Model accuracy: " & acc'Image);
         -- require > 70% accuracy, TODO: increase training set size
         Assert(acc > 0.69, "total: " & validationSet.size'Image & ", failed: " & failedPredictions'Image);
      end;

   end testTrainInput;
end TrainingSetTests;
