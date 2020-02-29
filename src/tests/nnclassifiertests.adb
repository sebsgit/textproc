with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with NNClassifier;
with NeuralNet;
with DataBatch;
with MathUtils;

use MathUtils.Float_Vec;
use Ada.Containers;

package body NNClassifierTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testLogisticRegression'Access, "logistic regression");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("NN Classifier Tests");
   end Name;

   procedure testLogisticRegression(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(1);
      dnn: NNClassifier.DNN(config.size + 1);
      batch: DataBatch.Batch;
      labels: NNClassifier.LabelVector;
      trainSetSize: constant Positive := 1000;

      input: MathUtils.Vector;
      prediction: MathUtils.Vector;
   begin
      config.inputSize := 4;
      config.act := NeuralNet.LOGISTIC;
      config.lr := 0.9;
      config.sizes := (1 => 16);
      dnn := NNClassifier.create(config          => config,
                                 numberOfClasses => 2);

      for i in 1 .. trainSetSize loop
         batch.append((MathUtils.rand01 + 0.5) & 0.0 & 0.0 & (MathUtils.rand01 + 0.5));
         labels.Append(0);

         batch.append((MathUtils.rand01 + 0.5) & (MathUtils.rand01 + 0.5) & 0.0 & 0.0);
         labels.Append(1);
      end loop;

      dnn.train(batch, labels);

      input := 0.5 & 0.0 & 0.0 & 0.5;
      prediction := dnn.classify(input);
      Assert(prediction(1) > prediction(2), "");

      input := 0.5 & 0.5 & 0.0 & 0.0;
      prediction := dnn.classify(input);
      Assert(prediction(2) > prediction(1), "");

   end testLogisticRegression;
end NNClassifierTests;
