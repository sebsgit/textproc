with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Numerics.Float_Random;

with NeuralNet;
with MathUtils;

use MathUtils.Float_Vec;

package body NeuralNetTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testBasicNet'Access, "basic net");
      Register_Routine (T, testForwardPropagation'Access, "forward propagate");
      Register_Routine (T, testTrain'Access, "train basic nn");
      Register_Routine (T, testTrainComplex'Access, "train complex nn");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Neural Net Tests");
   end Name;

   procedure testBasicNet(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(3);
      net: NeuralNet.Net(config.size);
   begin
      config.act := NeuralNet.RELU;
      config.lr := 0.05;
      config.inputSize := 4;
      config.sizes := (2, 3, 2);

      net := NeuralNet.create(conf => config);

      Assert(net.layers.Length = 3, "net should have 3 layers");

      Assert(net.layers(1).Length = 2, "2 neurons in layer 1");
      Assert(net.layers(2).Length = 3, "3 neurons in layer 2");
      Assert(net.layers(3).Length = 2, "2 neurons in layer 3");

      -- assertions for weights in layer 1
      Assert(net.layers(1)(1).w'Length = config.inputSize, "expected weights in layer 1 n 1");
      Assert(net.layers(1)(2).w'Length = config.inputSize, "expected weights in layer 1 n 2");

      -- assertions for weights in layer 2
      Assert(net.layers(2)(1).w'Length = 2, "expected weights in layer 2 n 1");
      Assert(net.layers(2)(2).w'Length = 2, "expected weights in layer 2 n 2");
      Assert(net.layers(2)(3).w'Length = 2, "expected weights in layer 2 n 3");

      -- assertions for weights in layer 3
      Assert(net.layers(3)(1).w'Length = 3, "expected weights in layer 3 n 1");
      Assert(net.layers(3)(2).w'Length = 3, "expected weights in layer 3 n 2");

   end testBasicNet;

   procedure testForwardPropagation(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(2);
      net: NeuralNet.Net(config.size);
      value: Float := 0.0;
      input: MathUtils.Vector;
      result: MathUtils.Vector;
   begin
      config.inputSize := 1;
      config.act := NeuralNet.RELU;
      config.sizes := (2, 1);

      net := NeuralNet.create(config);

      Assert(net.layers.Length = 2, "layer count");
      Assert(net.layers(1).Length = 2, "neurons in layer 1");
      Assert(net.layers(2).Length = 1, "neurons in output layer");

      -- hardcode biases and weights
      net.layers(1)(1).bias := 0.3;
      net.layers(1)(1).w := (1 => 0.5);

      net.layers(1)(2).bias := 0.7;
      net.layers(1)(2).w := (1 => 0.1);

      net.layers(2)(1).bias := 0.1;
      net.layers(2)(1).w := (0.4, 0.6);

      input.Append(2.0);

      value := NeuralNet.forward(net.layers(1)(1), input);
      Assert(value = 0.3 + 0.5 * 2.0, "forward 1: " & value'Image);

      value := NeuralNet.forward(net.layers(1)(2), input);
      Assert(value = 0.7 + 0.1 * 2.0, "forward 2: " & value'Image);

      input.Clear;
      input.Append(1.0);
      input.Append(2.0);
      value := NeuralNet.forward(net.layers(2)(1), input);
      Assert(value = 1.0 * 0.4 + 2.0 * 0.6 + 0.1, "forward 3: " & value'Image);

      input.Clear;
      input.Append(2.0);

      result := net.forward(input);
      Assert(result.Length = 1, "Final net output size: " & result.Length'Image);

      value := (0.3 + 0.5 * 2.0) * 0.4 + (0.7 + 0.1 * 2.0) * 0.6 + 0.1;
      Assert(abs(result(1) - value) < Float'Epsilon, "Final net result: " & value'Image);

   end testForwardPropagation;

   procedure testTrain(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(1);
      net: NeuralNet.Net(config.size);
      previousLoss, currentLoss: Float := 0.0;
      steps: constant Positive := 10;
      input: MathUtils.Vector;
      result: MathUtils.Vector;
   begin
      config.act := NeuralNet.RELU;
      config.lr := 0.07;
      config.inputSize := 1;
      config.sizes := (1 => 1);

      net := NeuralNet.create(config);
      net.layers(1)(1).bias := 0.5;
      net.layers(1)(1).w := (1 => 0.1);

      input.Append(0.1);
      result.Append(0.99);

      net.train(input, result);
      previousLoss := MathUtils.mse(result, net.forward(input));
      for i in 1 .. steps loop
         net.train(input, result);
         currentLoss :=  MathUtils.mse(result, net.forward(input));
         Assert(currentLoss < previousLoss, "Learning failed: " & currentLoss'Image & " > " & previousLoss'Image);
         previousLoss := currentLoss;
      end loop;
   end testTrain;

   procedure testTrainComplex(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(2);
      net: NeuralNet.Net(config.size);
      input: MathUtils.Vector;
      target: MathUtils.Vector;
      steps: constant Positive := 1000;

   begin

      config.act := NeuralNet.LOGISTIC;
      config.lr := 0.1;
      config.inputSize := 3;
      config.sizes := (40, 3);

      net := NeuralNet.create(config);

      for i in 0 .. steps loop
         input := 0.01 & 0.03 & (0.5 + MathUtils.rand01);
         target := 1.0 & 0.0 & 0.0;
         net.train(input, target);

         input := (0.5 + MathUtils.rand01) & 0.03 & 0.07;
         target := 0.0 & 1.0 & 0.0;
         net.train(input, target);

         input := 0.01 & (0.5 + MathUtils.rand01) & 0.0;
         target := 0.0 & 0.0 & 1.0;
         net.train(input, target);
      end loop;

      input := 0.0 & 0.0 & 0.5;
      target := net.forward(input);
      Assert(target(1) > target(2), "");
      Assert(target(1) > target(3), "");

      input := 0.5 & 0.0 & 0.0;
      target := net.forward(input);
      Assert(target(2) > target(1), "");
      Assert(target(2) > target(3), "");

      input := 0.0 & 0.5 & 0.0;
      target := net.forward(input);
      Assert(target(3) > target(1), "");
      Assert(target(3) > target(2), "");
   end testTrainComplex;

end NeuralNetTests;
