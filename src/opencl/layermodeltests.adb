with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;

with Tensor;
with NN2;

package body LayerModelTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testDense'Access, "Layer Model: Dense");
      Register_Routine (T, testDenseDeep'Access, "Layer Model: Dense (multilayer)");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Layer Model Tests");
   end Name;

   procedure testDense(T : in out Test_Cases.Test_Case'Class) is
      model: NN2.Model := NN2.Create(input_size => 2);
      layer: constant NN2.Dense_Layer_Access := model.Add_Dense_Layer(neuron_count => 1);
   begin

      layer.weights.Set((1.0, 2.0));
      Assert(NN2.Output_Size(layer.all) = 1, "");

      declare
         fwd_data: constant Tensor.Var := Tensor.Variable(values => (4.0, 5.0));
         fwd_result: constant Tensor.Var := NN2.Forward(layer.all, fwd_data);
         model_fwd: constant Tensor.Var := model.Forward(fwd_data);
      begin
         Assert(fwd_result.Dimension_Count = 1, "");
         Assert(fwd_result.Dimension(1) = 1, "");
         Assert(fwd_result.Element(1, 1) = 14.0, "");

         Assert(model_fwd.Dimension_Count = 1, "");
         Assert(model_fwd.Dimension(1) = 1, "");
         Assert(model_fwd.Element(1, 1) = 14.0, "");
      end;

   end testDense;

   procedure testDenseDeep(T : in out Test_Cases.Test_Case'Class) is
      model: NN2.Model := NN2.Create(input_size => 3);
      layer0: constant NN2.Dense_Layer_Access := model.Add_Dense_Layer(neuron_count => 2);
      layer1: constant NN2.Dense_Layer_Access := model.Add_Dense_Layer(neuron_count => 3);
      output_layer: constant NN2.Dense_Layer_Access := model.Add_Dense_Layer(neuron_count => 1);
   begin

      -- I0 -          L1_0 -
      --       L0_0 -
      -- I1 -          L1_1 -  OUT
      --       L0_1 -
      -- I2 -          L1_2 -

      Assert(NN2.Input_Size(layer0.all) = 3, "");
      Assert(NN2.Input_Size(layer1.all) = 2, "");
      Assert(NN2.Input_Size(output_layer.all) = 3, "");

      Assert(NN2.Output_Size(layer0.all) = 2, "");
      Assert(NN2.Output_Size(layer1.all) = 3, "");
      Assert(NN2.Output_Size(output_layer.all) = 1, "");

      layer0.weights.Set(values => (1.0, 2.0, 1.0,
                                    -1.0, -2.0, -1.0));
      layer1.weights.Set(values => (0.5, 0.2,
                                    0.7, 0.3,
                                    -0.5, 0.5));
      output_layer.weights.Set(values => (0.3, 0.4, 0.3));

      declare
         input: constant Tensor.Var := Tensor.Variable(values => (10.0, 15.0, 20.0));
         output: constant Tensor.Var := model.Forward(input);
         layer_0_res: constant Tensor.Float_Array := (1 => 1.0 * 10.0 + 2.0 * 15.0 + 1.0 * 20.0,
                                                      2 => -1.0 * 10.0 - 2.0 * 15.0 - 1.0 * 20.0);
         layer_1_res: constant Tensor.Float_Array := (1 => 0.5 * layer_0_res(1) + 0.2 * layer_0_res(2),
                                                      2 => 0.7 * layer_0_res(1) + 0.3 * layer_0_res(2),
                                                      3 => -0.5 * layer_0_res(1) + 0.5 * layer_0_res(2));
         expected_result: constant Float := 0.3 * layer_1_res(1) + 0.4 * layer_1_res(2) + 0.3 * layer_1_res(3);
      begin
         Assert(output.Element_Count = 1, "");
         Assert(expected_result = output.Element(1, 1), "Expected: " & expected_result'Image & ", actual: " & Float(output.Element(1, 1))'Image);
      end;

   end testDenseDeep;
end LayerModelTests;
