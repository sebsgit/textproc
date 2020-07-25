with Ada.Unchecked_Deallocation;

with Tensor; use Tensor;
with MathUtils;

package body NN2 is

   function Do_RELU(x: Float) return Float is
   begin
      return Float'Max(0.0, x);
   end Do_RELU;

   function Do_LOGISTIC(x: Float) return Float is
   begin
      return 1.0 / (1.0 + MathUtils.F.Exp(-x));
   end Do_LOGISTIC;

   function Do_RELU_Derivate(x: Float) return Float is
   begin
      if x > 0.0 then
         return 1.0;
      else
         return 0.0;
      end if;
   end Do_RELU_Derivate;

   function Do_LOGISTIC_Derivate(x: Float) return Float is
      logistic_res: constant Float := Do_LOGISTIC(x);
   begin
      return x * (1.0 - x);
   end Do_LOGISTIC_Derivate;

   function activate(val: in Tensor.Var; act: in Activation) return Tensor.Var is
      result: Tensor.Var := val;
   begin
      if act = RELU then
         result.Apply(Do_RELU'Access);
      elsif act = LOGISTIC then
         result.Apply(Do_LOGISTIC'Access);
      else
         null;
      end if;
      return result;
   end activate;

   function Input_Size(lay: in Layer) return Positive is
      Not_Implemented: exception;
   begin
      raise Not_Implemented with "Input_Size not implemented";
      return 1;
   end Input_Size;

   function Output_Size(lay: in Layer) return Positive is
      Not_Implemented: exception;
   begin
      raise Not_Implemented with "Output_Size not implemented";
      return 1;
   end Output_Size;

   function Forward(lay: in out Layer; values: in Tensor.Var) return Tensor.Var is
      Not_Implemented: exception;
   begin
      raise Not_Implemented with "Forward not implemented";
      return values;
   end Forward;

   function Forward(m: in Model; values: in Tensor.Var) return Tensor.Var is
      previous_result: Tensor.Var := values;
   begin
      for it of m.layers loop
         previous_result := Forward(it.all, previous_result);
      end loop;
      return previous_result;
   end Forward;

   procedure Finalize(This: in out Model) is
      procedure Free_Layer is new Ada.Unchecked_Deallocation(Object => Layer'Class,
                                                             Name   => Layer_Access);
   begin
      for it of This.layers loop
         Free_Layer(it);
      end loop;
   end Finalize;

   function Create(input_size: in Positive) return Model is
   begin
      return m: Model do
         m.input_size := input_size;
      end return;
   end Create;

   function Add_Dense_Layer(m: in out Model; neuron_count: in Positive; act: in Activation := RELU) return Dense_Layer_Access is
      added_elem: Dense_Layer_Access;
      value_count: Positive;
   begin
      if m.layers.Is_Empty then
         value_count := m.input_size;
      else
         value_count := Output_Size(m.layers.Last_Element.all);
      end if;
      m.layers.Append(new DenseLayer);
      added_elem := Dense_Layer_Access(m.layers.Last_Element);
      added_elem.activator := act;
      added_elem.weights := Tensor.Allocate(neuron_count * value_count);
      added_elem.weights.Reshape(neuron_count, value_count);
      added_elem.weights.Random;
      added_elem.biases := Tensor.Allocate(neuron_count);
      added_elem.biases.Random;
      return added_elem;
   end Add_Dense_Layer;

   procedure Add_Dense_Layer(m: in out Model; neuron_count: in Positive; act: in Activation := RELU) is
      tmp: Dense_Layer_Access := Add_Dense_Layer(m, neuron_count, act);
   begin
      null;
   end Add_Dense_Layer;

   function Forward(lay: in out DenseLayer; values: in Tensor.Var) return Tensor.Var is
   begin
      lay.weighted_output := lay.weights.Dot(values) + lay.biases;
      lay.activations := activate(val => lay.weighted_output,
                                  act => lay.activator);
      return lay.activations;
   end Forward;

   function Input_Size(lay: in DenseLayer) return Positive is
   begin
      return lay.weights.Dimension(2);
   end Input_Size;

   function Output_Size(lay: in DenseLayer) return Positive is
   begin
      return lay.weights.Dimension(1);
   end Output_Size;
end NN2;
