with Ada.Unchecked_Deallocation;

with Tensor; use Tensor;

package body NN2 is

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

   function Forward(lay: in Layer; values: in Tensor.Var) return Tensor.Var is
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

   function Add_Dense_Layer(m: in out Model; neuron_count: in Positive) return Dense_Layer_Access is
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
      added_elem.weights := Tensor.Allocate(neuron_count * value_count);
      added_elem.weights.Reshape(neuron_count, value_count);
      added_elem.weights.Random;
      added_elem.biases := Tensor.Allocate(neuron_count);
      added_elem.biases.Random;
      return added_elem;
   end Add_Dense_Layer;

   procedure Add_Dense_Layer(m: in out Model; neuron_count: in Positive) is
      tmp: Dense_Layer_Access := Add_Dense_Layer(m, neuron_count);
   begin
      null;
   end Add_Dense_Layer;

   function Forward(lay: in DenseLayer; values: in Tensor.Var) return Tensor.Var is
      result: Tensor.Var;
   begin
      result := lay.weights.Dot(values) + lay.biases;
      return result;
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
