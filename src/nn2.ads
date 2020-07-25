with Ada.Containers.Vectors;
with Ada.Finalization;

with Tensor;

package NN2 is
   pragma Elaborate_Body(NN2);

   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Model is tagged limited private;

   type Layer is tagged limited record
      null;
   end record;

   function Forward(lay: in Layer; values: in Tensor.Var) return Tensor.Var;
   function Input_Size(lay: in Layer) return Positive;
   function Output_Size(lay: in Layer) return Positive;

   type DenseLayer is new Layer with
      record
         weights: Tensor.Var;
      end record;

   type Layer_Access is access Layer'Class;
   type Dense_Layer_Access is access all DenseLayer;

   function Create(input_size: in Positive) return Model;
   procedure Add_Dense_Layer(m: in out Model; neuron_count: in Positive);
   function Add_Dense_Layer(m: in out Model; neuron_count: in Positive) return Dense_Layer_Access;

   function Forward(m: in Model; values: in Tensor.Var) return Tensor.Var;


   overriding
   function Forward(lay: in DenseLayer; values: in Tensor.Var) return Tensor.Var;
   overriding
   function Input_Size(lay: in DenseLayer) return Positive;
   overriding
   function Output_Size(lay: in DenseLayer) return Positive;


private
   package Layer_Vec is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                   Element_Type => Layer_Access);
   type Model is new Ada.Finalization.Limited_Controlled with record
      input_size: Positive;
      layers: Layer_Vec.Vector;
   end record;
   overriding procedure Finalize(This: in out Model);

end NN2;
