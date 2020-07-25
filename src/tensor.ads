with Ada.Numerics.Generic_Real_Arrays;
with Ada.Containers.Vectors;
with System; use System;

with MathUtils;

package Tensor is
   pragma Elaborate_Body(Tensor);

   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   package Float_Vec is new Ada.Numerics.Generic_Real_Arrays(Real => Float);
   package Int_Vec is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                 Element_Type => Positive);

   type Dims is new Int_Vec.Vector with null record;
   type Float_Array is array (Natural range <>) of Float;

   type Var is tagged private;

   function Allocate(value_count: in Positive) return Var;

   function Variable(values: in Float_Array) return Var
     with Pre => values'Length > 0,
     Post => (Dimension_Count(Variable'Result) = 1) and then (Dimension(Variable'Result, 1) = values'Length);

   function Matrix(row_length: in Positive; values: in Float_Array) return Var
     with Pre => values'Length mod row_length = 0,
     Post => (Dimension_Count(Matrix'Result) = 2) and then (Dimension(Matrix'Result, 1) = values'Length / row_length) and then (Dimension(Matrix'Result, 2) = row_length);

   function Dimension_Count(v: in Var'Class) return Positive;
   function Dimension(v: in Var'Class; n_dim: in Positive) return Positive
     with Pre => n_dim <= Dimension_Count(v);

   function Element_Count(d: in Var'Class) return Positive;

   function Data(v: in Var) return MathUtils.Vector;

   function Element(v: in Var; idx_0, idx_1: in Positive) return Float
     with Pre =>
       (Dimension_Count(v) = 2 and then idx_0 <= Dimension(v, 1) and then idx_1 <= Dimension(v, 2))
       or else
         (Dimension_Count(v) = 1 and then idx_1 <= Dimension(v, 1) and then idx_0 = 1);

   procedure Set(v: out Var; values: in Float_Array)
     with Pre => Element_Count(v) = values'Length;

   function Flatten(v: in Var'Class) return Var
     with Post => Dimension_Count(Flatten'Result) = 1 and then Dimension(Flatten'Result, 1) = Element_Count(v);
   procedure Flatten(v: in out Var'Class)
     with Post => Dimension_Count(v) = 1 and then Dimension(v, 1) = Element_Count(v);

   procedure Reshape(v: in out Var'Class; s0, s1: Positive)
     with Pre => Element_Count(v) = s0 * s1;

   procedure Random(v: in out Var'Class);

   function Dot(v0, v1: in Var'Class) return Var
     with Pre => Dimension(v0, Dimension_Count(v0)) = Dimension(v1, 1);

   function "+" (v0, v1: in Var'Class) return Var
     with Pre => Dimension_Count(v0) = Dimension_Count(v1) and then Element_Count(v0) = Element_Count(v1),
     Post => Dimension_Count("+"'Result) = Dimension_Count(v0) and then Element_Count("+"'Result) = Element_Count(v0);

   function "-" (v0, v1: in Var'Class) return Var
     with Pre => Dimension_Count(v0) = Dimension_Count(v1) and then Element_Count(v0) = Element_Count(v1),
     Post => Dimension_Count("-"'Result) = Dimension_Count(v0) and then Element_Count("-"'Result) = Element_Count(v0);

   type Lambda_Func is access function (x: Float) return Float;

   procedure Apply(v: in out Var; fn: Lambda_Func);

private
   type Var is tagged record
      data: aliased MathUtils.Vector;
      size: Dims;
   end record;
end Tensor;
