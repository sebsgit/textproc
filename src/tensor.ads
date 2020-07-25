with Ada.Numerics.Generic_Real_Arrays;
with System; use System;

package Tensor is
   pragma Elaborate_Body(Tensor);
   pragma Preelaborate(Tensor);

   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   package Float_Vec is new Ada.Numerics.Generic_Real_Arrays(Real => Float);

   type Dims is array (Natural range <>) of Natural;
   type Float_Array is array (Natural range <>) of Float;

   type Var (n_dims, total_size: Positive) is tagged private;

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

   function Data_Address(v: in Var'Class) return System.Address
     with Post => Data_Address'Result /= System.Null_Address;
   function Data(v: in Var) return Float_Vec.Real_Vector;

   function Element(v: in Var; idx_0, idx_1: in Positive) return Float
     with Pre =>
       (Dimension_Count(v) = 2 and then idx_0 <= Dimension(v, 1) and then idx_1 <= Dimension(v, 2))
       or else
         (Dimension_Count(v) = 1 and then idx_1 <= Dimension(v, 1) and then idx_0 = 1);

   function Flatten(v: in Var'Class) return Var
     with Post => Dimension_Count(Flatten'Result) = 1 and then Dimension(Flatten'Result, 1) = v.total_size;
   procedure Flatten(v: in out Var'Class)
     with Post => Dimension_Count(v) = 1 and then Dimension(v, 1) = v.total_size;

   function Dot(v0, v1: in Var'Class) return Var
     with Pre => Dimension(v0, Dimension_Count(v0)) = Dimension(v1, 1);

   function "+" (v0, v1: in Var'Class) return Var
     with Pre => Dimension_Count(v0) = Dimension_Count(v1) and then Element_Count(v0) = Element_Count(v1),
     Post => Dimension_Count("+"'Result) = Dimension_Count(v0) and then Element_Count("+"'Result) = Element_Count(v0);

   function "-" (v0, v1: in Var'Class) return Var
     with Pre => Dimension_Count(v0) = Dimension_Count(v1) and then Element_Count(v0) = Element_Count(v1),
     Post => Dimension_Count("-"'Result) = Dimension_Count(v0) and then Element_Count("-"'Result) = Element_Count(v0);

private
   type Var (n_dims, total_size: Positive) is tagged record
      data: aliased Float_Vec.Real_Vector (1 .. total_size);
      size: Dims (1 .. n_dims);
   end record;
end Tensor;
