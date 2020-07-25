package body Tensor is
   function Element_Count(d: in Var'Class) return Positive is
      total: Positive := 1;
   begin
      for n of d.size loop
         total := total * n;
      end loop;
      return total;
   end Element_Count;

   function Variable(values: in Float_Array) return Var is
      result: Var (1, values'Length);
      idx_out: Natural := 1;
   begin
      result.size(1) := values'Length;
      for idx_in in values'First .. values'Last loop
         result.data(idx_out) := values(idx_in);
         idx_out := idx_out + 1;
      end loop;
      return result;
   end Variable;

   function Matrix(row_length: in Positive; values: in Float_Array) return Var is
      result: Var (2, values'Length);
      idx_out: Natural := 1;
   begin
      result.size(1) := values'Length / row_length;
      result.size(2) := row_length;
      for idx_in in values'First .. values'Last loop
         result.data(idx_out) := values(idx_in);
         idx_out := idx_out + 1;
      end loop;
      return result;
   end Matrix;

   function Dimension_Count(v: in Var'Class) return Positive is
   begin
      return v.n_dims;
   end Dimension_Count;

   function Dimension(v: in Var'Class; n_dim: in Positive) return Positive is
   begin
      return v.size(n_dim);
   end Dimension;

   function Data_Address(v: in Var'Class) return System.Address is
   begin
      return v.data(v.data'First)'Address;
   end Data_Address;

   function Data(v: in Var) return Float_Vec.Real_Vector is
   begin
      return v.data;
   end Data;

   function Element(v: in Var; idx_0, idx_1: in Positive) return Float is
   begin
      if idx_0 = 1 then
         return v.data(idx_1);
      else
         return v.data( (idx_0 - 1) * v.size(2) + idx_1 );
      end if;
   end Element;

   function Flatten(v: in Var'Class) return Var is
      result: Var(1, v.total_size);
   begin
      result.data := v.data;
      result.size(1) := v.total_size;
      return result;
   end Flatten;

   procedure Flatten(v: in out Var'Class) is
   begin
      v.size(1) := v.total_size;
   end Flatten;

   function Dot_Elem_Count(v0, v1: in Var'Class) return Positive is
   begin
      if v0.Dimension_Count = 1 and v1.Dimension_Count = 1 then
         return 1;
      else
         return v0.Dimension(1);
      end if;
   end Dot_Elem_Count;

   function Dot(v0, v1: in Var'Class) return Var is
      result: Var(1, Dot_Elem_Count(v0, v1));
   begin
      result.size(1) := result.total_size;
      for r in 1 .. result.total_size loop
         declare
            row_result: Float := 0.0;
         begin
            for k in 1 .. v1.Dimension(1) loop
               row_result := row_result + v1.Data(k) * v0.Element(r, k);
            end loop;
            result.data(r) := row_result;
         end;
      end loop;
      return result;
   end Dot;

   function "+" (v0, v1: in Var'Class) return Var is
      result: Var(v0.n_dims, v0.total_size);
   begin
      result.size := v0.size;
      result.data := Float_Vec."+"(v0.data, v1.data);
      return result;
   end "+";

   function "-" (v0, v1: in Var'Class) return Var is
      result: Var(v0.n_dims, v0.total_size);
   begin
      result.size := v0.size;
      result.data := Float_Vec."-"(v0.data, v1.data);
      return result;
   end "-";

end Tensor;
