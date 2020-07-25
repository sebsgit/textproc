with MathUtils;

package body Tensor is
   function Element_Count(d: in Var'Class) return Positive is
   begin
      return Positive(d.data.Length);
   end Element_Count;

   function Allocate(value_count: in Positive) return Var is
   begin
      return r: Var do
         r.size.Append(value_count);
         r.data.Set_Length(Ada.Containers.Count_Type(value_count));
      end return;
   end Allocate;

   function Variable(values: in Float_Array) return Var is
      result: Var;
      idx_out: Natural := 1;
   begin
      result.size.Set_Length(1);
      result.data.Set_Length(values'Length);
      result.size(1) := values'Length;
      for idx_in in values'First .. values'Last loop
         result.data(idx_out) := values(idx_in);
         idx_out := idx_out + 1;
      end loop;
      return result;
   end Variable;

   function Matrix(row_length: in Positive; values: in Float_Array) return Var is
      result: Var;
      idx_out: Natural := 1;
   begin
      result.size.Set_Length(2);
      result.data.Set_Length(values'Length);
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
      return Positive(v.size.Length);
   end Dimension_Count;

   function Dimension(v: in Var'Class; n_dim: in Positive) return Positive is
   begin
      return v.size(n_dim);
   end Dimension;

   function Data(v: in Var) return MathUtils.Vector is
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

   procedure Set(v: out Var; values: in Float_Array) is
      idx_out: Natural := 1;
   begin
      v.data.Set_Length(values'Length);
      for idx_in in values'First .. values'Last loop
         v.data(idx_out) := values(idx_in);
         idx_out := idx_out + 1;
      end loop;
   end Set;

   procedure Set(v: out Var; idx: Positive; value: in Float) is
   begin
      v.data(idx) := value;
   end Set;

   function Flatten(v: in Var'Class) return Var is
      result: Var;
   begin
      result.data := v.data;
      result.size.Append(Element_Count(v));
      return result;
   end Flatten;

   procedure Flatten(v: in out Var'Class) is
   begin
      v.size.Clear;
      v.size.Append(Positive(v.data.Length));
   end Flatten;

   procedure Reshape(v: in out Var'Class; s0, s1: Positive) is
   begin
      v.size.Clear;
      v.size.Append(s0);
      v.size.Append(s1);
   end Reshape;

   procedure Random(v: in out Var'Class) is
   begin
      for i in v.data.First_Index .. v.data.Last_Index loop
         v.data.Replace_Element(i, MathUtils.rand01);
      end loop;
   end Random;

   function Dot_Elem_Count(v0, v1: in Var'Class) return Positive is
   begin
      if v0.Dimension_Count = 1 and v1.Dimension_Count = 1 then
         return 1;
      else
         return v0.Dimension(1);
      end if;
   end Dot_Elem_Count;

   function Dot(v0, v1: in Var'Class) return Var is
      result: Var;
   begin
      result.size.Append(Dot_Elem_Count(v0, v1));
      result.data.Set_Length(Ada.Containers.Count_Type(Dot_Elem_Count(v0, v1)));
      for r in 1 .. result.size.Element(1) loop
         declare
            row_result: Float := 0.0;
         begin
            for k in 1 .. v1.Dimension(1) loop
               row_result := row_result + v1.Data(k) * v0.Element(r, k);
            end loop;
            result.data.Replace_Element(r, row_result);
         end;
      end loop;
      return result;
   end Dot;

   function "+" (v0, v1: in Var'Class) return Var is
      result: Var;
   begin
      result.size := v0.size;
      result.data := v0.data;
      declare
         idx_0: Positive := result.data.First_Index;
      begin
         for val of v1.data loop
            result.data.Replace_Element(idx_0, result.data.Element(idx_0) + val);
            idx_0 := idx_0 + 1;
         end loop;
      end;
      return result;
   end "+";

   function "-" (v0, v1: in Var'Class) return Var is
      result: Var;
   begin
      result.size := v0.size;
      result.data := v0.data;
      declare
         idx_0: Positive := result.data.First_Index;
      begin
         for val of v1.data loop
            result.data.Replace_Element(idx_0, result.data.Element(idx_0) - val);
            idx_0 := idx_0 + 1;
         end loop;
      end;
      return result;
   end "-";

   procedure Apply(v: in out Var; fn: Lambda_Func) is
   begin
      for idx in v.data.First_Index .. v.data.Last_Index loop
         declare
            new_val: constant Float := fn(v.data(idx));
         begin
            v.data.Replace_Element(idx, new_val);
         end;
      end loop;
   end Apply;

end Tensor;
