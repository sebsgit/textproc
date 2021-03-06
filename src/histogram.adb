with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Histogram
with SPARK_Mode => On
is

   package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(Float);
   use Float_Functions;

   procedure set(d: out Data; i:Natural; value: Float) is
   begin
      d.bin(i + 1) := value;
   end set;

   function get(d: Data; i: Natural) return Float is
   begin
      return d.bin(i + 1);
   end get;

   function size(d: Data) return Natural is
   begin
      return d.bin'Length;
   end size;

   function sum(d: Data) return Float is
      result: Float := 0.0;
   begin
      for i in d.bin'Range loop
         result := result + d.bin(i);
      end loop;
      return result;
   end sum;

   function average(d: Data) return Float is
   begin
      return d.sum / Float(d.size);
   end average;

   function normalized(d: Data) return Data is
      result: Data(d.size);
      total: constant Float := d.sum;
   begin
      for i in result.bin'Range loop
         result.bin(i) := d.bin(i) / total;
      end loop;
      return result;
   end normalized;

   procedure normalize(d: in out Data) is
      total: constant Float := d.sum;
   begin
      if total /= 0.0 then
         for i in d.bin'Range loop
            d.bin(i) := d.bin(i) / total;
         end loop;
      end if;
   end normalize;

   function resized(d: Data; size: Positive) return Data is
      result: Data(size);
      scale: Float;
   begin
      if d.size = size then
         return d;
      end if;
      result.set(0, d.get(0));
      result.set(result.size - 1, d.get(d.size - 1));
      scale := Float(d.size - 1) / Float(size - 1);
      for i in 1 .. result.size - 2 loop
         declare
            newCoord: constant Float := scale * Float(i);
            x0: constant Float := Float'Floor(newCoord);
            x1: constant Float := x0 + 1.0;
            weight0: constant Float := 1.0 - (newCoord - x0);
            weight1: constant Float := 1.0 - weight0;
            total: constant Float := weight0 * d.get(Natural(x0)) + weight1 * d.get(Natural(x1));
         begin
            result.set(i, total);
         end;
      end loop;
      return result;
   end resized;

   procedure multiply(d: in out Data; value: Float) is
   begin
      for i in 0 .. d.size - 1 loop
         d.set(i, d.get(i) * value);
      end loop;
   end multiply;

   function multiplied(d: Data; value: Float) return Data is
      result: Data := (size => d.size, bin => d.bin);
   begin
      for i in 0 .. d.size - 1 loop
         result.set(i, d.get(i) * value);
      end loop;
      return result;
   end multiplied;

   function compare(d0, d1: Data; method: CompareMethod) return Float
   is
      result: Float := 0.0;
      avg0: constant Float := d0.average;
      avg1: constant Float := d1.average;
   begin
      case method is
         when Correlation =>
            declare
               a: Float := 0.0;
               b: Float := 0.0;
               c: Float := 0.0;
            begin
               for i in d0.bin'Range loop
                  a := a + (d0.bin(i) - avg0) * (d1.bin(i) - avg1);
                  b := b + (d0.bin(i) - avg0) ** 2;
                  c := c + (d1.bin(i) - avg1) ** 2;
               end loop;
               if b = 0.0 and c = 0.0 then
                  result := 0.0;
               elsif b = 0.0 then
                  result := c;
               elsif c = 0.0 then
                  result := b;
               else
                  result := a / Sqrt(b * c);
               end if;
            end;
         when ChiSquare =>
            begin
               for i in d0.bin'Range loop
                  if d0.bin(i) = 0.0 then
                     result := result + d1.bin(i);
                  else
                     result := result + Float((d0.bin(i) - d1.bin(i)) * (d0.bin(i) - d1.bin(i))) / Float(d0.bin(i));
                  end if;
               end loop;
            end;
         when Bhattacharyya =>
            declare
               a: Float := 0.0;
            begin
               if avg0 = 0.0 and avg1 = 0.0 then
                  result := 0.0;
               elsif avg1 = 0.0 then
                  result := 1.0;
               elsif avg0 = 0.0 then
                  result := 1.0;
               else
                  for i in d0.bin'Range loop
                     a := a + Sqrt(d0.bin(i) * d1.bin(i));
                  end loop;
                  a := a / Sqrt(avg1 * avg0 * Float(d0.size ** 2));
                  if a > 1.0 then
                     a := 1.0;
                  end if;
                  result := Sqrt(1.0 - a);
               end if;
            end;
      end case;
      return result;
   end compare;

   function add(d0, d1: Data) return Data is
      result: Data(d0.size);
   begin
      for i in 0 .. d0.size - 1 loop
         result.set(i, d0.get(i) + d1.get(i));
      end loop;
      return result;
   end add;

   function toString(d: Data) return Ada.Strings.Unbounded.Unbounded_String is
      result: Ada.Strings.Unbounded.Unbounded_String;
   begin
      result := Ada.Strings.Unbounded.To_Unbounded_String("[");
      for i in 0 .. d.size - 2 loop
         result := result & Float(d.get(i))'Image & ", ";
      end loop;
      result := result & Float(d.get(d.size - 1))'Image & "]";
      return result;
   end toString;

end Histogram;
