with Ada.Numerics.Generic_Elementary_Functions;

package body Histogram is

   package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(Float);
   use Float_Functions;

   function createEmpty(size: Positive) return Data is
      result: Data(size);
   begin
      for i in result.bin'Range loop
         result.bin(i) := 0.0;
      end loop;
      return result;
   end createEmpty;

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
      result: Data := createEmpty(d.size);
      total: Float := d.sum;
   begin
      for i in result.bin'Range loop
         result.bin(i) := d.bin(i) / total;
      end loop;
      return result;
   end normalized;

   procedure normalize(d: in out Data) is
      total: Float := d.sum;
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
      result := createEmpty(size);
      result.set(0, d.get(0));
      result.set(result.size - 1, d.get(d.size - 1));
      scale := Float(d.size - 1) / Float(size - 1);
      for i in 1 .. result.size - 2 loop
         declare
            newCoord: Float := scale * Float(i);
            x0: Float := Float'Floor(newCoord);
            x1: Float := x0 + 1.0;
            weight0: Float := 1.0 - (newCoord - x0);
            weight1: Float := 1.0 - weight0;
            total: Float := weight0 * d.get(Natural(x0)) + weight1 * d.get(Natural(x1));
         begin
            result.set(i, total);
         end;
      end loop;
      return result;
   end resized;

   function compare(d0, d1: Data; method: CompareMethod) return Float is
      result: Float := 0.0;
   begin
      case method is
         when Correlation =>
            declare
               avg0: Float := d0.average;
               avg1: Float := d1.average;
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
               null;
            end;
         when Bhattacharyya =>
            begin
               null;
            end;
      end case;
      return result;
   end compare;

end Histogram;
