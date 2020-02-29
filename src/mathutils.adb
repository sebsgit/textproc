with Ada.Text_IO;

package body MathUtils is
   function rand01 return Float is
   begin
      return Ada.Numerics.Float_Random.Random(gen);
   end rand01;

   function rand(min, max: Float) return Float is
   begin
      return (max - min)*rand01 + min;
   end rand;

   function mse(a, b: in Vector) return Float is
      result: Float := 0.0;
      ib: Positive := b.First_Index;
   begin
      for x of a loop
         result := result + (x - b(ib)) * (x - b(ib));
         ib := ib + 1;
      end loop;
      return result / Float(a.Length);
   end mse;

   function logLoss(target, predictions: in Vector) return Float is
      result: Float := 0.0;
      tmp: Float;
      ib: Positive := predictions.First_Index;
   begin
      for y of target loop
         tmp := y * F.Log(predictions(ib) + 0.00001) + (1.0 - y) * F.Log(1.00001 - predictions(ib));
         result := result - tmp;
         ib := ib + 1;
      end loop;
      return result / Float(target.Length);
   end logLoss;

   procedure multiply(vec: in out MathUtils.Vector; value: Float) is
   begin
      for x of vec loop
         x := x * value;
      end loop;
   end multiply;

   procedure softmax(vec: in out MathUtils.Vector) is
      total: Float := 0.0;
   begin
      for x of vec loop
         declare
            xp: constant Float := MathUtils.F.Exp(x);
         begin
            x := xp;
            total := total + xp;
         end;
      end loop;
      for x of vec loop
         x := x / total;
      end loop;
   end softmax;

   procedure print(vec: in MathUtils.Vector) is
      x: Float;
   begin
      for i in vec.First_Index .. vec.Last_Index loop
         x := vec(i);
         Ada.Text_IO.Put(x'Image & ", ");
      end loop;
   end print;

begin
   Ada.Numerics.Float_Random.Reset(gen);
end MathUtils;
