with Ada.Numerics.Float_Random;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Numerics.Generic_Elementary_Functions;

package MathUtils is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   package Float_Vec is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                   Element_Type => Float);
   package F is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => Float);

   subtype Vector is Float_Vec.Vector;

   function rand01 return Float;
   function rand(min, max: Float) return Float
     with Pre => min < max;

   function mse(a, b: in Vector) return Float
     with Pre => a.Length = b.Length and b.Length > 0;
   function logLoss(target, predictions: in Vector) return Float
     with Pre => target.Length = predictions.Length and target.Length > 0;

   procedure multiply(vec: in out Vector; value: Float);
   procedure softmax(vec: in out Vector)
     with Pre => vec.Length /= 0;

   procedure print(vec: in Vector);

private
   gen: Ada.Numerics.Float_Random.Generator;
end MathUtils;
