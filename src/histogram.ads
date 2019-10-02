with PixelArray;
with Ada.Strings.Unbounded;

package Histogram
with SPARK_Mode => On
is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type CompareMethod is (Correlation, ChiSquare, Bhattacharyya);

   type Bins is array (Positive range<>) of Float;

   type Data (Size: Positive) is tagged record
      bin: Bins(1 .. Size) := (others => 0.0);
   end record;

   procedure set(d: out Data; i: Natural; value: Float)
     with Pre'Class => (i < d.size),
     Inline;

   function get(d: Data; i: Natural) return Float
     with Pre'Class => (i < d.size),
     Inline;

   function size(d: Data) return Natural
     with Inline;

   function sum(d: Data) return Float;

   function average(d: Data) return Float;

   function normalized(d: Data) return Data
     with Post => (normalized'Result.size = d.size);

   procedure normalize(d: in out Data);

   function resized(d: Data; size: Positive) return Data
     with Post => resized'Result.size = size;

   procedure multiply(d: in out Data; value: Float)
     with Post => (for all i in 0 .. d.size - 1 => d.get(i) = d'Old.get(i) * value);

   function multiplied(d: Data; value: Float) return Data
     with Post => d.size = multiplied'Result.size;

   function compare(d0, d1: Data; method: CompareMethod) return Float
     with
       Pre'Class => d0.size = d1.size;

   function add(d0, d1: Data) return Data
     with
       Pre'Class => d0.size = d1.size,
       Post => add'Result.size = d1.size and (for all i in 0 .. d1.size -1 => add'Result.get(i) = d0.get(i) + d1.get(i));

   function toString(d: Data) return Ada.Strings.Unbounded.Unbounded_String;

end Histogram;
