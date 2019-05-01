with PixelArray;

package Histogram is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type CompareMethod is (Correlation, ChiSquare, Bhattacharyya);

   type Bins is array (Positive range<>) of Float;

   type Data (Size: Positive) is tagged record
      bin: Bins(1 .. Size);
   end record;

   function createEmpty(size: Positive) return Data
     with Post => (createEmpty'Result.size = size and createEmpty'Result.sum = 0.0);

   procedure set(d: out Data; i: Natural; value: Float)
     with Pre => (i < d.size);

   function get(d: Data; i: Natural) return Float
     with Pre => (i < d.size);

   function size(d: Data) return Natural;

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
       Pre => d0.size = d1.size;

   function add(d0, d1: Data) return Data
     with
       Pre => d0.size = d1.size,
       Post => add'Result.size = d1.size and (for all i in 0 .. d1.size -1 => add'Result.get(i) = d0.get(i) + d1.get(i));

   procedure print(d: Data);

end Histogram;
