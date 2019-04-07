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

   function compare(d0, d1: Data; method: CompareMethod) return Float
     with
       Pre => d0.size = d1.size;

   procedure print(d: Data);

end Histogram;
