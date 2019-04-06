with PixelArray;

package Histogram is
   type Bins is array (Natural range<>) of Float;

   type Data (Size: Natural) is tagged record
      bin: Bins(1 .. Size);
   end record;

   function createEmpty(size: Natural) return Data;
   procedure set(d: out Data; i: Natural; value: Float);
   function size(d: Data) return Natural;
   function sum(d: Data) return Float;
   function normalize(d: Data) return Data;
end Histogram;
