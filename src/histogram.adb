package body Histogram is

   function createEmpty(size: Natural) return Data is
      result: Data(size);
   begin
      for i in result.bin'Range loop
         result.bin(i) := 0.0;
      end loop;
      return result;
   end createEmpty;

   procedure set(d: out Data; i:Natural; value: Float) is
   begin
      d.bin(i) := value;
   end set;

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

   function normalize(d: Data) return Data is
      result: Data := createEmpty(d.size);
      total: Float := d.sum;
   begin
      for i in result.bin'Range loop
         result.set(i, d.bin(i) / total);
      end loop;
      return result;
   end normalize;

end Histogram;
