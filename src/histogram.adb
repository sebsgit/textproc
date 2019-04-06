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
      d.bin(i + 1) := value;
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

end Histogram;
