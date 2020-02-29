with MathUtils;

package body DataBatch is

   function size(b: in Batch) return Natural is
   begin
      return Natural(b.data.Length);
   end size;

   procedure randomize(b: in out Batch) is
      steps: constant Natural := b.size * 2;
      i1, i2: Positive;
   begin
      for i in 0 .. steps loop
         i1 := Positive(MathUtils.rand(Float(b.data.First_Index), Float(b.data.Last_Index)));
         i2 := Positive(MathUtils.rand(Float(b.data.First_Index), Float(b.data.Last_Index)));
         b.data.Swap(i1, i2);
      end loop;
   end randomize;

   procedure reserve(b: in out Batch; count: Positive) is
   begin
      b.data.Reserve_Capacity(Capacity => Ada.Containers.Count_Type(count));
   end reserve;

   procedure append(b: in out Batch; vec: MathUtils.Vector) is
   begin
      b.data.Append(vec);
   end append;

   function contains(b: in Batch; vec: in MathUtils.Vector) return Boolean is
   begin
      return b.data.Contains(vec);
   end contains;

end DataBatch;
