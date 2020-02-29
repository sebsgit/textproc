with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with DataBatch;
with MathUtils;

use MathUtils.Float_Vec;

package body DataBatchTests is
   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testBasicBatch'Access, "batch");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Data Batch Tests");
   end Name;

   procedure testBasicBatch(T : in out Test_Cases.Test_Case'Class) is
      b: DataBatch.Batch;
      vec: MathUtils.Vector;
   begin
      Assert(b.size = 0, "size 0");

      b.reserve(10);
      Assert(b.size = 0, "size 0");

      b.append(1.0 & 1.0 & 1.0);
      Assert(b.size = 1, "size 1");

      b.append(2.0 & 2.0 & 2.0);
      b.append(3.0 & 3.0 & 3.0);
      b.append(4.0 & 4.0 & 4.0);
      Assert(b.size = 4, "size 4");

      b.randomize;

      Assert(b.contains(1.0 & 1.0 & 1.0), "");
      Assert(b.contains(2.0 & 2.0 & 2.0), "");
      Assert(b.contains(3.0 & 3.0 & 3.0), "");
      Assert(b.contains(4.0 & 4.0 & 4.0), "");

   end testBasicBatch;
end DataBatchTests;
