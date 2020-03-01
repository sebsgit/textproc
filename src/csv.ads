with Ada.Text_IO;
with Ada.Strings.Unbounded;

with DataBatch;
with MathUtils;

package CSV is
   type Reader is tagged limited private;

   function open(path: in String; separator: in Character := ',') return Reader;
   function hasNext(r: in Reader) return Boolean;
   function next(r: in out Reader) return MathUtils.Vector;

private
   type Reader is tagged limited record
      handle: Ada.Text_IO.File_Type;
      nextLine: Ada.Strings.Unbounded.Unbounded_String;
      hasMore: Boolean := False;
   end record;
end CSV;
