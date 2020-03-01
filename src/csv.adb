with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package body CSV is
   function open(path: in String; separator: in Character := ',') return Reader is
   begin
      return result: Reader do
         Ada.Text_IO.Open(File => result.handle,
                          Mode => Ada.Text_IO.In_File,
                          Name => path);
         if Ada.Text_IO.Is_Open(File => result.handle) then
            result.nextLine := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line(result.handle));
            result.hasMore := Ada.Strings.Unbounded.Length(result.nextLine) > 0;
         else
            result.hasMore := False;
         end if;
      end return;
   end open;

   function hasNext(r: in Reader) return Boolean is
   begin
      return r.hasMore;
   end hasNext;

   function split(s: in Ada.Strings.Unbounded.Unbounded_String) return MathUtils.Vector is
      result: MathUtils.Vector;
      nextIndex: Natural := 1;
      startIndex: Natural := 1;
   begin
      while nextIndex < Ada.Strings.Unbounded.Length(s) loop
         nextIndex := Ada.Strings.Unbounded.Index(Source  => s,
                                                  Pattern => ",",
                                                  From    => nextIndex + 1);
         if nextIndex = 0 then
            declare
               subs: constant String := Ada.Strings.Unbounded.Slice(Source => s,
                                                                    Low    => startIndex,
                                                                    High   => Ada.Strings.Unbounded.Length(s));
            begin
               result.Append(Float'Value(subs));
            end;
            exit;
         end if;
         declare
            subs: constant String := Ada.Strings.Unbounded.Slice(Source => s,
                                                                 Low    => startIndex,
                                                                 High   => nextIndex - 1);
         begin
            result.Append(Float'Value(subs));
         end;
         startIndex := nextIndex + 1;
      end loop;
      return result;
   end split;


   function next(r: in out Reader) return MathUtils.Vector is
      result: MathUtils.Vector;
   begin
      if r.hasMore then
         result := split(r.nextLine);
         if Ada.Text_IO.End_Of_Line(File => r.handle) = False then
            r.nextLine := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line(r.handle));
            r.hasMore :=  Ada.Strings.Unbounded.Length(r.nextLine) > 0;
         else
            r.hasMore := False;
         end if;
      end if;
      return result;
   end next;
end CSV;
