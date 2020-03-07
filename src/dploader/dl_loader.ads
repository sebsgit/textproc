with Ada.Strings;
with Interfaces.C;
with System;

package dl_loader is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Handle is limited private;

   function Open(path: in String; h: in out Handle) return Boolean;
   function Is_Valid(h: in Handle) return Boolean;
   function Get_Symbol(h: in Handle; name: in String) return System.Address;
   procedure Close(h: in out Handle)
     with Post => Is_Valid(h) = False;

private
   type Handle is limited record
      h: System.Address := System.Null_Address;
   end record;

end dl_loader;
