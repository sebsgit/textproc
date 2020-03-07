with Interfaces.C;
with System; use System;

package body dl_loader is

   function Load_Wrapper(path: Interfaces.C.char_array) return System.Address
     with Import => True,
     Convention => C,
     External_Name => "dp_load";

   procedure Close_Wrapper(ptr: System.Address)
     with Import => True,
     Convention => C,
     External_Name => "dp_close";

   function Symbol_Wrapper(ptr: System.Address; name: Interfaces.C.char_array) return System.Address
     with Import => True,
     Convention => C,
     External_Name => "dp_symbol";


   function Open(path: in String; h: in out Handle) return Boolean is
   begin
      if Is_Valid(h) then
         Close(h);
      end if;
      h.h := Load_Wrapper(Interfaces.C.To_C(path));
      return Is_Valid(h);
   end Open;

   function Is_Valid(h: in Handle) return Boolean is
   begin
      return h.h /= System.Null_Address;
   end Is_Valid;

   function Get_Symbol(h: in Handle; name: String) return System.Address is
   begin
      return Symbol_Wrapper(h.h, Interfaces.C.To_C(name));
   end Get_Symbol;

   procedure Close(h: in out Handle) is
   begin
      if h.h /= System.Null_Address then
         Close_Wrapper(h.h);
         h.h := System.Null_Address;
      end if;
   end Close;
end dl_loader;
