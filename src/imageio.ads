with Interfaces.C;
with Interfaces.C.Strings;

with PixelArray;
with StbiWrapper;

package ImageIO is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   package C renames Interfaces.C;

   function load(filename: C.Strings.chars_ptr) return PixelArray.ImagePlane;
   function save(filename: C.Strings.chars_ptr; image: PixelArray.ImagePlane) return Boolean;
end ImageIO;
