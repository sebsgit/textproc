with PixelArray;
with StbiWrapper;

package ImageIO is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   function load(filename: String) return PixelArray.ImagePlane;
   function save(filename: String; image: PixelArray.ImagePlane) return Boolean;
   procedure save(filename: String; image: PixelArray.ImagePlane);
end ImageIO;
