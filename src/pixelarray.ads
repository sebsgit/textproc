with Ada.Containers.Vectors; use Ada.Containers;

package PixelArray is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Pixel is range 0 .. 255;

   package PixelVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Pixel);

   type PixelData is array (Natural range <>) of Pixel;

   Background: Pixel := 255;

   type ImagePlane is tagged record
      data: PixelVector.Vector;
      width_d, height_d: Natural := 0;
   end record;

   function width(img: ImagePlane) return Natural;

   function height(img: ImagePlane) return Natural;

   function allocate(width, height: Natural) return ImagePlane
     with Post => (width = allocate'Result.width);

   procedure set(img: out ImagePlane; x, y: Natural; px: Pixel)
     with Pre => (x < img.width and y < img.height);

   function get(img: ImagePlane; x, y: Natural) return Pixel
     with Pre => (x < img.width and y < img.height);

   procedure set(img: in out ImagePlane; px: Pixel)
     with Post => img'Old.width = img.width and img'Old.height = img.height;

   function isInside(image: in ImagePlane; x, y: in Integer) return Boolean;

   function allPixels(img: in ImagePlane; condition: access function(px: Pixel) return Boolean) return Boolean
     with Pre => (img.width > 0 and img.height > 0);
end PixelArray;
