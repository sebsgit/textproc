with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Finalization;
with Interfaces.C;
with Interfaces.C.Pointers;

package PixelArray is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Pixel is range 0 .. 255;

   Background: constant Pixel := 255;

   type ImagePlane is tagged limited private;

   function width(img: ImagePlane) return Natural
     with Inline;

   function height(img: ImagePlane) return Natural
     with Inline;

   function allocate(width, height: Natural) return ImagePlane
     with Post => (width = allocate'Result.width);

   procedure assign(This: in out ImagePlane; other: in ImagePlane)
     with Inline;

   procedure set(img: in out ImagePlane; x, y: Natural; px: Pixel)
     with Pre => (x < img.width and y < img.height),
     Inline;

   function get(img: ImagePlane; x, y: Natural) return Pixel
     with Pre => (x < img.width and y < img.height),
     Inline;

   procedure set(img: in out ImagePlane; px: Pixel)
     with Inline;

   function isInside(image: in ImagePlane; x, y: in Integer) return Boolean
     with Inline;

   function allPixels(img: in ImagePlane; condition: access function(px: Pixel) return Boolean) return Boolean
     with Pre => (img.width > 0 and img.height > 0);

   function rescale(img: in ImagePlane; w, h: in Positive) return ImagePlane
     with Post => rescale'Result.width = w and rescale'Result.height = h;

   function expand(img: in ImagePlane; w_margin, h_margin: in Positive; color: Pixel) return ImagePlane
     with Post => expand'Result.width = 2 * w_margin + img.width and expand'Result.height = 2 * h_margin + img.height;

   function cut(img: in ImagePlane; x, y: in Natural; w, h: in Positive) return ImagePlane
     with Pre => (x < img.width and y < img.height) and (w <= img.width and h <= img.height),
     Post => cut'Result.width = w and cut'Result.height = h,
     Inline;

private
   type Pixel_Buffer is array (Natural range <>) of aliased Interfaces.C.unsigned_char;
   type Pixel_Buffer_Access is access Pixel_Buffer;

   type ImagePlane is limited new Ada.Finalization.Limited_Controlled with record
      data: aliased Pixel_Buffer_Access;
      width_d, height_d: Natural := 0;
   end record;

   overriding procedure Finalize(This: in out ImagePlane)
     with Inline;

end PixelArray;
