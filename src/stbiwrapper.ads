with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package StbiWrapper is

   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   package C renames Interfaces.C;

   type IntArray is array (C.size_t range <>) of aliased C.int;
   type UCharArray is array (C.size_t range <>) of aliased C.unsigned_char;

   package IntPtr is
     new C.Pointers (Index => C.size_t,
                     Element => C.int,
                     Element_Array => IntArray,
                     Default_Terminator => C.int(0));

   package UCharPtr is
     new C.Pointers (Index => C.size_t,
                     Element => C.unsigned_char,
                     Element_Array => UCharArray,
                     Default_Terminator => C.unsigned_char(0));

   use IntPtr;
   use UCharPtr;
   --extern unsigned char *stbi_load (char const *filename, int *x, int *y, int *channels_in_file, int desired_channels);
   --extern int stbi_write_png(char const *filename, int x, int y, int comp, const void *data, int stride_bytes)
   --extern void free(unsigned char*)

   function writePng(filename: C.Strings.chars_ptr; width, height, compression: C.int; data: UCharPtr.Pointer; stride_in_bytes: C.int) return C.int
     with
       import => True,
       Convention => C,
       External_Name => "stbi_write_png",
       Pre => (width > 0 and height > 0 and stride_in_bytes >= width and data /= null),
     Post => (writePng'Result in 0 .. 1);

   type ImageData is record
      pixels: UCharPtr.Pointer;
      width, height, nChannels: C.int;
   end record;

   function load(filename: C.Strings.chars_ptr; desired_channels: C.int := 3) return ImageData
     with
       Pre => (desired_channels in 1 .. 4),
     Post => check(load'Result);
   procedure free(data: ImageData);
   function check(data: ImageData) return Boolean;

end StbiWrapper;
