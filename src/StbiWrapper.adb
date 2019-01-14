package body StbiWrapper is
   function load(filename: C.Strings.chars_ptr; x: IntPtr.Pointer; y: IntPtr.Pointer; channels_in_file: IntPtr.Pointer; desired_channels: C.int) return UCharPtr.Pointer
     with
       import => True,
       Convention => C,
       External_Name => "stbi_load";

   procedure free(ptr: UCharPtr.Pointer)
     with
       Import => True,
       Convention => C,
       External_Name => "stbi_image_free";

   function load(filename: C.Strings.chars_ptr; desired_channels: C.int := 3) return ImageData
   is
      result: ImageData;
      x, y, channels: aliased C.int;
   begin
      result.pixels := load(filename, x'Unchecked_Access, y'Unchecked_Access, channels'Unchecked_Access, desired_channels);
      result.width := x;
      result.height := y;
      result.nChannels := channels;
      return result;
   end load;

   procedure free(data: ImageData) is
   begin
      free(data.pixels);
   end free;

   function check(data: ImageData) return Boolean
   is
   begin
      return data.width > 0 and data.height > 0 and data.nChannels in 1 .. 4;
   end check;

end StbiWrapper;
