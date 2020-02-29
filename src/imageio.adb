with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with StbiWrapper; use StbiWrapper.UCharPtr;

with Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;

package body ImageIO is
   package C renames Interfaces.C;

   function load(filename: C.Strings.chars_ptr) return PixelArray.ImagePlane
   is
      imageData: StbiWrapper.ImageData;
   begin
      imageData := StbiWrapper.load(filename);
      declare
         result: PixelArray.ImagePlane := PixelArray.allocate(Integer(imageData.width), Integer(imageData.height));
      begin
         if imageData.nChannels = 1 then
            for y in 0 .. imageData.height - 1 loop
               for x in 0 .. imageData.width - 1 loop
                  declare
                     ptr: constant StbiWrapper.UCharPtr.Pointer := imageData.pixels + C.ptrdiff_t(x + y * imageData.width);
                  begin
                     result.set(Integer(x), Integer(y), PixelArray.Pixel(ptr.all));
                  end;
               end loop;
            end loop;
         else
            for y in 0 .. imageData.height - 1 loop
               for x in 0 .. imageData.width - 1 loop
                  declare
                     ptrR: constant StbiWrapper.UCharPtr.Pointer := imageData.pixels + C.ptrdiff_t(3 * x + 0 + y * 3 * imageData.width);
                     ptrG: constant StbiWrapper.UCharPtr.Pointer := imageData.pixels + C.ptrdiff_t(3 * x + 1 + y * 3 * imageData.width);
                     ptrB: constant StbiWrapper.UCharPtr.Pointer := imageData.pixels + C.ptrdiff_t(3 * x + 2 + y * 3 * imageData.width);
                     avgPx: constant Float := (Float(ptrR.all) + Float(ptrG.all) + Float(ptrB.all)) / 3.0;
                  begin
                     result.set(Integer(x), Integer(y), PixelArray.Pixel(avgPx));
                  end;
               end loop;
            end loop;
         end if;
         return result;
      end;
   end load;

   function save(filename: C.Strings.chars_ptr; image: PixelArray.ImagePlane) return Boolean
   is
   begin
      declare
         arraySize: constant C.size_t := C.size_t(image.width * image.height * 3);
         pxArray: StbiWrapper.UCharArray(0 .. arraySize);
         save_result: C.int;
         index: Integer := 0;
      begin
         for y in 0 .. image.height - 1 loop
            for x in 0 .. image.width - 1 loop
               declare
                  charValue: constant C.unsigned_char := C.unsigned_char(image.get(x, y));
               begin
                  pxArray(C.size_t(3 * index)) := charValue;
                  pxArray(C.size_t(3 * index + 1)) := charValue;
                  pxArray(C.size_t(3 * index + 2)) := charValue;
                  index := index + 1;
               end;
            end loop;
         end loop;

         save_result := StbiWrapper.writePng(filename, C.int(image.width), C.int(image.height), 3, pxArray(pxArray'First)'Unchecked_Access, C.int(3 * image.width));
         return save_result = 1;
      end;
   end save;

   function load(filename: String) return PixelArray.ImagePlane is
   begin
      return load(C.Strings.New_String(filename));
   end load;

   function save(filename: String; image: PixelArray.ImagePlane) return Boolean is
   begin
      return save(C.Strings.New_String(filename), image);
   end save;

end ImageIO;
