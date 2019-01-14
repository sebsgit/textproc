with Ada.Text_IO;

package body PixelArray is
   function allocate(width, height: Natural) return ImagePlane is
      result: ImagePlane;
   begin
      result.width_d := width;
      result.height_d := height;
      result.data.Set_Length(Length => Ada.Containers.Count_Type(width * height));
      return result;
   end allocate;

   function width(img: ImagePlane) return Natural is
   begin
      return img.width_d;
   end width;

   function height(img: ImagePlane) return Natural is
   begin
      return img.height_d;
   end height;

   procedure set(img: out ImagePlane; x, y: Natural; px: Pixel) is
   begin
      img.data.Replace_Element(x + y * img.width, px);
   end set;

   procedure set(img: in out ImagePlane; px: Pixel)
   is
   begin
      for i in 0 .. img.data.Length - 1 loop
         img.data.Replace_Element(Integer(i), px);
      end loop;
   end set;

   function get(img: ImagePlane; x, y: Natural) return Pixel is
   begin
      return img.data.Element(x + y * img.width);
   end get;

   function isInside(image: in ImagePlane; x, y: in Integer) return Boolean is
   begin
      return x < image.width and y < image.height and x >= 0 and y >= 0;
   end isInside;

   function allPixels(img: in ImagePlane; condition: access function(px: Pixel) return Boolean) return Boolean is
   begin
      for y in 0 .. img.height - 1 loop
         for x in 0 .. img.width - 1 loop
            if not condition(img.get(x, y)) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end allPixels;

end PixelArray;
