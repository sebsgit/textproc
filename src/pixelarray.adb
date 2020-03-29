with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body PixelArray is
   procedure Free_Pixel_Buffer is new Ada.Unchecked_Deallocation(Object => Pixel_Buffer,
                                                                 Name   => Pixel_Buffer_Access);

   function allocate(width, height: Natural) return ImagePlane is
   begin
      return result: ImagePlane do
         result.width_d := width;
         result.height_d := height;
         result.data := new Pixel_Buffer(0 .. width * height);
      end return;
   end allocate;

   procedure assign(This: in out ImagePlane; other: in ImagePlane) is
   begin
      Free_Pixel_Buffer(This.data);
      This.data := new Pixel_Buffer(0 .. other.width_d * other.height_d);
      This.data.all := other.data.all;
      This.width_d := other.width_d;
      This.height_d := other.height_d;
   end assign;

   procedure Finalize(This: in out ImagePlane) is
   begin
      if This.data /= null then
         Free_Pixel_Buffer(This.data);
      end if;
   end Finalize;

   function width(img: ImagePlane) return Natural is
   begin
      return img.width_d;
   end width;

   function height(img: ImagePlane) return Natural is
   begin
      return img.height_d;
   end height;

   procedure set(img: in out ImagePlane; x, y: Natural; px: Pixel) is
   begin
      img.data.all(x + y * img.width) := Interfaces.C.unsigned_char(px);
   end set;

   procedure set(img: in out ImagePlane; px: Pixel)
   is
   begin
      for i in 0 .. img.width * img.height loop
         img.data.all(i) := Interfaces.C.unsigned_char(px);
      end loop;
   end set;

   function get(img: ImagePlane; x, y: Natural) return Pixel is
   begin
      --Ada.Text_IO.Put_Line("get: " & x'Image & " " & y'Image & " " & img.width'Image & " " & img.height'Image);
      return Pixel(img.data.all(x + y * img.width));
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

   function rescale(img: in ImagePlane; w, h: in Positive) return ImagePlane is
      function sample(x, y: in Natural) return Pixel is
         nx: Natural;
         ny: Natural;
      begin
         nx := (x * img.width) / w;
         ny := (y * img.height) / h;
         return img.get(nx, ny);
      end sample;

   begin
      return result: ImagePlane := allocate(width  => w,
                                            height => h) do
         for y in 0 .. h - 1 loop
            for x in 0 .. w - 1 loop
               result.set(x  => x,
                          y  => y,
                          px => sample(x, y));
            end loop;
         end loop;
      end return;
   end;

   function expand(img: in ImagePlane; w_margin, h_margin: in Positive; color: Pixel) return ImagePlane is
   begin
      return result: ImagePlane := allocate(img.width + 2 * w_margin, img.height + 2 * h_margin) do
         result.set(color);
         for y in h_margin .. img.height + h_margin - 1 loop
            for x in w_margin .. img.width + w_margin - 1 loop
               result.set(x, y, img.get(x - w_margin, y - h_margin));
            end loop;
         end loop;
      end return;
   end expand;


   function cut(img: in ImagePlane; x, y: in Natural; w, h: in Positive) return ImagePlane is
      xr: Natural := 0;
      yr: Natural := 0;
   begin
      return result: ImagePlane := allocate(width  => w,
                                            height => h) do
         for yi in y .. y + h - 1 loop
            xr := 0;
            for xi in x .. x + w - 1 loop
               result.set(x  => xr,
                          y  => yr,
                          px => img.get(xi, yi));
               xr := xr + 1;
            end loop;
            yr := yr + 1;
         end loop;
      end return;
   end cut;

end PixelArray;
