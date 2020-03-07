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

   function rescale(img: in ImagePlane; w, h: in Positive) return ImagePlane is
      result: ImagePlane;

      function sample(x, y: in Natural) return Pixel is
         nx: Natural;
         ny: Natural;
      begin
         nx := (x * img.width) / w;
         ny := (y * img.height) / h;
         return img.get(nx, ny);
      end sample;

   begin
      result := allocate(width  => w,
                         height => h);
      for y in 0 .. h - 1 loop
         for x in 0 .. w - 1 loop
            result.set(x  => x,
                       y  => y,
                       px => sample(x, y));
         end loop;
      end loop;
      return result;
   end;

   function expand(img: in ImagePlane; w_margin, h_margin: in Positive; color: Pixel) return ImagePlane is
      result: ImagePlane;
   begin
      result := allocate(img.width + 2 * w_margin, img.height + 2 * h_margin);
      result.set(color);
      for y in h_margin .. img.height + h_margin - 1 loop
         for x in w_margin .. img.width + w_margin - 1 loop
            result.set(x, y, img.get(x - w_margin, y - h_margin));
         end loop;
      end loop;
      return result;
   end expand;


   function cut(img: in ImagePlane; x, y: in Natural; w, h: in Positive) return ImagePlane is
      result: ImagePlane;
      xr: Natural := 0;
      yr: Natural := 0;
   begin
      result := allocate(width  => w,
                         height => h);
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
      return result;
   end cut;


end PixelArray;
