with PixelArray; use PixelArray;
with ImageRegions;

with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Containers.Generic_Sort;

package body ImageMoments is
   package FloatFunctions is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => Float);
   use FloatFunctions;

   generic
      with function op(p1, p2: Float) return Float;
   function match_helper(ha, hb: HuMoments) return Float;

   function match_helper(ha, hb: HuMoments) return Float is
   begin
      return op(ha.h1, hb.h1) + op(ha.h2, hb.h2) + op(ha.h3, hb.h3) + op(ha.h4, hb.h4) + op(ha.h5, hb.h5) + op(ha.h6, hb.h6) + op(ha.h7, hb.h7);
   end match_helper;

   function match_I1(a, b: HuMoments) return Float is
      function optmp(p1, p2: Float) return Float is (abs(1.0 / p1 - 1.0 / p2));
      function tmp is new match_helper(op => optmp);
   begin
      return tmp(a, b);
   end match_I1;

   function match_I2(a, b: HuMoments) return Float is
      function optmp(p1, p2: Float) return Float is (abs(p1 - p2));
      function tmp is new match_helper(op => optmp);
   begin
      return tmp(a, b);
   end match_I2;

   function match_I3(a, b: HuMoments) return Float is
      function optmp(p1, p2: Float) return Float is (abs((p1 - p2) / p1));
      function tmp is new match_helper(op => optmp);
   begin
      return tmp(a, b);
   end match_I3;

   function match_rms(a, b: HuMoments) return Float is
      function optmp(p1, p2: Float) return Float is ((p1 - p2) ** 2);
      function tmp is new match_helper(op => optmp);
   begin
      return Sqrt(tmp(a, b));
   end match_rms;

   function moment(image: PixelArray.ImagePlane; region: ImageRegions.Region; p, q: Natural) return Float is
      result: Float;
   begin
      result := 0.0;
      for py in region.area.y .. region.area.y + region.area.height loop
         for px in region.area.x .. region.area.x + region.area.width loop
            if PixelArray.Pixel(region.label) = image.get(px, py) then
               result := result + Float((px - region.area.x) ** p) * Float((py - region.area.y) ** q);
            end if;
         end loop;
      end loop;
      return result;
   end moment;

   function centralMoment(image: PixelArray.ImagePlane; region: ImageRegions.Region; p, q: Natural) return Float is
      result: Float;
      cx, cy: Float;
   begin
      result := 0.0;
      cx := moment(image, region, 1, 0) / Float(region.pixelCount);
      cy := moment(image, region, 0, 1) / Float(region.pixelCount);
      for py in region.area.y .. region.area.y + region.area.height loop
         for px in region.area.x .. region.area.x + region.area.width loop
            if PixelArray.Pixel(region.label) = image.get(px, py) then
               result := result + Float((Float(px - region.area.x) - cx) ** p) * Float((Float(py - region.area.y) - cy) ** q);
            end if;
         end loop;
      end loop;
      return result;
   end centralMoment;

   function normalCentralMoment(image: PixelArray.ImagePlane; region: ImageRegions.Region; p, q: Natural) return Float is
      norm: Float;
   begin
      norm := Float(region.pixelCount) ** ((p + q + 2) / 2);
      return centralMoment(image, region, p, q) / norm;
   end normalCentralMoment;

   function getH1(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float is
   begin
      return normalCentralMoment(image, region, 2, 0) + normalCentralMoment(image, region, 0, 2);
   end getH1;

   function getH2(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float is
   begin
      return (normalCentralMoment(image, region, 2, 0) - normalCentralMoment(image, region, 0, 2)) ** 2 + 4.0 * (normalCentralMoment(image, region, 1, 1) ** 2);
   end getH2;

   function getH3(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float is
   begin
      return (normalCentralMoment(image, region, 3, 0) - 3.0 * normalCentralMoment(image, region, 1, 2)) ** 2 + (3.0 * normalCentralMoment(image, region, 2, 1) - normalCentralMoment(image, region, 0, 3)) ** 2;
   end getH3;

   function getH4(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float is
   begin
      return (normalCentralMoment(image, region, 3, 0) + normalCentralMoment(image, region, 1, 2)) ** 2 + (normalCentralMoment(image, region, 2, 1) + normalCentralMoment(image, region, 0, 3)) ** 2;
   end getH4;

   function getH5(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float is
      u30, u12, u21, u03: Float;
   begin
      u30 := normalCentralMoment(image, region, 3, 0);
      u03 := normalCentralMoment(image, region, 0, 3);
      u12 := normalCentralMoment(image, region, 1, 2);
      u21 := normalCentralMoment(image, region, 2, 1);
      return (u30 - 3.0 * u12) * (u30 + u12) * ((u30 + u12) ** 2 - 3.0 * ((u21 + u03) ** 2))
        + (3.0 * u21 - u03) * (3.0 * ((u30 + u12) ** 2) - (u21 + u03) ** 2);
   end getH5;

   function getH6(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float is
      u20, u02, u30, u12, u21, u03, u11: Float;
   begin
      u11 := normalCentralMoment(image, region, 1, 1);
      u20 := normalCentralMoment(image, region, 2, 0);
      u02 := normalCentralMoment(image, region, 0, 2);
      u12 := normalCentralMoment(image, region, 1, 2);
      u21 := normalCentralMoment(image, region, 2, 1);
      u30 := normalCentralMoment(image, region, 3, 0);
      u03 := normalCentralMoment(image, region, 0, 3);
      return (u20 - u02) * ((u30 + u12) ** 2 - (u21 + u03) ** 2
                            + 4.0 * u11 * (u30 + u12) * (u21 + u03));
   end getH6;

   function getH7(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float is
      u30, u12, u21, u03: Float;
   begin
      u12 := normalCentralMoment(image, region, 1, 2);
      u21 := normalCentralMoment(image, region, 2, 1);
      u30 := normalCentralMoment(image, region, 3, 0);
      u03 := normalCentralMoment(image, region, 0, 3);
      return (3.0 * u21 - u03) * (u30 + u12) * ((u30 + u12) ** 2 - 3.0 * ((u21 + u03) ** 2))
        + (3.0 * u12 - u30) * (u21 + u03) * (3.0 * ((u30 + u12) ** 2) - (u21 + u03) ** 2);
   end getH7;


   function logOf(value: Float) return Float is
   begin
      return Log(abs(value), 10.0) * (if value < 0.0 then -1.0 else 1.0);
   end logOf;

   function calculateMoments(image: PixelArray.ImagePlane; region: ImageRegions.Region) return HuMoments is
   begin
      return HuMoments'(h1 => logOf(getH1(image, region)),
                        h2 => logOf(getH2(image, region)),
                        h3 => logOf(getH3(image, region)),
                        h4 => logOf(getH4(image, region)),
                        h5 => logOf(getH5(image, region)),
                        h6 => logOf(getH6(image, region)),
                        h7 => logOf(getH7(image, region)));
   end calculateMoments;

   function orientationAngle(image: PixelArray.ImagePlane; region: ImageRegions.Region) return Float is
      d: Float;
   begin
      d := centralMoment(image, region, 2, 0) - centralMoment(image, region, 0, 2);
      if d /= 0.0 then
         return Arctan(Y => 2.0 * centralMoment(image, region, 1, 1),
                       X => d) / 2.0;
      else
         return 0.0;
      end if;
   end orientationAngle;
end ImageMoments;
