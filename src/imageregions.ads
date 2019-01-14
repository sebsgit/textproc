with PixelArray;
with Ada.Containers.Vectors;

package ImageRegions is
   -- 0 and 255 are reserved to represent 0 / 1 in "binary" images
   type RegionLabel is new Integer range 1 .. 254;

   type Rect is tagged record
      x, y, width, height: Natural;
   end record;

   function toString(r: Rect) return String;

   type Centroid is tagged record
      x, y: Float;
   end record;

   type Region is tagged record
      label: RegionLabel;
      area: Rect;
      center: Centroid;
      pixelCount: Natural;
   end record;

   package RegionVector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Region);

   function detectRegions(image: in out PixelArray.ImagePlane) return RegionVector.Vector;
end ImageRegions;
