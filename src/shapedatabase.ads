with PixelArray;
with ImageRegions;
with ImageMoments;
with ImageThresholds;
with Morphology;

with Ada.Containers.Vectors;

package ShapeDatabase is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Descriptor is tagged record
      moments: ImageMoments.HuMoments;
      orientation: Float;
   end record;

   type CharacterDescriptor is record
      c: Character;
      d: Descriptor;
   end record;

   package ShapeVector is new Ada.Containers.Vectors(Index_Type   => Natural,
                                                     Element_Type => CharacterDescriptor);

   use ShapeVector;

   type DB is tagged record
      shapes: ShapeVector.Vector;
   end record;

   type MatchScore is record
      cc: Character;
      score: Float;
   end record;

   function preprocess(image: PixelArray.ImagePlane) return PixelArray.ImagePlane
     with Post => ImageThresholds.isBinary(preprocess'Result);

   function init return DB;
   function match(database: DB; image: PixelArray.ImagePlane; region: ImageRegions.Region) return MatchScore;
   procedure add(database: in out DB; desc: CharacterDescriptor);
end ShapeDatabase;
