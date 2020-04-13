with PixelArray;
with ImageRegions;
with ImageMoments;
with ImageThresholds;
with Morphology;
with Histogram;
with HistogramDescriptor;

with Ada.Containers.Vectors;
with Ada.Directories;

package ShapeDatabase is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Descriptor is tagged record
      moments: ImageMoments.HuMoments;
      orientation: Float;
      histogram: HistogramDescriptor.Data;
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

   function Preprocess_And_Detect_Regions(image: in PixelArray.ImagePlane; res: out ImageRegions.RegionVector.Vector) return PixelArray.ImagePlane;

   function getDB return DB;
   function match(database: DB; image: PixelArray.ImagePlane; region: ImageRegions.Region) return MatchScore;

   function loadShapes(imagePath: String) return ShapeVector.Vector
     with Pre => Ada.Directories.Exists(Name => imagePath),
     Post => not loadShapes'Result.Is_Empty;
private
   staticDB: DB;
end ShapeDatabase;
