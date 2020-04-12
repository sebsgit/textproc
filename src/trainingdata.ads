with MathUtils;
with NNClassifier;
with DataBatch;
with PixelArray;

with Ada.Strings.Unbounded;

package TrainingData is
   pragma Elaborate_Body;
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Set is tagged limited record
      values: DataBatch.Batch;
      labels: NNClassifier.LabelVector;
   end record
     with Dynamic_Predicate => values.size = Natural(labels.Length);

   blockSize: constant Positive := 28;
   blockArea: constant Positive := blockSize * blockSize;

   function size(data: in Set) return Natural;
   procedure add(data: in out Set; label: Natural; vec: MathUtils.Vector);
   procedure loadFrom(data: in out Set; path: in Ada.Strings.Unbounded.Unbounded_String);

   function toDataVector(img: in PixelArray.ImagePlane; invertPixels: Boolean := False) return MathUtils.Vector
     with Post => Natural(toDataVector'Result.Length) = img.width * img.height;

end TrainingData;
