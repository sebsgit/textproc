with Ada.Containers.Vectors; use Ada.Containers;

with NeuralNet;
with DataBatch;
with MathUtils;

package NNClassifier is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   package LabelVecPkg is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                     Element_Type => Natural);
   subtype LabelVector is LabelVecPkg.Vector;

   type DNN (hiddenLayerCount: Positive) is tagged private;

   function create(config: NeuralNet.Config; numberOfClasses: Positive) return DNN
     with Pre => numberOfClasses >= 2;

   procedure print(nn: in DNN);

   procedure train(nn: in out DNN; data: in DataBatch.Batch; labels: in LabelVector)
     with Pre => Natural(labels.Length) = data.size;
   function classify(nn: in out DNN; vec: MathUtils.Vector) return MathUtils.Vector;

private
   type DNN (hiddenLayerCount: Positive) is tagged record
      labelCount: Positive;
      nn: NeuralNet.Net(hiddenLayerCount);
   end record;
end NNClassifier;
