package body NNClassifier is
   function create(config: NeuralNet.Config; numberOfClasses: Positive) return DNN is
      result: DNN(config.size + 1);
      configWithLogits: NeuralNet.Config(config.size + 1);
   begin
      configWithLogits.act := config.act;
      configWithLogits.lr := config.lr;
      configWithLogits.inputSize := config.inputSize;
      configWithLogits.gradientClipAbs := config.gradientClipAbs;
      for i in 1 .. config.size loop
         configWithLogits.sizes(i) := config.sizes(i);
      end loop;
      configWithLogits.sizes(configWithLogits.sizes'Length) := numberOfClasses;
      result.labelCount := numberOfClasses;
      result.nn := NeuralNet.create(configWithLogits);
      return result;
   end create;

   function oneHotEncode(label: Natural; labelCount: Positive) return MathUtils.Vector
     with Pre => label < labelCount
   is
      result: MathUtils.Vector;
   begin
      result.Set_Length(Ada.Containers.Count_Type(labelCount));
      for x of result loop
         x := 0.0;
      end loop;
      result(label + 1) := 1.0;
      return result;
   end oneHotEncode;

   procedure print(nn: in DNN) is
   begin
      nn.nn.print;
   end print;

   procedure train(nn: in out DNN; data: in DataBatch.Batch; labels: in LabelVector) is
      li: Positive;
      target: MathUtils.Vector;
   begin
      li := labels.First_Index;
      for vec of data.data loop
         target := oneHotEncode(labels(li), nn.labelCount);
         nn.nn.train(input  => vec,
                     target => target);
         li := li + 1;
      end loop;
   end train;

   function classify(nn: in out DNN; vec: MathUtils.Vector) return MathUtils.Vector is
      result: MathUtils.Vector;
   begin
      result := nn.nn.forward(vec);
      MathUtils.multiply(result, 10.0);
      MathUtils.softmax(result);
      return result;
   end classify;

end NNClassifier;
