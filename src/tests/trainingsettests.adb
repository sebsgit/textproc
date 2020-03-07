with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers;

with TrainingData;
with PixelArray;
with MathUtils;
with NeuralNet;
with NNClassifier;
with Timer;
with ImageIO;
with CSV;

use Ada.Containers;
use MathUtils.Float_Vec;

package body TrainingSetTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testDataGenerator'Access, "float vec generator");
      Register_Routine (T, testTrainInput'Access, "input validation");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Training Set Tests");
   end Name;

   procedure testDataGenerator(T : in out Test_Cases.Test_Case'Class) is
      image: PixelArray.ImagePlane;
      output: MathUtils.Vector;
      expectedValues: MathUtils.Vector;
   begin
      image := PixelArray.allocate(16, 16);
      for x in 0 .. 15 loop
         for y in 0 .. 15 loop
            image.set(x, y, PixelArray.Pixel((x + 1) * (y + 1) - 1));
            expectedValues.Append(Float(image.get(x, y)) / 255.0);
         end loop;
      end loop;

      output := TrainingData.toDataVector(image);
      Assert(output.Length = 16 * 16, "data length");
      Assert(output = expectedValues, "");
   end testDataGenerator;

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

   procedure loadMNistData(input, validation: in out TrainingData.Set) is
      reader: CSV.Reader := CSV.open("../training_set/mnist_train_small.csv");
      trainSetSize: constant Positive := 1350;
      testSetSize: constant Positive := 250;

      procedure load(s: in out TrainingData.Set; cnt: in Positive)
        with Post => s.size >= cnt
      is
         i: Positive := 1;
         vec: MathUtils.Vector;
         label: Natural;
      begin
         while i <= cnt loop
            vec := reader.next;
            label := Natural(vec.Element(1));
            vec.Delete_First;
            for v of vec loop
               v := v / 255.0;
            end loop;

            Assert(Positive(vec.Length) = TrainingData.blockArea, "wrong size of input, expected: " & TrainingData.blockArea'Image & ", got: " & vec.Length'Image);
            s.add(label, vec);
            i := i + 1;
         end loop;
      end load;
   begin
      load(input, trainSetSize);
      load(validation, testSetSize);
   end loadMNistData;

   procedure saveToFile(data: MathUtils.Vector; path: String)
     with Pre => Positive(data.Length) = TrainingData.blockArea
   is
      image: PixelArray.ImagePlane;
      saveResult: Boolean;
   begin
      image := PixelArray.allocate(width  => TrainingData.blockSize,
                                   height => TrainingData.blockSize);
      for y in 0 .. image.height - 1 loop
         for x in 0 .. image.width - 1 loop
            declare
               id: constant Positive := y * image.width + x + 1;
            begin
               image.set(x  => x,
                         y  => y,
                         px => PixelArray.Pixel(255.0 * data(id)));
            end;
         end loop;
      end loop;
      saveResult := ImageIO.save(filename => path,
                                 image    => image);
   end saveToFile;

   procedure testTrainInput(T : in out Test_Cases.Test_Case'Class) is
      set: TrainingData.Set;
      validationSet: TrainingData.Set;
      config: NeuralNet.Config(1);
      dnn: NNClassifier.DNN(config.size + 1);
      tm: Timer.T;

      pred: MathUtils.Vector;
      di: Positive := 1;

      failedPredictions: Natural := 0;
   begin
      Ada.Text_IO.Put_Line("Load training set...");
      tm := Timer.start;
      -- set.loadFrom(Ada.Strings.Unbounded.To_Unbounded_String("../training_set/"));
      -- tm.report;
      -- validationSet.loadFrom(Ada.Strings.Unbounded.To_Unbounded_String("../training_set_test_cases/"));

      loadMNistData(set, validationSet);

      tm.report;

      Assert(set.size > 10, "not enough train data: " & set.size'Image);
      Assert(validationSet.size > 10, "not enough test data: " & validationSet.size'Image);

      config.act := NeuralNet.LOGISTIC;
      config.inputSize := TrainingData.blockArea;
      config.lr := 0.7;
      config.sizes := (1 => 32);

      dnn := NNClassifier.create(config          => config,
                                 numberOfClasses => 10);

      Ada.Text_IO.Put_Line("Train the model...");

      tm.reset;
      dnn.train(data   => set.values,
                labels => set.labels);
      tm.report;

      Ada.Text_IO.Put_Line("Inference...");
      tm.reset;
      for lab of validationSet.labels loop
         --  MathUtils.print(validationSet.values.data(di));
         pred := dnn.classify(validationSet.values.data(di));
         for idx in pred.First_Index .. pred.Last_Index loop
            if idx /= lab + 1 then
               if pred(idx) > pred(lab + 1) then
                  failedPredictions := failedPredictions + 1;
                  exit;
               end if;
            end if;
         end loop;
         di := di + 1;
      end loop;
      tm.report;

      declare
         acc: Float;
      begin
         acc := Float(validationSet.size - failedPredictions) / Float(validationSet.size);
         Ada.Text_IO.Put_Line("Model accuracy: " & acc'Image);
         -- require > 75% accuracy
         Assert(acc > 0.75, "total: " & validationSet.size'Image & ", failed: " & failedPredictions'Image);
      end;

   end testTrainInput;
end TrainingSetTests;
