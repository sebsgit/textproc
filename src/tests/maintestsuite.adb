with AUnit.Run;
with AUnit.Reporter.Text;

with ImageTests;
with ShapeMatchingTests;
with ShapeDatabaseTest;
with HistogramTests;
with HistogramDescriptorTests;
with NeuralNetTests;
with NeuralNetIOTests;
with DataBatchTests;
with NNClassifierTests;
with TrainingSetTests;
with CSVTests;
with OpenCLTests;
with GpuImageTests;
with GpuInferenceTests;
with GpuComponentLabelingTests;
with TensorTests;

package body MainTestSuite is
   use AUnit.Test_Suites;

   suiteObject: aliased Test_Suite;

   csvTestCase: aliased CSVTests.TestCase;
   imageTestCase: aliased ImageTests.ImageTest;
   shapeTestCase: aliased ShapeMatchingTests.TestCase;
   histogramTestCase: aliased HistogramTests.TestCase;
   histogramDescriptorTestCase: aliased HistogramDescriptorTests.TestCase;
   shapeDbTests: aliased ShapeDatabaseTest.TestCase;
   nnTests: aliased NeuralNetTests.TestCase;
   nnioTests: aliased NeuralNetIOTests.TestCase;
   dataBatchTestCase: aliased DataBatchTests.TestCase;
   nnClassifyTestCase: aliased NNClassifierTests.TestCase;
   trainSetTestCase: aliased TrainingSetTests.TestCase;
   openclTestCase: aliased OpenCLTests.TestCase;
   gpuImageTestCase: aliased GpuImageTests.TestCase;
   gpuInferenceTestCase: aliased GpuInferenceTests.TestCase;
   gpuCCLTestCase: aliased GpuComponentLabelingTests.TestCase;
   tensorTestCase: aliased TensorTests.TestCase;

   function Suite return Access_Test_Suite is
   begin
      Add_Test(suiteObject'Access, tensorTestCase'Access);
      Add_Test(suiteObject'Access, nnioTests'Access);
      Add_Test(suiteObject'Access, openclTestCase'Access);
      Add_Test(suiteObject'Access, gpuImageTestCase'Access);
      Add_Test(suiteObject'Access, gpuCCLTestCase'Access);
      Add_Test(suiteObject'Access, gpuInferenceTestCase'Access);
      Add_Test(suiteObject'Access, csvTestCase'Access);
      Add_Test(suiteObject'Access, imageTestCase'Access);
      Add_Test(suiteObject'Access, shapeTestCase'Access);
      Add_Test(suiteObject'Access, histogramTestCase'Access);
      Add_Test(suiteObject'Access, histogramDescriptorTestCase'Access);
      Add_Test(suiteObject'Access, shapeDbTests'Access);
      Add_Test(suiteObject'Access, nnTests'Access);
      Add_Test(suiteObject'Access, dataBatchTestCase'Access);
      Add_Test(suiteObject'Access, nnClassifyTestCase'Access);
      Add_Test(suiteObject'Access, trainSetTestCase'Access);
      return suiteObject'Access;
   end Suite;

   procedure runAll is
      procedure Run is new AUnit.Run.Test_Runner (MainTestSuite.Suite);
      reporter : AUnit.Reporter.Text.Text_Reporter;
   begin
      Run(reporter);
   end runAll;

end MainTestSuite;
