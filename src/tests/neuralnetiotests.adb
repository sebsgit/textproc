with AUnit.Assertions; use AUnit.Assertions;

with NeuralNet;
with NeuralNet.IO;
with MathUtils;

with Ada.Text_IO;

package body NeuralNetIOTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testCreateNNFile'Access, "Neural Net IO: create file");
      Register_Routine (T, testLoadNNFile'Access, "Neural Net IO: load file");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Neural Net IO Tests");
   end Name;

   nn_file_path: constant String := "test_net.nn";
   nn_input_data: MathUtils.Vector;
   nn_expected_results: MathUtils.Vector;

   procedure testCreateNNFile(T : in out Test_Cases.Test_Case'Class) is
      config: NeuralNet.Config(2);
      nn_input: MathUtils.Vector;
   begin
      for i in 0 .. 10 loop
         nn_input_data.Append(Float(i) / 10.0);
      end loop;

      config.act := NeuralNet.RELU;
      config.inputSize := 1;
      config.lr := 0.05;
      config.gradientClipAbs := 0.4;
      config.sizes := (1 => 2, 2 => 1);

      declare
         nn: NeuralNet.Net := NeuralNet.create(config);
      begin
         for i of nn_input_data loop
            declare
               inp: MathUtils.Vector;
            begin
               inp.Append(i);
               nn_expected_results.Append(nn.forward(inp));
            end;
         end loop;

         NeuralNet.IO.save(nn, nn_file_path);
      end;
   end testCreateNNFile;

   procedure testLoadNNFile(T : in out Test_Cases.Test_Case'Class) is
      status: Boolean := False;
      nn: NeuralNet.Net := NeuralNet.IO.load(nn_file_path, status);
      tmp_inp: MathUtils.Vector;
   begin
      Assert(status, "can't load the nn file");
      for idx in nn_input_data.First_Index .. nn_input_data.Last_Index loop
         tmp_inp.Clear;
         tmp_inp.Append(nn_input_data(idx));
         declare
            res: constant Float := nn.forward(tmp_inp).First_Element;
            exp_res: constant Float := nn_expected_results(idx);
         begin
            Assert(abs(res - exp_res) < 0.001, "NN result failure: " & res'Image & " " & exp_res'Image);
         end;
      end loop;
   end testLoadNNFile;

end NeuralNetIOTests;
