with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with System;
with System.Address_To_Access_Conversions;

with Tensor; use Tensor;

package body TensorTests is

   procedure Register_Tests (T: in out TestCase) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, testTensor'Access, "tensor API");
      Register_Routine (T, testFlatten'Access, "tensor API: Flatten");
      Register_Routine (T, testDataGetter'Access, "tensor API: Data");
      Register_Routine (T, testDot'Access, "tensor API: Dot");
      Register_Routine (T, testPlus'Access, "tensor API: +");
      Register_Routine (T, testMinus'Access, "tensor API: -");
   end Register_Tests;

   function Name(T: TestCase) return Test_String is
   begin
      return Format("Tensor Tests");
   end Name;

   procedure testTensor(T : in out Test_Cases.Test_Case'Class) is
      t0: constant Tensor.Var := Tensor.Variable(values => (1.0, 2.0, 3.0));
      m0: constant Tensor.Var := Tensor.Matrix(row_length => 2,
                                               values     => (1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
   begin
      Assert(t0.Dimension_Count = 1, "");
      Assert(t0.Dimension(1) = 3, "");

      Assert(m0.Dimension_Count = 2, "");
      Assert(m0.Dimension(1) = 3, "");
      Assert(m0.Dimension(2) = 2, "");
   end testTensor;

   procedure testFlatten(T : in out Test_Cases.Test_Case'Class) is
      m0: constant Tensor.Var := Tensor.Matrix(row_length => 2,
                                               values     => (1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
      m0_flat: constant Tensor.Var := m0.Flatten;
   begin
      Assert(m0.Dimension_Count = 2, "");
      Assert(m0.Dimension(1) = 3, "");
      Assert(m0.Dimension(2) = 2, "");

      Assert(m0_flat.Dimension_Count = 1, "");
      Assert(m0_flat.Dimension(1) = 6, "");
   end testFlatten;

   procedure testDataGetter(T : in out Test_Cases.Test_Case'Class) is
      m0: constant Tensor.Var := Tensor.Matrix(row_length => 2,
                                               values     => (1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
   begin
      Assert(m0.Element(1, 1) = 1.0, "");
      Assert(m0.Element(1, 2) = 2.0, "");
      Assert(m0.Element(2, 1) = 3.0, "");
      Assert(m0.Element(2, 2) = 4.0, "");
      Assert(m0.Element(3, 1) = 5.0, "");
      Assert(m0.Element(3, 2) = 6.0, "");
   end testDataGetter;

   procedure testDot(T : in out Test_Cases.Test_Case'Class) is
      A: constant Tensor.Var := Tensor.Matrix(row_length => 2,
                                              values     => (1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
      x: constant Tensor.Var := Tensor.Variable(values => (10.0, 20.0));

      xx: constant Tensor.Var := x.Dot(x);
      Ax: constant Tensor.Var := A.Dot(x);
   begin
      Assert(xx.Dimension_Count = 1, "");
      Assert(xx.Dimension(1) = 1, "");
      Assert(xx.Element(1, 1) = 500.0, "");

      Assert(Ax.Dimension_Count = 1, "");
      Assert(Ax.Dimension(1) = 3, "");
      Assert(Ax.Data.Element(1) = 50.0, "");
      Assert(Ax.Data.Element(2) = 110.0, "");
      Assert(Ax.Data.Element(3) = 170.0, "");
   end testDot;

   procedure testPlus(T : in out Test_Cases.Test_Case'Class) is
      x: constant Tensor.Var := Tensor.Variable(values => (10.0, 20.0));
      x2: constant Tensor.Var := x + x;
   begin
      Assert(x2.Dimension_Count = 1, "");
      Assert(x2.Dimension(1) = 2, "");
      Assert(x2.Data.Element(1) = 20.0, "");
      Assert(x2.Data.Element(2) = 40.0, "");
   end testPlus;

   procedure testMinus(T : in out Test_Cases.Test_Case'Class) is
      x: constant Tensor.Var := Tensor.Variable(values => (10.0, 20.0));
      y: constant Tensor.Var := Tensor.Variable(values => (1.0, 2.0));
      yx: constant Tensor.Var := y - x;
   begin
      Assert(yx.Dimension_Count = 1, "");
      Assert(yx.Dimension(1) = 2, "");
      Assert(yx.Data.Element(1) = -9.0, "");
      Assert(yx.Data.Element(2) = -18.0, "");
   end testMinus;

end TensorTests;
