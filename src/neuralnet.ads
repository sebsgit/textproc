with Ada.Containers.Vectors; use Ada.Containers;
with MathUtils;

package NeuralNet is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   type Activator is (RELU, LOGISTIC);
   type LossFunction is (MSE);

   type Shape is array (Positive range <>) of Positive;
   type Weights is array (Positive range <>) of Float;
   type LearningRate is new Float range 0.0 .. Float'Last;

   subtype NeuronIndex is Positive range 1 .. 2048;
   subtype LayerIndex is Positive range 1 .. 32;

   type Neuron (size: NeuronIndex := 1) is record
      a: Float := 0.0;
      z: Float := 0.0;
      bias: Float := 0.0;
      act: Activator := RELU;
      w: Weights (1 .. size) := (others => MathUtils.rand01 * (1.0 / MathUtils.F.Sqrt(Float(size))));
   end record;

   type Config (size: LayerIndex) is record
      act: Activator := RELU;
      inputSize: Positive := 1;
      lr: LearningRate := 0.05;
      gradientClipAbs: Float := 5.0;
      sizes: Shape(1 .. size);
   end record;

   package NeuronVecPkg is new Ada.Containers.Vectors(Index_Type   => NeuronIndex,
                                                      Element_Type => Neuron);
   package NeuronLayerVecPkg is new Ada.Containers.Vectors(Index_Type   => LayerIndex,
                                                           Element_Type => NeuronVecPkg.Vector,
                                                           "=" => NeuronVecPkg."=");
   package LayerErrorVecPkg is new Ada.Containers.Vectors(Index_Type   => LayerIndex,
                                                          Element_Type => MathUtils.Vector,
                                                          "="          => MathUtils.Float_Vec."=");

   subtype LayerVector is NeuronLayerVecPkg.Vector;

   type Net (size: Positive) is tagged record
      layers: LayerVector;
      gradients: LayerErrorVecPkg.Vector;
      conf: Config (size);
   end record;

   function create(conf: Config) return Net;
   procedure print(n: in Neuron);
   procedure print(nn: in Net);

   function forward(n: in out Neuron; values: in MathUtils.Vector) return Float
     with Pre => values.Length = n.w'Length;

   function forward(nn: in out Net; values: in MathUtils.Vector) return MathUtils.Vector
     with Pre => values.Length = nn.layers(1)(1).w'Length,
     Post => forward'Result.Length = nn.layers.Last_Element.Length;

   procedure train(nn: in out Net; input: in MathUtils.Vector; target: MathUtils.Vector)
     with Pre => input.Length = nn.layers(1)(1).w'Length;

end NeuralNet;
