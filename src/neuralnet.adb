with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body NeuralNet is
   function activate(n: in Neuron; val: in Float) return Float is
   begin
      case n.act is
         when RELU =>
            return Float'Max(0.0, val);
         when LOGISTIC =>
            return 1.0 / (1.0 + MathUtils.F.Exp(-val));
      end case;
   end activate;

   function derivative(n: in Neuron; val: in Float) return Float is
   begin
      case n.act is
         when RELU =>
            if val < 0.0 then
               return 0.0;
            else
               return 1.0;
            end if;
         when LOGISTIC =>
            declare
               x: constant Float := activate(n, val);
            begin
               return x * (1.0 - x);
            end;
      end case;
   end derivative;

   function create(conf: Config) return Net is
      result: Net(conf.size);
   begin
      result.conf := conf;
      result.layers.Reserve_Capacity(Capacity => conf.sizes'Length);

      for i in conf.sizes'Range loop
         declare
            layer: NeuronVecPkg.Vector;
            gradients_per_layer: MathUtils.Vector;
         begin
            layer.Reserve_Capacity(Capacity => Ada.Containers.Count_Type(conf.sizes(i)));
            gradients_per_layer.Reserve_Capacity(Capacity => layer.Capacity);
            for k in 1 .. conf.sizes(i) loop
               declare
                  n: Neuron(if i = 1 then conf.inputSize else conf.sizes(i - 1));
               begin
                  n.act := conf.act;
                  layer.Append(n);
                  gradients_per_layer.Append(0.0);
               end;
            end loop;
            result.layers.Append(layer);
            result.gradients.Append(gradients_per_layer);
         end;
      end loop;
      return result;
   end create;

   procedure print(n: in Neuron) is
   begin
      Ada.Text_IO.Put("{bias: " & n.bias'Image & ", w: ");
      for i in n.w'Range loop
         Ada.Text_IO.Put(n.w(i)'Image & ", ");
      end loop;
      Ada.Text_IO.Put("}");
   end print;

   procedure print(nn: in Net) is
   begin
      for i in nn.layers.First_Index .. nn.layers.Last_Index loop
         for n in nn.layers(i).First_Index .. nn.layers(i).Last_Index loop
            print(nn.layers(i)(n));
         end loop;
         Ada.Text_IO.Put_Line("");
      end loop;
   end print;

   function forward(n: in out Neuron; values: in MathUtils.Vector) return Float is
      result: Float := n.bias;
      wi: Positive := 1;
   begin
      for i in values.First_Index .. values.Last_Index loop
         result := result + values(i) * n.w(wi);
         wi := wi + 1;
      end loop;
      n.z := result;
      n.a := activate(n, result);
      return n.a;
   end forward;

   function forward(nn: in out Net; values: in MathUtils.Vector) return MathUtils.Vector is
      result: MathUtils.Vector;
      input: MathUtils.Vector := values;
   begin
      for i in nn.layers.First_Index .. nn.layers.Last_Index loop
         result.Clear;
         result.Reserve_Capacity(Capacity => nn.layers(i).Length);
         for n in nn.layers(i).First_Index .. nn.layers(i).Last_Index loop
            declare
               neu: Neuron renames nn.layers(i)(n);
            begin
               result.Append( forward(neu, input) );
            end;
         end loop;
         input.Move(result);
      end loop;
      return input;
   end forward;

   function calculateNeuronErr(n: in Neuron; targetVal: Float) return Float is
   begin
      return (n.a - targetVal) * derivative(n, n.z);
   end calculateNeuronErr;

   function clipGradient(nn: in Net; val: Float) return Float
     with Pre => nn.conf.gradientClipAbs >= 0.0
   is
   begin
      if val < -nn.conf.gradientClipAbs then
         return -nn.conf.gradientClipAbs;
      elsif val > nn.conf.gradientClipAbs then
         return nn.conf.gradientClipAbs;
      else
         return val;
      end if;
   end clipGradient;

   -- implements the backpropagation algorithm for nn
   procedure backward(nn: in out Net; input: in MathUtils.Vector; target: in MathUtils.Vector)
     with Pre => input.Length = nn.layers.First_Element.First_Element.w'Length
   is
      ti: Positive := target.First_Index;
      lastLayer: NeuronVecPkg.Vector renames nn.layers.Last_Element;
   begin
      -- errors in the output layer
      for n in lastLayer.First_Index .. lastLayer.Last_Index loop
         declare
            grad_vec_ref: MathUtils.Vector renames nn.gradients(nn.layers.Last_Index);
         begin
            grad_vec_ref(n) := calculateNeuronErr(lastLayer(n), target(ti));
         end;
         ti := ti + 1;
      end loop;

      -- error propagation
      declare
         current_layer: Natural := nn.layers.Last_Index - 1;
         next_to_current: Natural := nn.layers.Last_Index;
      begin
         while current_layer >= nn.layers.First_Index loop
            declare
               current_layer_ref: NeuronVecPkg.Vector renames nn.layers(current_layer);
               next_layer_ref: NeuronVecPkg.Vector renames nn.layers(next_to_current);
               current_layer_grad_ref: MathUtils.Vector renames nn.gradients(current_layer);
               next_layer_grad_ref: MathUtils.Vector renames nn.gradients(next_to_current);
            begin
               for id in current_layer_ref.First_Index .. current_layer_ref.Last_Index loop
                  declare
                     n: Neuron renames current_layer_ref(id);
                  begin
                     current_layer_grad_ref(id) := 0.0;
                     for next_id in next_layer_ref.First_Index .. next_layer_ref.Last_Index loop
                        current_layer_grad_ref(id) := current_layer_grad_ref(id) + next_layer_ref(next_id).w(id) * next_layer_grad_ref(next_id);
                     end loop;
                     current_layer_grad_ref(id) := current_layer_grad_ref(id) * derivative(n, n.z);
                  end;
               end loop;
            end;
            current_layer := current_layer - 1;
            next_to_current := next_to_current - 1;
         end loop;
      end;

      -- updating nn weights and biases
      for il in nn.layers.First_Index .. nn.layers.Last_Index loop
         declare
            layer_ref: NeuronVecPkg.Vector renames nn.layers(il);
            gradients: MathUtils.Vector renames nn.gradients(il);
            weight_reduction: Float := 0.0;
         begin
            for ni in layer_ref.First_Index .. layer_ref.Last_Index loop
               declare
                  n: Neuron renames layer_ref(ni);
               begin
                  --TODO: move gradiend clipping to the optimizer class
                  n.bias := n.bias - nn.clipGradient(Float(nn.conf.lr) * gradients(ni));
                  for wi in n.w'Range loop
                     if il > nn.layers.First_Index then
                        weight_reduction := Float(nn.conf.lr) * gradients(ni) * nn.layers(il - 1)(wi).a;
                     else
                        weight_reduction := Float(nn.conf.lr) * gradients(ni) * input(wi);
                     end if;
                     n.w(wi) := n.w(wi) - nn.clipGradient(weight_reduction);
                  end loop;
               end;
            end loop;
         end;
      end loop;

   end backward;

   procedure train(nn: in out Net; input: in MathUtils.Vector; target: MathUtils.Vector) is
      res: MathUtils.Vector;
   begin
      res := nn.forward(input);
      nn.backward(input, target);
      if False then
         for n of nn.layers(2) loop
            print(n);
            --  Ada.Text_IO.Put_Line("");
            --   MathUtils.print(nn.gradients(1));
            Ada.Text_IO.Put_Line("");
         end loop;
         Ada.Text_IO.Put_Line("-");
      end if;
   end train;

end NeuralNet;
