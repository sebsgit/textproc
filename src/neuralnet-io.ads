with NeuralNet;

package NeuralNet.IO is
   procedure save(nn: in NeuralNet.Net; path: in String);
   function load(path: in String; status: out Boolean) return NeuralNet.Net;
end NeuralNet.IO;
