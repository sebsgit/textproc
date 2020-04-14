with Ada.Text_IO;
with Ada.Streams.Stream_IO;

package body NeuralNet.IO is

   file_magic_mark: constant Integer := 16#666DEAD#;
   file_format_version: constant Integer := 1;

   procedure save(nn: in NeuralNet.Net; path: in String) is
      output_file: Ada.Streams.Stream_IO.File_Type;
      output_stream: Ada.Streams.Stream_IO.Stream_Access;
   begin
      Ada.Streams.Stream_IO.Create(File => output_file,
                                   Mode => Ada.Streams.Stream_IO.Out_File,
                                   Name => path);
      output_stream := Ada.Streams.Stream_IO.Stream(output_file);

      Integer'Write(output_stream, file_magic_mark);
      Integer'Write(output_stream, file_format_version);
      Positive'Write(output_stream, nn.conf.size);
      NeuralNet.Config'Write(output_stream, nn.conf);
      NeuralNet.Net'Write(output_stream, nn);
      Ada.Streams.Stream_IO.Close(output_file);
   end save;

   function load(path: in String; status: out Boolean) return NeuralNet.Net is
      null_net_config: NeuralNet.Config(1);
      input_file: Ada.Streams.Stream_IO.File_Type;
      input_stream: Ada.Streams.Stream_IO.Stream_Access;
   begin
      ADa.Streams.Stream_IO.Open(File => input_file,
                                 Mode => Ada.Streams.Stream_IO.In_File,
                                 Name => path);
      input_stream := Ada.Streams.Stream_IO.Stream(input_file);
      status := not Ada.Streams.Stream_IO.End_Of_File(input_file);

      if status then
         declare
            magic_mark: Integer;
            format_ver: Integer;
            conf_size: Positive;
         begin
            Integer'Read(input_stream, magic_mark);
            Integer'Read(input_stream, format_ver);
            Positive'Read(input_stream, conf_size);
            if magic_mark = file_magic_mark and format_ver = file_format_version then
               declare
                  conf: NeuralNet.Config(conf_size);
               begin
                  NeuralNet.Config'Read(input_stream, conf);
                  return nn: NeuralNet.Net := NeuralNet.create(conf) do
                     NeuralNet.Net'Read(input_stream, nn);
                     Ada.Streams.Stream_IO.Close(input_file);
                  end return;
               end;
            end if;
         end;
         Ada.Streams.Stream_IO.Close(input_file);
      end if;
      null_net_config.sizes := (1 => 1);
      return NeuralNet.create(null_net_config);
   end load;
end NeuralNet.IO;
