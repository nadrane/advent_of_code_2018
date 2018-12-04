[@bs.val] external __dirname: string = "";
let inputFilePath = Node.Path.join2(__dirname, "./input.txt");
let data = Node.Fs.readFileAsUtf8Sync(inputFilePath);
let dataToInts = data =>
  Js.String.split("\n", data)
  |> Js.Array.map(line =>
       Js.String.startsWith("-", line) ?
         (-1) * int_of_string(Js.String.sliceToEnd(~from=1, line)) :
         int_of_string(Js.String.sliceToEnd(~from=1, line))
     );

let add = dataPoints =>
  dataPoints |> Array.fold_left((total, dataPoint) => total + dataPoint, 0);

Js.log(data |> dataToInts |> add);