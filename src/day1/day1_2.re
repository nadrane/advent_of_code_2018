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

let getFirstRepeatedFrequency = originalDataPoints => {
  let toDataList = data => List.map(string_of_int, Array.to_list(data));
  let rec iter = (frequencyCounts, currentFrequency, dataPoints) =>
    if (List.length(dataPoints) === 0) {
      iter(
        frequencyCounts,
        currentFrequency,
        toDataList(originalDataPoints),
      );
    } else {
      let key = string_of_int(currentFrequency);
      switch (Js.Dict.get(frequencyCounts, key)) {
      | Some(matchedFrequency) => currentFrequency
      | None =>
        Js.Dict.set(frequencyCounts, key, true);
        iter(
          frequencyCounts,
          int_of_string(List.hd(dataPoints)) + currentFrequency,
          List.tl(dataPoints),
        );
      };
    };
  iter(Js.Dict.empty(), 0, toDataList(originalDataPoints));
};

Js.log(data |> dataToInts |> getFirstRepeatedFrequency);