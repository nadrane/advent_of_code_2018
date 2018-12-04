[@bs.val] external __dirname: string = "";
let inputFilePath = Node.Path.join2(__dirname, "./input.txt");
let readFile = () =>
  Node.Fs.readFileAsUtf8Sync(inputFilePath) |> Js.String.split("\n");

let generatePairs = lines => {
  let rec iter = (idx1, idx2, generatedPairs) =>
    switch (
      idx1 === Array.length(lines) - 2,
      idx2 === Array.length(lines) - 1,
    ) {
    | (true, true) => generatedPairs
    | (false, true) =>
      iter(
        idx1 + 1,
        idx1 + 2,
        [(lines[idx1], lines[idx2]), ...generatedPairs],
      )
    | (false, false) =>
      iter(idx1, idx2 + 1, [(lines[idx1], lines[idx2]), ...generatedPairs])
    | (true, false) => assert(false)
    };
  iter(0, 1, [(lines[0], lines[0])]);
};

let zipLinesByLetter = ((line1, line2)) =>
  List.combine(
    Array.to_list(Js.String.split("", line1)),
    Array.to_list(Js.String.split("", line2)),
  );

let countMismatches = pair =>
  pair
  |> zipLinesByLetter
  |> List.fold_left(
       (mismatches, (letter1, letter2)) =>
         mismatches + (letter1 !== letter2 ? 1 : 0),
       0,
     );
let linesDifferByOne = pair => countMismatches(pair) === 1;

let rec findLineWithoutDifferentLetter = (idx, (line1, line2)) =>
  if (Js.String.length(line1) === idx) {
    "";
  } else {
    line1.[idx] === line2.[idx] ?
      String.make(1, line1.[idx])
      ++ findLineWithoutDifferentLetter(idx + 1, (line1, line2)) :
      findLineWithoutDifferentLetter(idx + 1, (line1, line2));
  };

readFile()
|> generatePairs
|> List.filter(linesDifferByOne)
|> List.map(findLineWithoutDifferentLetter(0))
|> Js.log;