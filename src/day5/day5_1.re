[@bs.val] external __dirname: string = "";
let inputFilePath = Node.Path.join2(__dirname, "./input.txt");
let readFile = () => Node.Fs.readFileAsUtf8Sync(inputFilePath);

let compareChars = (a, b) => {
  let charCodeA = Char.code(a);
  let charCodeB = Char.code(b);

  switch (charCodeA < 91, charCodeB < 91) {
  | (true, false) => charCodeA + 32 == charCodeB
  | (false, true) => charCodeA == 32 + charCodeB
  | (false, false) => false
  | (true, true) => false
  };
};

let rec minimize = line => {
  let rec iter = (minimizedLine, idx) =>
    Js.String.length(minimizedLine) == 0 ?
      "" :
      Js.String.length(minimizedLine) - 1 == idx ?
        minimizedLine :
        compareChars(minimizedLine.[idx], minimizedLine.[idx + 1]) ?
          iter(
            Js.String.slice(~from=0, ~to_=idx, minimizedLine)
            ++ Js.String.sliceToEnd(~from=idx + 2, minimizedLine),
            0,
          ) :
          iter(minimizedLine, idx + 1);
  iter(line, 0);
};

readFile() |> minimize |> String.length |> Js.log;