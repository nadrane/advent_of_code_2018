[@bs.val] external __dirname: string = "";
let inputFilePath = Node.Path.join2(__dirname, "./input.txt");
let readFile = () =>
  Node.Fs.readFileAsUtf8Sync(inputFilePath) |> Js.String.split("\n");

let countContributions = line =>
  line
  |> Js.String.split("")
  |> Array.fold_left(
       (letterOccurrences, letter) =>
         switch (Js.Dict.get(letterOccurrences, letter)) {
         | Some(occurrences) =>
           Js.Dict.set(letterOccurrences, letter, occurrences + 1);
           letterOccurrences;
         | None =>
           Js.Dict.set(letterOccurrences, letter, 1);
           letterOccurrences;
         },
       Js.Dict.empty(),
     );

let findContriubtions = letterCounts =>
  Js.Dict.entries(letterCounts)
  |> Js.Array.filter(pair =>
       Pervasives.snd(pair) === 2 || Pervasives.snd(pair) === 3
     )
  |> Js.Array.map(((_, count)) => count)
  |> Array.fold_left(
       (contributions, count) => {
         let (twos, threes) = contributions;
         switch (count) {
         | 2 => (Pervasives.min(twos + 1, 1), threes)
         | 3 => (twos, Pervasives.min(threes + 1, 1))
         | _ => assert(false)
         };
       },
       (0, 0),
     );

let sumContributions = contributions =>
  contributions
  |> Array.fold_left(
       ((totalTwos, totalThrees), (twos, threes)) => (
         totalTwos + twos,
         totalThrees + threes,
       ),
       (0, 0),
     );

let multiplyContributions = ((twos, threes)) => twos * threes;

readFile()
|> Array.map(countContributions)
|> Array.map(findContriubtions)
|> sumContributions
|> multiplyContributions
|> Js.log;