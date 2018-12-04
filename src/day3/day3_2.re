[@bs.val] external __dirname: string = "";
let inputFilePath = Node.Path.join2(__dirname, "./input.txt");

type claim = {
  id: int,
  row: int,
  col: int,
  width: int,
  height: int,
};

let readFile = () =>
  Node.Fs.readFileAsUtf8Sync(inputFilePath) |> Js.String.split("\n");

let parseClaim = line => {
  let id =
    Js.String.split("@", line)[0]
    |> Js.String.trim
    |> Js.String.sliceToEnd(~from=1)
    |> int_of_string;
  /* Destructured assignments needs to be easier to figure out... */
  let otherHalf =
    Js.String.split("@", line)[1] |> Js.String.trim |> Js.String.split(": ");
  let position = otherHalf[0];
  let dimensions = otherHalf[1];

  {
    id,
    row: int_of_string(Js.String.split(",", position)[1]),
    col: int_of_string(Js.String.split(",", position)[0]),
    width: int_of_string(Js.String.split("x", dimensions)[0]),
    height: int_of_string(Js.String.split("x", dimensions)[1]),
  };
};

let width = 1000;
let height = 1000;
let grid = Array.make_matrix(width, height, 0);

let getCoordsOfClaim = claim => {
  let coords = ref([]);
  for (row in claim.row to claim.row + claim.height - 1) {
    for (col in claim.col to claim.col + claim.width - 1) {
      coords := [(row, col), ...coords^];
    };
  };
  coords^;
};
let applyClaimToGrid = claim => {
  let coords = getCoordsOfClaim(claim);
  List.iter(
    ((row, col)) =>
      switch (grid[row][col]) {
      | 0 => grid[row][col] = 1
      | 1 => grid[row][col] = 2
      | _ => ()
      },
    coords,
  );
  claim;
};

let doesNotIntersect = claim => {
  let rec iter = coords =>
    if (List.length(coords) === 0) {
      true;
    } else {
      let (row, col) = List.hd(coords);
      switch (grid[row][col]) {
      /* If it only intersects with itself */
      | 1 => true && iter(List.tl(coords))
      | _ => false
      };
    };
  iter(getCoordsOfClaim(claim));
};

readFile()
|> Array.map(parseClaim)
|> Array.map(applyClaimToGrid)
|> Array.to_list
|> List.filter(c => doesNotIntersect(c))
|> Js.log;