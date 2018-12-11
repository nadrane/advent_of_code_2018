[@bs.val] external __dirname: string = "";
let inputFilePath = Node.Path.join2(__dirname, "./input.txt");
let readFile = () =>
  Node.Fs.readFileAsUtf8Sync(inputFilePath) |> Js.String.split("\n");

type sleepCycle = {
  id: string,
  start: string,
  end_: string,
  sleeping: bool,
};

let rec print = list =>
  switch (list) {
  | [] => ()
  | [head, ...tail] =>
    Js.log(head);
    print(tail);
  };
let timeRegex = [%bs.re "/\\[(.*)\\]/"];
let guardRegex = [%bs.re "/#(\\d{1,4})/"];

let dateComparator = (a, b) => {
  let extractDate = line =>
    Js.Date.fromString(Js.String.splitByRe(timeRegex, line)[1])
    |> Js.Date.valueOf;

  let difference = extractDate(a) -. extractDate(b);

  if (difference > 0.) {
    1;
  } else if (difference == 0.) {
    0;
  } else {
    (-1);
  };
};

let chunkByGuard = lines => {
  let rec iter = (lines, chunks, currrentChunk) =>
    switch (lines) {
    | [] => chunks
    | [nextLine, ...rest] =>
      Js.String.includes("Guard", nextLine) ?
        iter(rest, chunks @ [currrentChunk @ [nextLine]], [nextLine]) :
        iter(rest, chunks, currrentChunk @ [nextLine])
    };
  switch (Array.to_list(lines)) {
  | [] => []
  | [firstLine, ...restOfLines] => iter(restOfLines, [], [firstLine])
  };
};

let getTime = line => Js.String.match(timeRegex, line);
let getGuardId = line => Js.String.match(guardRegex, line);
let createSleepCycle = (line, lastTimestamp, id) =>
  switch (getTime(line)) {
  | Some(matchedTime) =>
    if (Js.String.includes("Guard", line)) {
      {start: lastTimestamp, end_: matchedTime[1], sleeping: false, id};
    } else if (Js.String.includes("falls asleep", line)) {
      {start: lastTimestamp, end_: matchedTime[1], sleeping: false, id};
    } else if (Js.String.includes("wakes up", line)) {
      {start: lastTimestamp, end_: matchedTime[1], sleeping: true, id};
    } else {
      Js.log("incorrect string found");
      assert(false);
    }
  | None =>
    Js.log("time not matched");
    assert(false);
  };

let createSleepCycles = guardChunk => {
  let rec iter = (line, remainingLines, lastTimestamp, sleepCycles, guardId) =>
    switch (remainingLines) {
    | [] =>
      switch (getTime(line)) {
      | Some(currentTime) =>
        sleepCycles @ [createSleepCycle(line, lastTimestamp, guardId)]
      | None =>
        Js.log("Could not parse time");
        assert(false);
      }
    | [nextLine, ...remainingLines] =>
      switch (getTime(line)) {
      | Some(currentTime) =>
        iter(
          nextLine,
          remainingLines,
          currentTime[1],
          sleepCycles @ [createSleepCycle(line, lastTimestamp, guardId)],
          guardId,
        )
      | _ =>
        Js.log("failed to parse time");
        assert(false);
      }
    };

  switch (guardChunk) {
  | [] => []
  | [oneLine] => []
  | [firstLine, ...restOfChunk] =>
    switch (getTime(firstLine), getGuardId(firstLine)) {
    | (Some(firstTimestamp), Some(firstGuardId)) =>
      iter(
        List.hd(restOfChunk),
        List.tl(restOfChunk),
        firstTimestamp[1],
        [],
        firstGuardId[1],
      )
    | _ =>
      Js.log("no guards left");
      assert(false);
    }
  };
};

let sleepLogs =
  readFile()
  |> Js.Array.sortInPlaceWith(dateComparator)
  |> chunkByGuard
  |> List.map(createSleepCycles)
  |> List.flatten
  |> List.filter(log => log.sleeping);

let asleepByMinute = Js.Dict.empty();

let timeAsleepOnMinute38 = sleepLogs =>
  sleepLogs
  |> List.iter(log => {
       let id = log.id;
       let start = Js.Date.fromString(log.start);
       let end_ = Js.Date.fromString(log.end_);
       let startMinute = int_of_float(Js.Date.getMinutes(start));
       let endMinute = int_of_float(Js.Date.getMinutes(end_));
       for (minute in startMinute to endMinute - 1) {
         let minuteKey = string_of_int(minute);
         switch (Js.Dict.get(asleepByMinute, id)) {
         | Some(guardDict) =>
           switch (Js.Dict.get(guardDict, minuteKey)) {
           | Some(minutesAsleepOnMinute) =>
             Js.Dict.set(guardDict, minuteKey, minutesAsleepOnMinute + 1)
           | None => Js.Dict.set(guardDict, minuteKey, 1)
           }
         | None => Js.Dict.set(asleepByMinute, id, Js.Dict.empty())
         };
       };
     });

timeAsleepOnMinute38(sleepLogs);
Js.log(asleepByMinute);