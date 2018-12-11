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

let asleepByGuard = Js.Dict.empty();
let asleepByMinute = Js.Dict.empty();
let recordTimeAsleep = sleepCycle => {
  let sleepCycleLength =
    int_of_float(
      Js.Date.valueOf(Js.Date.fromString(sleepCycle.end_))
      -. Js.Date.valueOf(Js.Date.fromString(sleepCycle.start)),
    )
    / 1000
    / 60;
  switch (Js.Dict.get(asleepByGuard, sleepCycle.id)) {
  | Some(minutesSleeping) =>
    Js.Dict.set(
      asleepByGuard,
      sleepCycle.id,
      minutesSleeping + sleepCycleLength,
    )
  | None => Js.Dict.set(asleepByGuard, sleepCycle.id, sleepCycleLength)
  };
};

let sleepLogs =
  readFile()
  |> Js.Array.sortInPlaceWith(dateComparator)
  /* |> Js.Array.slice(~start=0, ~end_=80) */
  |> chunkByGuard
  |> List.map(createSleepCycles)
  |> List.flatten
  |> List.filter(log => log.sleeping);

List.iter(recordTimeAsleep, sleepLogs);
let sleepiestGuard =
  Js.Dict.entries(asleepByGuard)
  |> Array.fold_left(
       ((maxId, maxTimeSleep), (id, timeSleep)) =>
         maxTimeSleep > timeSleep ? (maxId, maxTimeSleep) : (id, timeSleep),
       ("id", 2),
     )
  |> Pervasives.fst;

Js.log(sleepiestGuard);

let timeAleepByMinute = (guardId, sleepLogs) =>
  sleepLogs
  |> List.filter(log => log.id == guardId)
  |> List.iter(log => {
       let start = Js.Date.fromString(log.start);
       let end_ = Js.Date.fromString(log.end_);
       let startMinute = int_of_float(Js.Date.getMinutes(start));
       let endMinute = int_of_float(Js.Date.getMinutes(end_));
       for (minute in startMinute to endMinute - 1) {
         let minuteKey = string_of_int(minute);
         switch (Js.Dict.get(asleepByMinute, minuteKey)) {
         | Some(minuteAsleep) =>
           Js.Dict.set(asleepByMinute, minuteKey, minuteAsleep + 1)
         | None => Js.Dict.set(asleepByMinute, minuteKey, 1)
         };
       };
     });

timeAleepByMinute(sleepiestGuard, sleepLogs);
Js.log(asleepByMinute);