// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var List = require("bs-platform/lib/js/list.js");
var Path = require("path");
var $$Array = require("bs-platform/lib/js/array.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var inputFilePath = Path.join(__dirname, "./input.txt");

function readFile(param) {
  return Fs.readFileSync(inputFilePath, "utf8").split("\n");
}

function generatePairs(lines) {
  var _idx1 = 0;
  var _idx2 = 1;
  var _generatedPairs = /* :: */[
    /* tuple */[
      Caml_array.caml_array_get(lines, 0),
      Caml_array.caml_array_get(lines, 0)
    ],
    /* [] */0
  ];
  while(true) {
    var generatedPairs = _generatedPairs;
    var idx2 = _idx2;
    var idx1 = _idx1;
    var match = idx1 === (lines.length - 2 | 0);
    var match$1 = idx2 === (lines.length - 1 | 0);
    if (match) {
      if (match$1) {
        return generatedPairs;
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "day2_2.re",
                21,
                23
              ]
            ];
      }
    } else {
      _generatedPairs = /* :: */[
        /* tuple */[
          Caml_array.caml_array_get(lines, idx1),
          Caml_array.caml_array_get(lines, idx2)
        ],
        generatedPairs
      ];
      if (match$1) {
        _idx2 = idx1 + 2 | 0;
        _idx1 = idx1 + 1 | 0;
        continue ;
      } else {
        _idx2 = idx2 + 1 | 0;
        continue ;
      }
    }
  };
}

function zipLinesByLetter(param) {
  return List.combine($$Array.to_list(param[0].split("")), $$Array.to_list(param[1].split("")));
}

function countMismatches(pair) {
  return List.fold_left((function (mismatches, param) {
                var match = param[0] !== param[1];
                return mismatches + (
                        match ? 1 : 0
                      ) | 0;
              }), 0, zipLinesByLetter(pair));
}

function linesDifferByOne(pair) {
  return countMismatches(pair) === 1;
}

function findLineWithoutDifferentLetter(_idx, _param) {
  while(true) {
    var param = _param;
    var idx = _idx;
    var line1 = param[0];
    if (line1.length === idx) {
      return "";
    } else {
      var line2 = param[1];
      var match = Caml_string.get(line1, idx) === Caml_string.get(line2, idx);
      if (match) {
        return $$String.make(1, Caml_string.get(line1, idx)) + findLineWithoutDifferentLetter(idx + 1 | 0, /* tuple */[
                    line1,
                    line2
                  ]);
      } else {
        _param = /* tuple */[
          line1,
          line2
        ];
        _idx = idx + 1 | 0;
        continue ;
      }
    }
  };
}

console.log(List.map((function (param) {
            return findLineWithoutDifferentLetter(0, param);
          }), List.filter(linesDifferByOne)(generatePairs(Fs.readFileSync(inputFilePath, "utf8").split("\n")))));

exports.inputFilePath = inputFilePath;
exports.readFile = readFile;
exports.generatePairs = generatePairs;
exports.zipLinesByLetter = zipLinesByLetter;
exports.countMismatches = countMismatches;
exports.linesDifferByOne = linesDifferByOne;
exports.findLineWithoutDifferentLetter = findLineWithoutDifferentLetter;
/* inputFilePath Not a pure module */