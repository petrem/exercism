export const score = (word) => [...word].reduce((acc, letter) =>
  acc + letterScore(letter), 0);

const letterScore = (letter) => LetterScores.get(letter.toUpperCase());

const Scores = [
  [1, [..."AEIOULNRST"]],
  [2, ["D", "G"]],
  [3, [..."BCMP"]],
  [4, [..."FHVWY"]],
  [5, ["K"]],
  [8, ["J", "X"]],
  [10, ["Q", "Z"]]
];
const LetterScores = Scores.reduce(
  (accExt, [ls, letters]) => letters.reduce(
    (acc, letter) => acc.set(letter, ls)
    , accExt)
  , new Map());
