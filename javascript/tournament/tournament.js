export function tournamentTally(tournamentLog) {
  const matches = getMatchesFromLog(tournamentLog);
  const fullTally = matches.reduce((tally, match) =>
    updatedTally(tally, match), {});
  const sortedTally = Object.entries(fullTally).sort(compareTeams);
  return [header, ...sortedTally].map(printTallyEntry).join("\n");
}

const getMatchesFromLog = 
  (tournamentLog) => tournamentLog
    .split("\n")
    .filter(Boolean)
    .map((match) => match.split(";"));

const updatedTally = (tally, [teamA, teamB, result]) => {
  const [updateA, updateB] = updatesByResult[result];
  tally[teamA] = addVectors(tally[teamA] ?? [0,0,0,0,0], updateA);
  tally[teamB] = addVectors(tally[teamB] ?? [0,0,0,0,0], updateB);
  return tally;
};

const addVectors = (array1, array2) => array1.map((x, idx) => x + array2[idx]);

const updatesByResult = {
  // MP, W, D, L, P
  "win": [[1, 1, 0, 0, 3], [1, 0, 0, 1, 0]],
  "loss": [[1, 0, 0, 1, 0], [1, 1, 0, 0, 3]],
  "draw": [[1, 0, 1, 0, 1], [1, 0, 1, 0, 1]],
};

const compareTeams = ([teamA, resultsA], [teamB, resultsB]) =>
  resultsB[4] - resultsA[4] || teamA.localeCompare(teamB);

const printTallyEntry = ([team, results]) => [
    team.padEnd(teamNameWidth),
    ...results.map((r) => String(r).padStart(countWidth))
  ].join(" |");

const header = ["Team", ["MP", "W", "D", "L", "P"]];
const teamNameWidth = 30;
const countWidth = 3;
