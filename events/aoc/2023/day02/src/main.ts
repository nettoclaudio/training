import readline from "node:readline";

interface Round {
  red: number;
  green: number;
  blue: number;
}

interface Game {
  id: number;
  rounds: Round[];
}

const MAX_CUBES_PER_ROUND: Round = {
  red: 12,
  green: 13,
  blue: 14,
};

function emptyRound(): Round {
  return {green: 0, blue: 0, red: 0};
}

function sum(n: number[]): number {
  return n.reduce((acc, n) => acc + n, 0);
}

function parse(lines: string[]): Game[] {
  const parseGameID = (str: string): number => {
    const [, id]: string[] = str.split(" ");
    return parseInt(id);
  };

  const parseRounds = (str: string): Round[] =>
    str.split(";").map(parseRound);

  const parseRound = (str: string): Round => {
    const round: Round = emptyRound();

    str.split(",").forEach((roundStr: string): void => {
      const [, cubesStr, color] = roundStr.split(" ");

      const cubes: number = parseInt(cubesStr);

      if (color === "red")
        round.red = cubes;

      if (color === "green")
        round.green = cubes;

      if (color === "blue")
        round.blue = cubes;
    });

    return round;
  }

  return lines.map((line) => {
    const [p1, p2]: string[] = line.split(":");

    return {
      id: parseGameID(p1),
      rounds: parseRounds(p2),
    };
  });
}

function validGames(games: Game[]): Game[] {
  const isRoundValid = (r: Round): boolean =>
    r.red <= MAX_CUBES_PER_ROUND.red && r.green <= MAX_CUBES_PER_ROUND.green && r.blue <= MAX_CUBES_PER_ROUND.blue;

  return games.filter((g: Game): boolean =>
    g.rounds
      .map(isRoundValid)
      .filter((valid: boolean): boolean => !valid)
      .length === 0
  );
}

function sumGameIDs(games: Game[]): number {
  return sum(games.map((game) => game.id));
}

function powerOfMinSetOfCubes(games: Game[]): number[] {
  const minSetOfCubes = (g: Game): Round => ({
    red: g.rounds.reduce((greatest, r) => Math.max(r.red, greatest), 0),
    green: g.rounds.reduce((greatest, r) => Math.max(r.green, greatest), 0),
    blue: g.rounds.reduce((greatest, r) => Math.max(r.blue, greatest), 0),
  });

  const multiplyCubes = (r: Round): number =>
    r.red * r.green * r.blue;

  return games.map(minSetOfCubes).map(multiplyCubes);
}

function entrypoint(lines: string[]): void {
  const games: Game[] = parse(lines);

  console.log('Part 01:', sumGameIDs(validGames(games)));
  console.log('Part 02:', sum(powerOfMinSetOfCubes(games)));
}

const readInputText = (callback: (lines: string[]) => void): void => {
  const lines: string[] = [];

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  rl.on("line", (line: string) => {
    lines.push(line);
  });

  rl.on("close", () => {
    callback(lines);
  });
};

readInputText(entrypoint);
