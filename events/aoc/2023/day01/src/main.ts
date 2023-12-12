import readline from "node:readline";

interface Map {
  [key: string]: number;
}

const DigitNameToDigitValue: Map = {
  "1": 1,
  "2": 2,
  "3": 3,
  "4": 4,
  "5": 5,
  "6": 6,
  "7": 7,
  "8": 8,
  "9": 9,

  one: 1,
  two: 2,
  three: 3,
  four: 4,
  five: 5,
  six: 6,
  seven: 7,
  eight: 8,
  nine: 9,
};

function isNumber(str: string): boolean {
  return str.length === 1 && !isNaN(Number(str));
}

function findDigitByName(name: string): number {
  return DigitNameToDigitValue[name];
}

function sum(numbers: number[]): number {
  return numbers.reduce((acc, value) => acc + value, 0);
}

function toArrayOfNumbers(str: string): number[] {
  return str.split("").map((ch) => Number(ch));
}

function joinFirstAndLastDigits(numbers: number[]): number {
  const [first, last]: number[] = [numbers.at(0) || 0, numbers.at(-1) || 0];
  return first * 10 + last;
}

function filterDigitNameAndNumberFromString(str: string): string {
  const removeNonDigitChars = (
    acc: string,
    _: string,
    index: number,
    array: string[],
  ): string => {
    for (let j = index; j <= array.length; j++) {
      const name = array.slice(index, j).join("");

      const digit = findDigitByName(name);
      if (!!digit && digit > 0) return acc + digit;
    }

    return acc;
  };

  return str.split("").reduce(removeNonDigitChars, "");
}

function filterSingleDigitFromString(str: string): string {
  return str
    .split("") // creates an array with each letter as item
    .filter(isNumber)
    .join("");
}

type FilterFunction = (text: string) => string;

function calibrationValues(
  lines: string[],
  filterByDigitName?: boolean,
): number[] {
  const filterFunction: FilterFunction = filterByDigitName
    ? filterDigitNameAndNumberFromString
    : filterSingleDigitFromString;

  return lines
    .map(filterFunction)
    .map(toArrayOfNumbers)
    .map(joinFirstAndLastDigits);
}

function entrypoint(lines: string[]): void {
  console.log(`Part 1: ${sum(calibrationValues(lines))}`);
  console.log(`Part 2: ${sum(calibrationValues(lines, true))}`);
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
