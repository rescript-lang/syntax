let customOperatorChars = [
  "!",
  "$",
  "%",
  "&",
  "*",
  "+",
  "-",
  ".",
  "/",
  ":",
  "<",
  "=",
  ">",
  "?",
  "@",
  "^",
  "|",
  "~",
];

function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min) + min); //The maximum is exclusive and the minimum is inclusive
}

function logDeclaration(operator) {
  console.log(`let (${operator}) = (a, b) => a + b`);
  console.log(`external (${operator}): (int, int) => int = "js_call"`);
}

function logCall(operator) {
  console.log(`2 ${operator} 1`);
}

function generateOperators(prefix) {
  logDeclaration(prefix);
  logCall(prefix);

  customOperatorChars.forEach((char) => {
    // generate a suffix of length between 1 and 6
    const count = getRandomInt(1, 6);
    let suffix = "";
    for (let i = 0; i < count; i++) {
      suffix += char;
    }
    logDeclaration(prefix + suffix);
    logCall(prefix + suffix);
  });
}

generateOperators("**");
