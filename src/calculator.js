// Core math and expression evaluation engine for the calculator

function sanitizeExpression(expr) {
  if (typeof expr !== "string") {
    throw new Error("Invalid expression");
  }
  return expr
    .replace(/×/g, "*")
    .replace(/÷/g, "/")
    .replace(/−/g, "-")
    .replace(/\bpi\b/gi, `(${Math.PI})`)
    .replace(/π/g, `(${Math.PI})`)
    .trim();
}

function roundToPrecision(num, decimals = 10) {
  if (typeof num !== "number" || !Number.isFinite(num)) {
    return num;
  }
  const factor = 10 ** decimals;
  return Math.round((num + Number.EPSILON) * factor) / factor;
}

// Tokenizer and Recursive Descent Parser for safe mathematical evaluation
function tokenize(input) {
  const tokens = [];
  let i = 0;
  const str = input.replace(/\s+/g, "");

  while (i < str.length) {
    const char = str[i];

    // Numbers (including decimals and scientific notation like 1e-4)
    if ("0123456789.".includes(char)) {
      let numStr = "";
      let hasDot = false;
      while (i < str.length) {
        const c = str[i];
        if ("0123456789".includes(c)) {
          numStr += c;
          i++;
        } else if (c === ".") {
          if (hasDot) throw new Error("Invalid number format: multiple decimal points");
          hasDot = true;
          numStr += c;
          i++;
        } else {
          break;
        }
      }
      tokens.push({ type: "NUMBER", value: parseFloat(numStr) });
      continue;
    }

    // Binary / Unary operators
    if (char === "+" || char === "-" || char === "*" || char === "/" || char === "%" || char === "^") {
      tokens.push({ type: "OP", value: char });
      i++;
      continue;
    }

    // Parentheses
    if (char === "(" || char === ")") {
      tokens.push({ type: "PAREN", value: char });
      i++;
      continue;
    }

    // Mathematical functions (sqrt, abs, sin, cos, tan, ln, log)
    const matchFunc = str.slice(i).match(/^(sqrt|abs|sin|cos|tan|ln|log)\b/i);
    if (matchFunc) {
      tokens.push({ type: "FUNC", value: matchFunc[1].toLowerCase() });
      i += matchFunc[0].length;
      continue;
    }

    throw new Error(`Unexpected character: '${char}'`);
  }

  return tokens;
}

class Parser {
  constructor(tokens) {
    this.tokens = tokens;
    this.pos = 0;
  }

  peek() {
    return this.tokens[this.pos];
  }

  consume(expectedType, expectedValue) {
    const token = this.peek();
    if (!token) throw new Error("Unexpected end of expression");
    if (expectedType && token.type !== expectedType) {
      throw new Error(`Expected ${expectedType} but got ${token.type}`);
    }
    if (expectedValue && token.value !== expectedValue) {
      throw new Error(`Expected '${expectedValue}' but got '${token.value}'`);
    }
    this.pos++;
    return token;
  }

  // expr := term ( ('+' | '-') term )*
  parseExpr() {
    let result = this.parseTerm();

    while (this.peek() && this.peek().type === "OP" && (this.peek().value === "+" || this.peek().value === "-")) {
      const op = this.consume().value;
      const right = this.parseTerm();
      if (op === "+") {
        result += right;
      } else {
        result -= right;
      }
      result = roundToPrecision(result);
    }

    return result;
  }

  // term := power ( ('*' | '/' | '%') power )*
  parseTerm() {
    let result = this.parsePower();

    while (this.peek() && this.peek().type === "OP" && (this.peek().value === "*" || this.peek().value === "/" || this.peek().value === "%")) {
      const op = this.consume().value;
      const right = this.parsePower();
      if (op === "*") {
        result *= right;
      } else if (op === "/") {
        if (right === 0) {
          throw new Error("Cannot divide by zero");
        }
        result /= right;
      } else if (op === "%") {
        if (right === 0) {
          throw new Error("Cannot divide by zero in modulo");
        }
        result = result % right;
      }
      result = roundToPrecision(result);
    }

    return result;
  }

  // power := factor ( '^' power )*  (right-associative)
  parsePower() {
    let result = this.parseFactor();

    if (this.peek() && this.peek().type === "OP" && this.peek().value === "^") {
      this.consume();
      const exponent = this.parsePower();
      result = Math.pow(result, exponent);
      result = roundToPrecision(result);
    }

    return result;
  }

  // factor := ('+' | '-') factor | FUNC '(' expr ')' | '(' expr ')' | NUMBER
  parseFactor() {
    const token = this.peek();
    if (!token) {
      throw new Error("Unexpected end of expression");
    }

    // Unary plus or minus
    if (token.type === "OP" && (token.value === "+" || token.value === "-")) {
      const op = this.consume().value;
      const factor = this.parseFactor();
      return op === "-" ? -factor : factor;
    }

    // Functions: sqrt, abs, sin, cos, tan, ln, log
    if (token.type === "FUNC") {
      const funcName = this.consume().value;
      this.consume("PAREN", "(");
      const arg = this.parseExpr();
      this.consume("PAREN", ")");

      let res;
      switch (funcName) {
        case "sqrt":
          if (arg < 0) throw new Error("Cannot calculate square root of negative number");
          res = Math.sqrt(arg);
          break;
        case "abs":
          res = Math.abs(arg);
          break;
        case "sin":
          res = Math.sin(arg);
          break;
        case "cos":
          res = Math.cos(arg);
          break;
        case "tan":
          res = Math.tan(arg);
          break;
        case "ln":
          if (arg <= 0) throw new Error("Logarithm undefined for non-positive numbers");
          res = Math.log(arg);
          break;
        case "log":
          if (arg <= 0) throw new Error("Logarithm undefined for non-positive numbers");
          res = Math.log10(arg);
          break;
        default:
          throw new Error(`Unknown function: ${funcName}`);
      }
      return roundToPrecision(res);
    }

    // Parenthesized subexpression
    if (token.type === "PAREN" && token.value === "(") {
      this.consume("PAREN", "(");
      const val = this.parseExpr();
      this.consume("PAREN", ")");
      return val;
    }

    // Number literal
    if (token.type === "NUMBER") {
      this.consume("NUMBER");
      return token.value;
    }

    throw new Error(`Unexpected token '${token.value || token.type}'`);
  }
}

function evaluate(expr) {
  if (!expr || typeof expr !== "string" || expr.trim() === "") {
    throw new Error("Empty expression");
  }

  const sanitized = sanitizeExpression(expr);
  const tokens = tokenize(sanitized);
  if (tokens.length === 0) {
    throw new Error("Empty expression");
  }

  const parser = new Parser(tokens);
  const result = parser.parseExpr();

  if (parser.pos < tokens.length) {
    throw new Error("Extra tokens at end of expression");
  }

  if (!Number.isFinite(result) || Number.isNaN(result)) {
    throw new Error("Result is undefined or infinite");
  }

  return roundToPrecision(result);
}

module.exports = {
  evaluate,
  roundToPrecision,
  sanitizeExpression,
  tokenize
};
