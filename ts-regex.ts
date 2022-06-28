// https://support.bettercloud.com/s/article/Creating-your-own-Custom-Regular-Expression-bc72153

import { CharTable, InvertedCharTable } from './char-table';

// utils

// counter logic
type DecreaseDigitMap = {
    "9": "8",
    "8": "7",
    "7": "6",
    "6": "5",
    "5": "4",
    "4": "3",
    "3": "2",
    "2": "1",
    "1": "0"
};

type Decrease<T extends string> =
  T extends `0${infer A}`
  ? Decrease<A>
  : T extends ""
  ? ""
  : T extends keyof DecreaseDigitMap
  ? DecreaseDigitMap[T]
  : T extends `${infer A}0`
  ? (Decrease<A> extends 0 ? "9" : `${Decrease<A>}9`)
  : T extends `${infer A}${infer B}`
  ? `${A}${Decrease<B>}`
  : "0";

type IncreaseDigitMap = {
    "0": "1",
    "1": "2",
    "2": "3",
    "3": "4",
    "4": "5",
    "5": "6",
    "6": "7",
    "7": "8",
    "8": "9",
};

type Increase<T extends string> =
  T extends ""
  ? ""
  : T extends keyof IncreaseDigitMap
  ? IncreaseDigitMap[T]
  : T extends `${infer A}9`
  ? (Increase<A> extends "" ? "10" : `${Increase<A>}0`)
  : T extends `${infer A}${infer B}`
  ? `${A}${Increase<B>}`
  : "0";

// range logic
type CharTableRange<from extends string, to extends string> =
  from extends to
  ? (from extends keyof CharTable ? CharTable[from] : never)
  : from extends keyof CharTable
  ? CharTable[from] | CharTableRange<Increase<from>, to>
  : CharTableRange<Increase<from>, to>;

type IsKeyofCharTable<T extends string> =
  T extends keyof CharTable ? true : false;
type IsKeyofInvertedCharTable<T extends string> =
  T extends keyof InvertedCharTable ? true : false;
type CharRange<range extends string> =
  range extends `${infer from}-${infer to}${infer rest}`
    ? from extends keyof InvertedCharTable
      ? to extends keyof InvertedCharTable
        ? CharTableRange<InvertedCharTable[from], InvertedCharTable[to]> | CharRange<rest>
        : never
      : never
    : range extends `${infer char}${infer rest}`
      ? char | CharRange<rest>
      : never;

// basic symbols
type whitespace = "\n" | " " | "    ";
type digit = CharRange<"0-9">;
type word = CharRange<"a-zA-Z0-9">;
type anyChar = whitespace | digit | word;

type IsWhitespace<str extends string> = str extends whitespace ? true : false;
type IsDigit<str extends string> = str extends digit ? true : false;
type IsWord<str extends string> = str extends word ? true : false;
type IsAnyChar<str extends string> = str extends anyChar ? true : false;

type numberString = `${number}`;
type IsNumberString<str extends string> = str extends numberString ? true : false;


type Quantified<T extends string, count extends string> = count extends "0" ? "" : `${T}${Quantified<T, Decrease<count>>}`;

type And<A extends boolean, B extends boolean, C extends boolean = true, D extends boolean = true, E extends boolean = true, F extends boolean = true> =
    A extends true ? B extends true ? C extends true ? D extends true ? E extends true ? F extends true ? true : false : false : false : false : false : false;
type Or<A extends boolean, B extends boolean, C extends boolean = false, D extends boolean = false, E extends boolean = false, F extends boolean = false> =
    A extends true ? true : B extends true ? true : C extends true ? true : D extends true ? true : E extends true ? true : F extends true ? true : false;


// regex components
// TODO is-token-checks required?
type IsGroup<regex extends string> = regex extends `(${"?:" | ""}${string})${string}` ? true : false;
type IsGroupToken<regex extends string> = regex extends `(${"?:" | ""}${string})` ? true : false;
type Group<regex extends string> = regex extends `(${"?:" | ""}${infer groupContent})${infer rest}` ? `${Regex<groupContent>}${Regex<rest>}` : never;

type IsWordSymbol<regex extends string> = regex extends `\w${string}` ? true : false;
type IsWordSymbolToken<regex extends string> = regex extends `\w` ? true : false;
type WordSymbol<regex extends string> = regex extends `\w${infer rest}` ? `${word}${Regex<rest>}` : never;

type IsToken<regex extends string> = Or<IsWordSymbolToken<regex>, IsGroupToken<regex>, IsAnyChar<regex>, IsCharRangeGroupToken<regex>>;

type IsQuantifier<regex extends string> =
    regex extends `${infer token}{${infer quantity}}${string}`
    ? And<IsToken<token>, IsNumberString<quantity>> extends true
    ? true : false : false;
type Quantifier<regex extends string> =
    regex extends `${infer token}{${infer quantity}}${infer rest}`
    ? `${Quantified<Regex<token>, quantity>}${Regex<rest>}`
    : never;

type IsCharRangeGroup<regex extends string> = regex extends `[${string}]${string}` ? true : false;
type IsCharRangeGroupToken<regex extends string> = regex extends `[${string}]` ? true : false;
type CharRangeGroup<regex extends string> =
  regex extends `[${infer range}]${infer rest}`
    ? `${CharRange<range>}${Regex<rest>}`
    : never;

// main type
type Regex<S extends string> =
    // S extends `${infer A}{${infer B}}${infer C}`
    // ? `${Quantified<Regex<A>, B>}${Regex<C>}`
    // : S extends `(${"?:" | ""}${infer A}){${infer B}}${infer C}`
    // ? `${Quantified<Regex<A>, B>}${Regex<C>}`
    // TODO add remaining quantified, *, +, ... -groups

    // : S extends `(${"?:" | ""}${infer A})${infer B}`
    // ? `${Regex<A>}${Regex<B>}`

    IsQuantifier<S> extends true ? Quantifier<S>
    : IsGroup<S> extends true ? Group<S>

    : S extends `${infer A}|${infer B}`
    ? Regex<A> | Regex<B>

    : IsCharRangeGroup<S> extends true ? CharRangeGroup<S>

    : S extends `\w${infer A}`
    ? `${word}${Regex<A>}`

    : S extends `\d${infer A}`
    ? `${digit}${Regex<A>}`

    : S extends `\s${infer A}`
    ? `${whitespace}${Regex<A>}`

    : S extends `.${infer A}`
    ? `${anyChar}${Regex<A>}`
    : S;

// experiments
type X = "[a]" extends `[${infer A}]${infer B}` ? [A, B] : never;
type Y = "abcdef" extends `${infer A}${infer B}` ? [A, B] : never;

// testing library
type Pass = 'pass';
type Test<T, U> = [U] extends [T]
    ? Pass
    : { actual: T; expected: U };
type TestBothWays<T, U> = [T] extends [U]
    ? [U] extends [T]
        ? Pass
        : { actual: T; expected: U }
    : { actual: T; expected: U };

function typeAssert<T extends Pass>() {}

// unit tests
typeAssert<TestBothWays<Regex<"">, "">>();
typeAssert<TestBothWays<Regex<"[abc]">, "a" | "b" | "c">>();
typeAssert<TestBothWays<Regex<"\w\d\s">, `${word}${digit}${whitespace}`>>();
typeAssert<Test<Regex<"\w\d\s">, "a3 ">>();
typeAssert<Test<Regex<"[abc]|\dxx">, "a" | "b" | "c" | `${digit}xx`>>();
typeAssert<Test<Regex<"(ab){4}">, "abababab">>();
typeAssert<Test<Regex<"a{10}">, "aaaaaaaaaa">>();
typeAssert<Test<Regex<"[a-zA-Z]{2}\d">, "aD4">>();