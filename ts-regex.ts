// https://support.bettercloud.com/s/article/Creating-your-own-Custom-Regular-Expression-bc72153

import { CharTable, InvertedCharTable } from './char-table';

                                                                      //////////////////////////////////////////////////
                                                                          ////////////////////////////////////// DEMO //
                                                                              //////////////////////////////////////////
// --- Positive examples; These all evaluate to true
type DemoPositive01 = Match<"[a-zA-Z]{5}", "Regex">;
type DemoPositive02 = Match<"\w\d\d", "X45">;
type DemoPositive03 = Match<"(\w{5}123)|\d", "hello123">;

// --- Negative examples; These all evaluate to false
type DemoNegative01 = Match<"[a-zA-Z]{5}", "too long">;
type DemoNegative02 = Match<"\w\d\d", "123">;
type DemoNegative03 = Match<"(\w{5}123)|\d", "xxx">;

// For more examples, scroll to the bottom where more unit tests verify more functionality.



                                                                      //////////////////////////////////////////////////
                                                                          ///////////////////////////////// UTILITIES //
                                                                              //////////////////////////////////////////

type And<A extends boolean, B extends boolean, C extends boolean = true, D extends boolean = true,
  E extends boolean = true, F extends boolean = true> =
  A extends true ? B extends true ? C extends true ? D extends true ? E extends true ? F extends true ?
    true : false : false : false : false : false : false;
type Or<A extends boolean, B extends boolean, C extends boolean = false, D extends boolean = false,
  E extends boolean = false, F extends boolean = false> =
  A extends true ? true : B extends true ? true : C extends true ? true : D extends true ? true : E extends true
    ? true : F extends true ? true : false;
type Not<T extends boolean> = T extends true ? false : true;
type IsEmpty<T extends string> = T extends "" ? true : false;
type IsNotEmpty<T extends string> = Not<IsEmpty<T>>;
type IsEqual<A extends string, B extends string> = And<A extends B ? true : false, B extends A ? true : false>;
type IsInequal<A extends string, B extends string> = Not<IsEqual<A, B>>;
type Extends<A, B> = A extends B ? true : false;

                                                                      //////////////////////////////////////////////////
                                                                          ///////////////////////////// COUNTER LOGIC //
                                                                              //////////////////////////////////////////

// ----------------------------------------------------------------------------------------------------- Decrease --- //
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


// ----------------------------------------------------------------------------------------------------- Increase --- //
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


// --------------------------------------------------------------------------------------------------- Comparison --- //
type IsLonger<a extends string, b extends string> =
  a extends `${infer a1}${infer a2}`
    ? b extends `${infer b1}${infer b2}`
      ? And<IsNotEmpty<a2>, IsNotEmpty<b2>> extends true
          ? IsLonger<a2, b2>
      : IsNotEmpty<a2>
    : never
  : never;
type IsDigitLarger<a extends string, b extends string, counter extends string = "0"> =
  counter extends "10"
    ? false
    : a extends b
      ? false
      : a extends counter
        ? false
        : b extends counter
          ? true
          : IsDigitLarger<a, b, Increase<counter>>;
type IsLarger<a extends string, b extends string> =
  IsEqual<a, b> extends true
    ? false
    : IsLonger<a, b> extends true
      ? true
      : IsLonger<b, a> extends true
        ? false
        : a extends `${infer a1}${infer a2}`
          ? b extends `${infer b1}${infer b2}`
            ? IsEqual<a1, b1> extends true
              ? IsLarger<a2, b2>
              : IsDigitLarger<a1, b1>
            : never
          : never;
type IsSmaller<a extends string, b extends string> = And<Not<IsLarger<a, b>>, Not<IsEqual<a, b>>>;
type IsLargerEquals<a extends string, b extends string> = Or<IsLarger<a, b>, IsEqual<a, b>>;
type IsSmallerEquals<a extends string, b extends string> = Or<IsSmaller<a, b>, IsEqual<a, b>>;



                                                                      //////////////////////////////////////////////////
                                                                          /////////////////////////////// RANGE LOGIC //
                                                                              //////////////////////////////////////////
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

type RepeatString<text extends string, count extends string> =
  count extends "0" ? "" : `${text}${RepeatString<text, Decrease<count>>}`;



                                                                      //////////////////////////////////////////////////
                                                                          ///////////////////////////// BASIC SYMBOLS //
                                                                              //////////////////////////////////////////
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



                                                                      //////////////////////////////////////////////////
                                                                          ////////////////////////// REGEX COMPONENTS //
                                                                              //////////////////////////////////////////
// ------------------------------------------------------------------------------------------------------- Groups --- //
type IsGroup<regex extends string> = regex extends `(${"?:" | ""}${string})${string}` ? true : false;
type IsGroupToken<regex extends string> = regex extends `(${"?:" | ""}${string})` ? true : false;
type Group<regex extends string> =
  regex extends `(${"?:" | ""}${infer groupContent})${infer rest}`
    ? `${Regex<groupContent>}${Regex<rest>}`
    : never;


// -------------------------------------------------------------------------------------------- Character classes --- //
type IsWordSymbol<regex extends string> = regex extends `\w${string}` ? true : false;
type IsWordSymbolToken<regex extends string> = regex extends `\w` ? true : false;
type WordSymbol<regex extends string> = regex extends `\w${infer rest}` ? `${word}${Regex<rest>}` : never;

// TODO is-token-checks required?
type IsToken<regex extends string> =
  Or<IsWordSymbolToken<regex>, IsGroupToken<regex>, IsAnyChar<regex>, IsCharRangeGroupToken<regex>>;


// -------------------------------------------------------------------------------------------------- Quantifiers --- //
type IsQuantifier<regex extends string> =
    regex extends `${infer token}{${infer quantity}}${string}`
    ? And<IsToken<token>, IsNumberString<quantity>> extends true
    ? true : false : false;
type Quantifier<regex extends string> =
    regex extends `${infer token}{${infer quantity}}${infer rest}`
    ? `${RepeatString<Regex<token>, quantity>}${Regex<rest>}`
    : never;
type MatchQuantifier<regex extends string, test extends string> =
  regex extends `${infer token}{${infer quantity}}${infer regexRest}`
    ? test extends `${RepeatString<Regex<token>, quantity>}${infer testRest}`
      ? Match<regexRest, testRest>
      : false
    : false;

// returns remaining string or never if not matched
type ProcessQuantifier<
  regexPart extends string, test extends string, min extends string, max extends string, count extends string = "0"> =
  IsLargerEquals<count, max> extends true
    ? test
    : test extends `${Regex<regexPart>}${infer testRest}`
      ? ProcessQuantifier<regexPart, testRest, min, max, Increase<count>>
      : IsSmaller<count, min> extends true
        ? never
        : test;
type ParseQuantityMin<quantity extends string> =
  quantity extends numberString
    ? quantity
    : quantity extends `${infer min},${string}`
      ? min
      : "0";
type ParseQuantityMax<quantity extends string> =
  quantity extends numberString
    ? quantity
    : quantity extends `${string},${infer max}`
      ? max
      : "100";
type MatchQuantifier2<regex extends string, test extends string> =
  regex extends `${infer token}{${infer quantity}}${infer regexRest}`
    ? ProcessQuantifier<token, test, ParseQuantityMin<quantity>, ParseQuantityMax<quantity>> extends false
      ? false
      : Match<regexRest, ProcessQuantifier<token, test, ParseQuantityMin<quantity>, ParseQuantityMax<quantity>>>
    : false;


// ------------------------------------------------------------------------------------------------- Range groups --- //
type IsCharRangeGroup<regex extends string> = regex extends `[${string}]${string}` ? true : false;
type IsCharRangeGroupToken<regex extends string> = regex extends `[${string}]` ? true : false;
type CharRangeGroup<regex extends string> =
  regex extends `[${infer range}]${infer rest}`
    ? `${CharRange<range>}${Regex<rest>}`
    : never;
type MatchCharRangeGroup<regex extends string, test extends string> =
  regex extends `[${infer range}]${infer regexRest}`
    ? test extends `${CharRange<range>}${infer testRest}`
      ? Match<regexRest, testRest>
      : false
    : false;



                                                                      //////////////////////////////////////////////////
                                                                          ////////////////////////////// MATCHER TYPE //
                                                                              //////////////////////////////////////////
type Match<regex extends string, test extends string> =
  And<regex extends "" ? true : false, test extends "" ? true : false> extends true ? true
  : test extends never ? false
  : IsQuantifier<regex> extends true ? MatchQuantifier2<regex, test>
  : IsCharRangeGroup<regex> extends true ? MatchCharRangeGroup<regex, test>
  : false;




                                                                      //////////////////////////////////////////////////
                                                                          //////////////////////////////// REGEX TYPE //
                                                                              //////////////////////////////////////////
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



                                                                      //////////////////////////////////////////////////
                                                                          /////////////////////INSIGHTFUL EXPERIMENTS //
                                                                              //////////////////////////////////////////
type X = "[a]" extends `[${infer A}]${infer B}` ? [A, B] : never; // ["a", ""]
type Y = "abcdef" extends `${infer A}${infer B}` ? [A, B] : never; // ["a", "bcdef"]
type Z = "a" extends `${infer A}${infer B}` ? [A, B] : never; // ["a", ""]



                                                                      //////////////////////////////////////////////////
                                                                          /////////////////////////// TESTING LIBRARY //
                                                                              //////////////////////////////////////////
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
function assertMatch<T extends true>() {}
function assertNoMatch<T extends false>() {}



                                                                      //////////////////////////////////////////////////
                                                                          //////////////////////////////// UNIT TESTS //
                                                                              //////////////////////////////////////////
typeAssert<Test<ParseQuantityMin<"3,4">, "3">>();
typeAssert<Test<ParseQuantityMax<"3,4">, "4">>();
typeAssert<Test<ProcessQuantifier<"a", "aaaaaaax", "1", "6">, "ax">>();
typeAssert<Test<ProcessQuantifier<"a", "aaxxxxxx", "1", "6">, "xxxxxx">>();
typeAssert<Test<ProcessQuantifier<"a", "axxxxxxx", "2", "6">, never>>();
assertMatch<MatchQuantifier2<"[a-zA-Z]{4,5}", "aaDSa">>();
assertNoMatch<MatchQuantifier2<"[a-zA-Z]{4,5}", "aaDSDa">>();

typeAssert<TestBothWays<Regex<"">, "">>();
typeAssert<TestBothWays<Regex<"[abc]">, "a" | "b" | "c">>();
typeAssert<TestBothWays<Regex<"\w\d\s">, `${word}${digit}${whitespace}`>>();
typeAssert<Test<Regex<"\w\d\s">, "a3 ">>();
typeAssert<Test<Regex<"[abc]|\dxx">, "a" | "b" | "c" | `${digit}xx`>>();
typeAssert<Test<Regex<"(ab){4}">, "abababab">>();
typeAssert<Test<Regex<"a{10}">, "aaaaaaaaaa">>();
typeAssert<Test<Regex<"[a-zA-Z]{2}\d">, "aD4">>();

assertMatch<Match<"", "">>();
assertMatch<Match<"[abc]", "a">>();
assertMatch<Match<"[abc][def]", "ad">>();
assertMatch<Match<"[a-z][A-Z0-9][0-9]", "aD4">>();
assertMatch<Match<"[a-z]{20}", "aaaaaaaaaaaaaaaaaaaa">>();

assertNoMatch<Match<"[abc][def]", "ag">>();