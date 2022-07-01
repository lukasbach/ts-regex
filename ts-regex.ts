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
                                                                          ///////////////////////////////// GUIDELINE //
                                                                              //////////////////////////////////////////
/**
 * # Recurring semantics
 * - Component: regex syntax component, such as groups, bracket expressions, ...
 * - Producer method, i.e. `ProduceGroup`: A type that takes a regex, processes the next token,
 *   and produces all strings that match that token, and continues to process the remaining string
 *   with the general `Regex` producer.
 * - Matcher method, i.e. `MatchGroup`: takes a regex and a test string, matches if the next token
 *   in both match, and if so, continues to match the rest with the general `Match` matcher.
 * - Component tester, i.e. anything in `ComponentTests` as well as `isXxx` methods: contains a general
 *   string that matches the component, for example `[${string}]` for a bracket expression.
 *   Match with
 *   - `Extends<token, ComponentTests["xxx"]>` to test if a token matches that component, or
 *   - `StartsWith<regex, ComponentTests["xxx"]>` to test if the next token in a regex matches that component.
 */


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
type StartsWith<str extends string, prefix extends string> = Extends<str, `${prefix}${string}`>;
type IfElse<cond extends boolean, then extends any, otherwise extends any> = cond extends true ? then : otherwise;
type Contains<str extends string, substring extends string> = Extends<str, `${string}${substring}${string}`>;

// Nullish Coalescing
type NuCo<Value, Otherwise> =
  IfElse<Or<Extends<Value, "">, Extends<Value, false>, Extends<Value, never>>, Otherwise, Value>;

type NotNullish<value> = value extends null | false ? never : value;

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
type someTest = string | boolean;

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
// ---------------------------------------------------------------------------------------------- Component utils --- //
// type StartsWithComponent<componentTest extends string, regex extends string> = Extends<regex, `${componentTest}${string}`>;
// type IsComponentToken<componentTest extends string, regex extends string> = Extends<regex, componentTest>;


// -------------------------------------------------------------------------------------------- Component testers --- //
interface ComponentTests {
    or: `${string}|${string}`;
    group: `(${"?:" | ""}${string})`;
    word: `\w`;
    bracketExpr: `[${string}]`;
}


// ----------------------------------------------------------------------------------------------------------- Or --- //
type ProduceOr<regex extends string> =
  regex extends `${infer a}|${infer b}`
    ? Regex<a> | Regex<b>
    : never;
type MatchOr<regex extends string, test extends someTest> =
  regex extends `${infer a}|${infer b}`
    ? NotNullish<
      NuCo<
        Or<Match<a, test>, Match<b, test>>,
        Match<a, test> | Match<b, test>
        >
      >
    : false;
type IsOr<regex extends string> =
  regex extends `${infer start}|${string}`
    ? Not<Contains<start, "(">>
    : false;
type test = 'bb(c|d)' extends `${infer start}|${string}` ? start : never;
type testxasdasd = MatchOr<"b|c", "be">


// ------------------------------------------------------------------------------------------------------- Groups --- //
type ProduceGroup<regex extends string> =
  regex extends `(${"?:" | ""}${infer groupContent})${infer rest}`
// TODO    ? ProduceRemainingGroup<groupContent, rest>
     ? Regex<`${Regex<groupContent>}${rest}`>
//    ? `${Regex<groupContent>}${Regex<rest>}`
    : never;
type ProduceRemainingGroup<groupContent extends string, rest extends string> =
    rest extends `${infer remainingGroup})${infer actualRest}`
      ? ProduceRemainingGroup<`${ groupContent })${ remainingGroup }`, actualRest>
      // : [groupContent, rest];
      // : Regex<`${Regex<groupContent>}${rest}`>;
      : `${Regex<groupContent>}${rest}`;
// ProduceRemainingGroup accounts for the case of nested groups, since ProduceGroup stops at the first occurance
// of a closing bracket.

type MatchGroup<regex extends string, test extends someTest> =
  regex extends `(${infer regexGroup})${infer regexRest}`
      // ? Match<regexRest, testRest>
      ? MatchRemainingGroup<regexGroup, regexRest, test>
    : false;
type MatchRemainingGroup<regexGroup extends string, regexRest extends string, test extends someTest> =
    regexRest extends `${infer regexRest1})${infer regexRest2}`
    ? MatchRemainingGroup<`${regexGroup})${regexRest1}`, regexRest2, test>
    /// : /*Match<regexGroup, test> extends true ? true :*/ Match<regexRest, /*NuCo<*/Match<regexGroup, test>/*, test>*/>;
    : Match<regexRest, IfElse<StartsWith<regexRest, "|">, NuCo<Match<regexGroup, test>, test>, Match<regexGroup, test>>>;

// "(b)c", "bc"
type debug10 = "(b)c" extends `(${infer regexGroup})${infer regexRest}` ? [regexGroup, regexRest] : never;
type partialMatch = Match<"b", "bc">





type sampleRegex = "(a|(b|c)|d)z";
type groupTest1 = MatchGroup<sampleRegex, "az">;
type groupTest2 = MatchGroup<sampleRegex, "bz">;
type groupTest3 = MatchGroup<sampleRegex, "cz">;
type groupTest4 = MatchGroup<sampleRegex, "dz">;
type groupTest5 = MatchGroup<sampleRegex, "ez">;

type debug = Match<"e(g|(ez))x", "egx">;
type debug20 = Match<"(g|(e))x", "gx">;
type debug21 = "(g|(e))x" extends `(${infer regexGroup})${infer regexRest}` ? [regexGroup, regexRest] : never;
type debug22 = ")x" extends `${infer regexRest1})${infer regexRest2}` ? [regexRest1, regexRest2] : never;
type debug23 = MatchRemainingGroup<`${"g|(e"})`, "x", "gx">
type debug24 = "x" extends `${infer regexRest1})${infer regexRest2}` ? [regexRest1, regexRest2] : never;
type debug25 = Match<"x", NuCo<Match<`${"g|(e"})`, "gx">, "gx">>;
type debug26 = NuCo<Match<`g|(e)`, "gx">, "gx">;
type debug27 = Match<`g|(e)`, "gx">;
type debug28 = Match<`(e)`, "gx">;
type debug29 = Match<`e`, "gx">;
type debug30 = Match<`(e)`, "gx">;
type debug31 = MatchRemainingGroup<`e`, "", "gx">;
type debug32 = Match<"", NuCo<Match<`e`, "gx">, "gx">>;
type debug33 = NuCo<Match<`e`, "gx">, "gx">;

type matchTest = Match<"abc|(bb(c|d))", "bbc">;
type matchTestx = Match<"bb(c|d)", "bbc">;
type matchTest2 = StartsWith<"abc|(bb(c|d))", ComponentTests["group"]>;
type matchTest3 = StartsWith<"abc|(bb(c|d))", ComponentTests["or"]>;
type matchTest4 = MatchOr<"abc|(bb(c|d))", "abc">;


// -------------------------------------------------------------------------------------------- Character classes --- //
type WordSymbol<regex extends string> = regex extends `\w${infer rest}` ? `${word}${Regex<rest>}` : never;

// TODO is-token-checks required?
type IsToken<regex extends string> =
  Or<
    Extends<regex, ComponentTests["word"]>,
    Extends<regex, ComponentTests["group"]>,
    Extends<regex, ComponentTests["bracketExpr"]>,
    IsAnyChar<regex>
  >;


// -------------------------------------------------------------------------------------------------- Quantifiers --- //
type IsQuantifier<regex extends string> =
    regex extends `${infer token}{${infer quantity}}${string}`
      //? And<IsToken<token>, IsNumberString<quantity>> extends true
      ? IsNumberString<quantity> extends true
    ? true : false : false;
type ProduceQuantifier<regex extends string> =
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
  regexPart extends string, test extends someTest, min extends string, max extends string, count extends string = "0"> =
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
type MatchQuantifier2<regex extends string, test extends someTest> =
  regex extends `${infer token}{${infer quantity}}${infer regexRest}`
    ? ProcessQuantifier<token, test, ParseQuantityMin<quantity>, ParseQuantityMax<quantity>> extends false
      ? false
      : Match<regexRest, ProcessQuantifier<token, test, ParseQuantityMin<quantity>, ParseQuantityMax<quantity>>>
    : false;


// ------------------------------------------------------------------------------------------------- Range groups --- //
type ProduceBracketExpr<regex extends string> =
  regex extends `[${infer range}]${infer rest}`
    ? `${CharRange<range>}${Regex<rest>}`
    : never;
type MatchBracketExpr<regex extends string, test extends someTest> =
  regex extends `[${infer range}]${infer regexRest}`
    ? test extends `${CharRange<range>}${infer testRest}`
      ? Match<regexRest, testRest>
      : false
    : false;



                                                                      //////////////////////////////////////////////////
                                                                          ////////////////////////////// MATCHER TYPE //
                                                                              //////////////////////////////////////////
type Match<regex extends string, test extends string | boolean> =
  test extends boolean ? test
  : And<regex extends "" ? true : false, test extends "" ? true : false> extends true ? true
  : regex extends "" ? test
  : test extends never ? false
  : StartsWith<regex, ComponentTests["group"]> extends true ? MatchGroup<regex, test>
  : IsOr<regex> extends true ? MatchOr<regex, test>
  : IsQuantifier<regex> extends true ? MatchQuantifier2<regex, test>
  : StartsWith<regex, ComponentTests["bracketExpr"]> extends true ? MatchBracketExpr<regex, test>
  : StartsWith<regex, anyChar> extends true ? MatchRegularChar<regex, test>
  : false;


type debug2 = Match<"(g|x)x", "gx">;


type MatchRegularChar<regex extends string, test extends someTest> =
  regex extends `${infer regexChar}${infer regexRest}`
    ? test extends `${infer testChar}${infer testRest}`
      ? IfElse<Extends<regexChar, testChar>, Match<regexRest, testRest>, false>
      : false
    : false;


                                                                      //////////////////////////////////////////////////
                                                                          //////////////////////////////// REGEX TYPE //
                                                                              //////////////////////////////////////////
type Regex<S extends string> =
    false extends true ? never // just for syntax so we can start the next line with a :
    // S extends `${infer A}{${infer B}}${infer C}`
    // ? `${Quantified<Regex<A>, B>}${Regex<C>}`
    // : S extends `(${"?:" | ""}${infer A}){${infer B}}${infer C}`
    // ? `${Quantified<Regex<A>, B>}${Regex<C>}`
    // TODO add remaining quantified, *, +, ... -groups

    // : S extends `(${"?:" | ""}${infer A})${infer B}`
    // ? `${Regex<A>}${Regex<B>}`

    // : StartsWith<S, anyChar> extends true ? (S extends `${infer char}${infer rest}` ? `${char}${Regex<rest>}` : never)
    : StartsWith<S, ComponentTests["group"]> extends true ? ProduceGroup<S>
    : StartsWith<S, ComponentTests["or"]> extends true ? ProduceOr<S>
    : IsQuantifier<S> extends true ? ProduceQuantifier<S>


    : StartsWith<S, ComponentTests["bracketExpr"]> extends true ? ProduceBracketExpr<S>

    : S extends `\\w${infer A}`
    ? `${word}${Regex<A>}`

    : S extends `\\d${infer A}`
    ? `${digit}${Regex<A>}`

    : S extends `\\s${infer A}`
    ? `${whitespace}${Regex<A>}`

    : S extends `.${infer A}`
    ? `${anyChar}${Regex<A>}`

    // : S extends `${infer char}${infer rest}` ? `${char}${Regex<rest>}` : S;
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
function assertNoMatch<T extends false | string>() {}



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
typeAssert<TestBothWays<Regex<"a|b|c">, "a" | "b" | "c">>();
typeAssert<TestBothWays<Regex<"abc|b|c">, "abc" | "b" | "c">>();
typeAssert<TestBothWays<Regex<"(b{2})|d">, "d" | "bb">>();
typeAssert<TestBothWays<Regex<"abc|(b{2})|(d|ef)">, "abc" | "bb" | "d" | "ef">>();
// typeAssert<TestBothWays<Regex<"abc|(bb(c|d))">, "abc" | "bbc" | "bbd">>();
typeAssert<TestBothWays<Regex<"\\w\\d\\s">, `${word}${digit}${whitespace}`>>();
typeAssert<Test<Regex<"\\w\\d\\s">, "a3 ">>();
typeAssert<Test<Regex<"[abc]|\\dxx">, "a" | "b" | "c" | `${digit}xx`>>();
typeAssert<Test<Regex<"(ab){4}">, "abababab">>();
typeAssert<Test<Regex<"a{10}">, "aaaaaaaaaa">>();
typeAssert<Test<Regex<"[a-zA-Z]{2}\\d">, "aD4">>();

assertMatch<Match<"", "">>();
assertMatch<Match<"abc", "abc">>();
assertMatch<Match<"[abc]", "a">>();
assertMatch<Match<"[abc][def]", "ad">>();
assertMatch<Match<"[a-z][A-Z0-9][0-9]", "aD4">>();
assertMatch<Match<"[a-z]{20}", "aaaaaaaaaaaaaaaaaaaa">>();

assertMatch<Match<"abc|(bb(c|d))", "abc">>();
assertMatch<Match<"abc|(bb(c|d))", "bbc">>();
assertMatch<Match<"abc|(bb(c|d))", "bbd">>();
assertNoMatch<Match<"abc|(bb(c|d))", "bba">>();

assertMatch<Match<"[abc][def]", "bd">>();
assertNoMatch<Match<"[abc][def]", "ag">>();

assertMatch<Match<"ab(d|e)(g|(e(f|h)z))x", "abegx">>();
assertMatch<Match<"abe(g|(e(f|h)z))x", "abegx">>();
assertMatch<Match<"e(g|(ez))x", "egx">>();
assertMatch<Match<"a(b)c", "abc">>();

assertMatch<Match<"(ab){2}", "abab">>();