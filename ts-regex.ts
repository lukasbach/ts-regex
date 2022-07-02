                                                                      //////////////////////////////////////////////////
                                                                          ////////////////////////////////////// DEMO //
                                                                              //////////////////////////////////////////
// --- Positive examples; These all evaluate to true
type DemoPositive01 = Match<"[a-zA-Z]{5}", "Regex">;
type DemoPositive02 = Match<"((hello)|(goodbye)) world [0-9]+", "hello world 123">;
type DemoPositive03 = Match<"([a-z]{5}123)|\\d", "hello123">;

// --- Negative examples; These all evaluate to false
type DemoNegative01 = Match<"[a-zA-Z]{5}", "too long">;
type DemoNegative02 = Match<"\\w\\d\\d", "123">;
type DemoNegative03 = Match<"(\\w{5}123)|\\d", "xxx">;

// For more examples, scroll to the bottom where more unit tests verify more functionality.



                                                                      //////////////////////////////////////////////////
                                                                          ///////////////////////////////// GUIDELINE //
                                                                              //////////////////////////////////////////
/**
 * # Recurring semantics
 * - Component: regex syntax component, such as groups, bracket expressions, ...
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
type IfElse<cond extends boolean, then extends any, otherwise extends any> = cond extends true ? then : otherwise;

type Contains<str extends string, substring extends string> = Extends<str, `${string}${substring}${string}`>;

type FirstChar<str extends string> = str extends `${infer first}${infer rest}` ? first : never;
type WithoutFirstChar<str extends string> = str extends `${infer first}${infer rest}` ? rest : never;
type StartsWith<str extends string, prefix extends string> = Extends<str, `${prefix}${string}`>;
type EndsWith<str extends string, suffix extends string> = Extends<str, `${string}${suffix}`>;
//    IsEmpty<str> extends true
//      ? false
//      : IsEqual<str, suffix> extends true
//      ? true
//      : EndsWith<WithoutFirstChar<str>, suffix>;

type IsSingleChar<str extends string> = str extends `${infer a}${infer b}` ? IsEmpty<b> : false;

// Nullish Coalescing
type NuCo<Value, Otherwise> =
  IfElse<Or<Extends<Value, "">, Extends<Value, false>, Extends<Value, never>>, Otherwise, Value>;


type NotNullish<value> = value extends null | false ? never : value;

type CountCharOccurances<str extends string, char extends string, count extends string = "0"> =
  IsEmpty<str> extends true
    ? count
    : CountCharOccurances<
      WithoutFirstChar<str>,
      char,
      FirstChar<str> extends char ? Increase<count> : count
      >;
type EqualCharOccurances<str extends string, charA extends string, charB extends string> =
  IsEqual<CountCharOccurances<str, charA>, CountCharOccurances<str, charB>>;



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
// -------------------------------------------------------------------------------------------- Component testers --- //
interface ComponentTests {
    or: `${string}|${string}`;
    group: `(${"?:" | ""}${string})`;
    wordClass: `\w`;
    nonWordClass: `\W`;
    digitClass: `\d`;
    nonDigitClass: `\D`;
    whitespaceClass: `\s`;
    nonWhitespaceClass: `\S`;
    anyCharClass: `.`;
    bracketExpr: `[${string}]`;
}


// ----------------------------------------------------------------------------------------------------------- Or --- //
type MatchOr<regex extends string, test extends someTest> =
  regex extends `${infer a}|${infer b}`
    ? NotNullish<
      NuCo<
        Or<MatchInternal<a, test>, MatchInternal<b, test>>,
        MatchInternal<a, test> | MatchInternal<b, test>
        >
      >
    : false;
type IsOr<regex extends string> =
  regex extends `${infer start}|${string}`
    ? Or<IsToken<start>, Not<Contains<start, "(" | ")">>>
    : false;


// ------------------------------------------------------------------------------------------------------- Groups --- //
type MatchGroup<regex extends string, test extends someTest> =
  regex extends `(${infer regexGroup})${infer regexRest}`
      ? MatchRemainingGroup<regexGroup, regexRest, test>
    : false;
type MatchRemainingGroup<regexGroup extends string, regexRest extends string, test extends someTest> =
  regexRest extends `${infer regexRest1})${infer regexRest2}`
    ? MatchRemainingGroup<`${regexGroup})${regexRest1}`, regexRest2, test>
    : MatchInternal<
      regexRest,
      IfElse<StartsWith<regexRest, "|">, NuCo<MatchInternal<regexGroup, test>, test>, MatchInternal<regexGroup, test>>
      >;

// MatchRemainingGroup accounts for the case of nested groups, since ProduceGroup
// stops at the first occurance of a closing bracket.

type debug4 = MatchGroup<'(hello)|(goodbye)', "hello">;
type debug5 = "(hello)|(goodbye)" extends `(${infer regexGroup})${infer regexRest}` ? [regexGroup, regexRest] : never;
type debug6 = "|(goodbye)" extends `${infer regexRest1})${infer regexRest2}` ? [regexRest1, regexRest2] : never;


// -------------------------------------------------------------------------------------------- Character classes --- //
type IsToken<regex extends string> =
  And<
    Or<
      Or<
        Extends<regex, ComponentTests["group"]>,
        Extends<regex, ComponentTests["bracketExpr"]>,
        Extends<regex, ComponentTests["wordClass"]>,
        Extends<regex, ComponentTests["nonWordClass"]>,
        Extends<regex, ComponentTests["digitClass"]>
        >,
      Or<
        Extends<regex, ComponentTests["nonDigitClass"]>,
        Extends<regex, ComponentTests["whitespaceClass"]>,
        Extends<regex, ComponentTests["nonWhitespaceClass"]>,
        Extends<regex, ComponentTests["anyCharClass"]>,
        IsAnyChar<regex>
        >
      >,
    And<
      EqualCharOccurances<regex, "(", ")">,
      EqualCharOccurances<regex, "[", "]">,
      EqualCharOccurances<regex, "{", "}">
      >
  >;


// -------------------------------------------------------------------------------------------------- Quantifiers --- //
type QUANTITIY_MAX = "100";

type IsQuantifier<regex extends string> =
    Or<
      IsLimitsQuantifier<regex>,
      IsQmarkQuantifier<regex>,
      IsPlusQuantifier<regex>,
      IsAsterixQuantifier<regex>
      >;

type IsLimitsQuantifier<regex extends string> =
  regex extends `${infer token}{${infer quantity}}${string}`
    ? And<IsNumberString<quantity>, IsToken<token>>
    : false;
type IsQmarkQuantifier<regex extends string> =
  regex extends `${infer token}?${string}`
    ? IsToken<token>
    : false;
type IsPlusQuantifier<regex extends string> =
  regex extends `${infer token}+${string}`
    ? IsToken<token>
    : false;
type IsAsterixQuantifier<regex extends string> =
  regex extends `${infer token}*${string}`
    ? IsToken<token>
    : false;

type MatchQuantifier<regex extends string, test extends someTest> =
  IsLimitsQuantifier<regex> extends true ? MatchLimitsQuantifier<regex, test>
  : IsQmarkQuantifier<regex> extends true ? MatchQmarkQuantifier<regex, test>
  : IsPlusQuantifier<regex> extends true ? MatchPlusQuantifier<regex, test>
  : IsAsterixQuantifier<regex> extends true ? MatchAsterixQuantifier<regex, test>
  : never;

type MatchLimitsQuantifier<regex extends string, test extends someTest> =
  regex extends `${infer token}{${infer quantity}}${infer regexRest}`
    ? ProcessQuantifier<token, test, ParseQuantityMin<quantity>, ParseQuantityMax<quantity>> extends false
      ? false
      : MatchInternal<regexRest, ProcessQuantifier<token, test, ParseQuantityMin<quantity>, ParseQuantityMax<quantity>>>
    : false;
type MatchQmarkQuantifier<regex extends string, test extends someTest> =
  regex extends `${infer token}?${infer regexRest}`
    ? ProcessQuantifier<token, test, "0", "1"> extends false
      ? false
      : MatchInternal<regexRest, ProcessQuantifier<token, test, "0", "1">>
    : false;
type MatchPlusQuantifier<regex extends string, test extends someTest> =
  regex extends `${infer token}+${infer regexRest}`
    ? ProcessQuantifier<token, test, "1", QUANTITIY_MAX> extends false
      ? false
      : MatchInternal<regexRest, ProcessQuantifier<token, test, "1", QUANTITIY_MAX>>
    : false;
type MatchAsterixQuantifier<regex extends string, test extends someTest> =
  regex extends `${infer token}*${infer regexRest}`
    ? ProcessQuantifier<token, test, "0", QUANTITIY_MAX> extends false
      ? false
      : MatchInternal<regexRest, ProcessQuantifier<token, test, "0", QUANTITIY_MAX>>
    : false;


// returns remaining string or false if not matched
type ProcessQuantifier<
  regexPart extends string, test extends someTest, min extends string, max extends string, count extends string = "0"> =
  IsLargerEquals<count, max> extends true
    ? test
    : MatchInternal<regexPart, test> extends string
      ? ProcessQuantifier<regexPart, MatchInternal<regexPart, test>, min, max, Increase<count>>
      : (
        IsSmaller<
          IfElse<
            Extends<MatchInternal<regexPart, test>, true>,
            Increase<count>,
            count
            >,
          min
          > extends true
          ? false
          : (MatchInternal<regexPart, test> extends true ? "" : test))
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
      : QUANTITIY_MAX;



// ------------------------------------------------------------------------------------------------- Range groups --- //
type MatchBracketExpr<regex extends string, test extends someTest> =
  regex extends `[${infer range}]${infer regexRest}`
    ? range extends `^${infer actualRange}`
      ? test extends `${infer firstChar}${infer testRest}`
        ? IfElse<Not<Extends<firstChar, CharRange<actualRange>>>, MatchInternal<regexRest, testRest>, false>
        : false
      : test extends `${CharRange<range>}${infer testRest}`
        ? MatchInternal<regexRest, testRest>
        : false
    : false;



// ------------------------------------------------------------------------------------------- Character matchers --- //

type MatchCharacterClass<
  regex extends string, test extends someTest, classSymbol extends string, group extends string, prefix extends string = "\\"> =
    regex extends `${prefix}${infer actualSymbol}${infer regexRest}`
      ? test extends `${MatchBracketExpr<group, test>}${infer testRest}`
        ? IfElse<Extends<actualSymbol, classSymbol>, MatchInternal<regexRest, testRest>, false>
        : false
      : false;

type MatchRegularChar<regex extends string, test extends someTest> =
  regex extends `${infer regexChar}${infer regexRest}`
    ? test extends `${infer testChar}${infer testRest}`
      ? IfElse<Extends<regexChar, testChar>, MatchInternal<regexRest, testRest>, false>
      : false
    : false;

                                                                      //////////////////////////////////////////////////
                                                                          ////////////////////////////// MATCHER TYPE //
                                                                              //////////////////////////////////////////
type MatchInternal<regex extends string, test extends string | boolean> =
  test extends boolean ? IfElse<Extends<regex, ''>, test, false>
  : And<regex extends "" ? true : false, test extends "" ? true : false> extends true ? true
  : regex extends "" ? test
  : test extends never ? false
  : IsOr<regex> extends true ? MatchOr<regex, test> // TODO order?
  : IsQuantifier<regex> extends true ? MatchQuantifier<regex, test>
  : StartsWith<regex, ComponentTests["group"]> extends true ? MatchGroup<regex, test>
  : StartsWith<regex, ComponentTests["bracketExpr"]> extends true ? MatchBracketExpr<regex, test>
  // TODO: The character classes work in theory, but break the compilation due to callstack size being exceeded
  // : StartsWith<regex, "\\d"> extends true ? MatchCharacterClass<regex, test, "d", "[0-9]">
  // : StartsWith<regex, "\\D"> extends true ? MatchCharacterClass<regex, test, "D", "[^0-9]">
  // : StartsWith<regex, "\\w"> extends true ? MatchCharacterClass<regex, test, "w", "[a-zA-Z]">
  // : StartsWith<regex, "\\W"> extends true ? MatchCharacterClass<regex, test, "W", "[^a-zA-Z]">
  // : StartsWith<regex, "\\s"> extends true ? MatchCharacterClass<regex, test, "s", "[ ]">
  // : StartsWith<regex, "\\S"> extends true ? MatchCharacterClass<regex, test, "S", "[^ ]">
  // : StartsWith<regex, "."> extends true ? MatchCharacterClass<regex, test, ".", "[0-9a-zA-Z ]", "">
  : StartsWith<regex, anyChar> extends true ? MatchRegularChar<regex, test>
  : false;

type Match<regex extends string, test extends string> = Extends<MatchInternal<regex, test>, true>;

type debug1 = MatchInternal<"((hello)|(goodbye)) world [0-9]+", "hello world 123">;
type debug2 = MatchInternal<"(hello)|(goodbye)", "hello">;
type debug24 = MatchInternal<"hello|goodbye", "hello">;
type debug3 = StartsWith<"(hello)|(goodbye)", ComponentTests["group"]>;
type debug311 = MatchInternal<"((hello)|(goodbye)) world [0-9]+", "hello world 123">;


type debug10 = MatchInternal<"(a)|(b)", "ab">;
type debug11 = StartsWith<"(a)|(b)", ComponentTests["group"]>;



                                                                      //////////////////////////////////////////////////
                                                                          ///////////////////// ASCII Character Table //
                                                                              //////////////////////////////////////////
interface CharTable {
    "0": never,       "32": " ",      "64": "@",      "97": "a",
    "1": never,       "33": "!",      "65": "A",      "98": "b",
    "2": never,       "34": "\"",     "66": "B",      "99": "c",
    "3": never,       "35": "#",      "67": "C",      "100": "d",
    "4": never,       "36": "$",      "68": "D",      "101": "e",
    "5": never,       "37": "%",      "69": "E",      "102": "f",
    "6": never,       "38": "&",      "70": "F",      "103": "g",
    "7": never,       "39": "'",      "71": "G",      "104": "h",
    "8": never,       "40": "(",      "72": "H",      "105": "i",
    "9": never,       "41": ")",      "73": "I",      "106": "j",
    "10": never,      "42": "*",      "74": "J",      "107": "k",
    "11": never,      "43": "+",      "75": "K",      "108": "l",
    "12": never,      "44": ",",      "76": "L",      "109": "m",
    "13": never,      "45": "-",      "77": "M",      "110": "n",
    "14": never,      "46": ".",      "78": "N",      "111": "o",
    "15": never,      "47": "/",      "79": "O",      "112": "p",
    "16": never,      "48": "0",      "80": "P",      "113": "q",
    "17": never,      "49": "1",      "81": "Q",      "114": "r",
    "18": never,      "50": "2",      "82": "R",      "115": "s",
    "19": never,      "51": "3",      "83": "S",      "116": "t",
    "20": never,      "52": "4",      "84": "T",      "117": "u",
    "21": never,      "53": "5",      "85": "U",      "118": "v",
    "22": never,      "54": "6",      "86": "V",      "119": "w",
    "23": never,      "55": "7",      "87": "W",      "120": "x",
    "24": never,      "56": "8",      "88": "X",      "121": "y",
    "25": never,      "57": "9",      "89": "Y",      "122": "z",
    "26": never,      "58": ":",      "90": "Z",      "123": "{",
    "27": never,      "59": ";",      "91": "[",      "124": "|",
    "28": never,      "60": "<",      "92": "\\",     "125": "}",
    "29": never,      "61": "=",      "93": "]",      "126": "~",
    "30": never,      "62": ">",      "94": "^",      "127": never,
    "31": never,      "63": "?",      "95": "_",
}

type InvertedCharTable = {
    [P in keyof CharTable as CharTable[P]]: P
}




                                                                      //////////////////////////////////////////////////
                                                                          //////////////////// INSIGHTFUL EXPERIMENTS //
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
function assert<T extends true>() {}
function assertNot<T extends false | string>() {}


                                                                      //////////////////////////////////////////////////
                                                                          //////////////////////////////// UNIT TESTS //
                                                                              //////////////////////////////////////////
typeAssert<Test<ParseQuantityMin<"3,4">, "3">>();
typeAssert<Test<ParseQuantityMax<"3,4">, "4">>();
typeAssert<Test<ProcessQuantifier<"a", "aaaaaaax", "1", "6">, "ax">>();
typeAssert<Test<ProcessQuantifier<"a", "aaxxxxxx", "1", "6">, "xxxxxx">>();
typeAssert<Test<ProcessQuantifier<"a", "axxxxxxx", "2", "6">, never>>();
assert<MatchQuantifier<"[a-zA-Z]{4,5}", "aaDSa">>();
assertNot<MatchQuantifier<"[a-zA-Z]{4,5}", "aaDSDa">>();


assert<Match<"", "">>();
assert<Match<"abc", "abc">>();
assert<Match<"[abc]", "a">>();
assert<Match<"[abc][def]", "ad">>();
assert<Match<"[a-z][A-Z0-9][0-9]", "aD4">>();
assert<Match<"[a-z]{20}", "aaaaaaaaaaaaaaaaaaaa">>();
assert<Match<"[^abc]", "0">>();
assertNot<Match<"[^abc]", "a">>();

assert<Match<"abc|(bb(c|d))", "abc">>();
assert<Match<"abc|(bb(c|d))", "bbc">>();
assert<Match<"abc|(bb(c|d))", "bbd">>();
assertNot<Match<"abc|(bb(c|d))", "bba">>();

assert<Match<"[abc][def]", "bd">>();
assertNot<Match<"[abc][def]", "ag">>();

assert<Match<"ab(d|e)(g|(e(f|h)z))x", "abegx">>();
assert<Match<"abe(g|(e(f|h)z))x", "abegx">>();
assert<Match<"e(g|(ez))x", "egx">>();
assert<Match<"a(b)c", "abc">>();

assert<Match<"ab{2}", "abb">>();

assert<Match<"(ab){2}", "abab">>();

assert<Match<"(a|b){2}", "aa">>();
assert<Match<"(a|b){2}", "bb">>();
assertNot<Match<"(a|b){2}", "a">>();
assertNot<Match<"(a|b){2}", "b">>();

assert<Match<"a|b{2}", "a">>();
assert<Match<"a|b{2}", "bb">>();
assertNot<Match<"a|b{2}", "aa">>();

assert<Match<"(a|b){2}", "aa">>();
assert<Match<"(a|b){2}", "bb">>();
assertNot<Match<"(a|b){2}", "a">>();
assertNot<Match<"(a|b){2}", "b">>();


assert<Match<"a(b)", "ab">>();
assertNot<Match<"a(b)", "a">>();
assertNot<Match<"a(b)", "b">>();

assert<Match<"e(g|(ez))x", "egx">>();
assert<Match<"e(g|(ez))x", "eezx">>();
assertNot<Match<"e(g|(ez))x", "egezx">>();


assert<Match<"(a|(b|c)|d)z", "az">>();
assert<Match<"(a|(b|c)|d)z", "bz">>();
assert<Match<"(a|(b|c)|d)z", "cz">>();
assert<Match<"(a|(b|c)|d)z", "dz">>();
assertNot<Match<"(a|(b|c)|d)z", "a">>();
assertNot<Match<"(a|(b|c)|d)z", "z">>();


assert<Match<"ab?", "a">>();
assert<Match<"ab?", "ab">>();
assertNot<Match<"ab?", "ab?">>();
assert<Match<"x(abc)?", "xabc">>();
assert<Match<"x(abc)?", "x">>();
assert<Match<"x[abc]?", "xa">>();
assert<Match<"x[abc]?", "x">>();

assert<Match<"ab+", "ab">>();
assert<Match<"ab+", "abb">>();
assert<Match<"ab+", "abbb">>();
assertNot<Match<"ab+", "a">>();

assert<Match<"ab*", "a">>();
assert<Match<"ab*", "ab">>();
assert<Match<"ab*", "abb">>();
assert<Match<"ab*", "abbb">>();

assert<Match<"(a)|(b)", "a">>();
assert<Match<"(a)|(b)", "b">>();
assertNot<Match<"(a)|(b)", "ab">>();

// assert<Match<"\w\w\w-\d\d\d-\s-\W\W\W-\D\D\D-\S\S\S-...", "aGz-159- - 13-a5z-1g ">>();
// assertNot<Match<"\w\w\w-\d\d\d-\s-\W\W\W-\D\D\D-\S\S\S-...", "aGz-159- - 13-a5z-1g ">>();
// type test = Match<"\w\w\w-\d\d\d-\s-\W\W\W-\D\D\D-\S\S\S-...", "aGz-159- - 13-a5z-1g ">;

assert<Match<"((hello)|(goodbye)) world [0-9]+", "hello world 123">>();
assert<Match<"((hello)|(goodbye)) world [0-9]+", "goodbye world 1">>();
assertNot<Match<"((hello)|(goodbye)) world [0-9]+", "hello world ">>();
assertNot<Match<"((hello)|(goodbye)) world [0-9]+", "hello world">>();
assertNot<Match<"((hello)|(goodbye)) world [0-9]+", "hello ">>();
type testasd = Match<"((hello)|(goodbye)) world [0-9]+", "hello world 3">;

