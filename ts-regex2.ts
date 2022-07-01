import { CharTable, InvertedCharTable } from './char-table';


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
type FirstChar<str extends string> = str extends `${infer char}${string}` ? char : never;
type WithoutFirstChar<str extends string> = str extends `${string}${infer rest}` ? rest : never;

type IfElse<cond extends boolean, then extends any, otherwise extends any> = cond extends true ? then : otherwise;

type JoinList<T extends any[]> = T extends [infer initial, ...(infer rest)]
  ? initial extends string ? `${initial}${JoinList<rest>}` : "" : "";
type FirstListItem<T extends any[]> = T extends [infer initial, ...any] ? initial : never;
type WithoutFirstListItem<T> = T extends [any, ...(infer rest)] ? rest : never;

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
///////////////////////////////////// LEXER //
//////////////////////////////////////////
type Token<name extends string = string, value extends string = string> = {
  type: "token";
  name: name;
  value: value;
};
type IsToken<name extends string, token extends any> =
  token extends Token<any, any> ? Extends<token["name"], token> : false;


type Lexer<pattern extends string> =
  IsEmpty<pattern> extends true ? [Token<"NONE", "">]
    : StartsWith<pattern, "("> extends true ? [Token<"LEFT_PAREN", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
      : StartsWith<pattern, ")"> extends true ? [Token<"RIGHT_PAREN", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
        : StartsWith<pattern, "*"> extends true ? [Token<"STAR", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
          : StartsWith<pattern, "+"> extends true ? [Token<"PLUS", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
            : StartsWith<pattern, "?"> extends true ? [Token<"QMARK", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
              : StartsWith<pattern, "|"> extends true ? [Token<"ALT", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
                : StartsWith<pattern, "["> extends true ? [Token<"LEFT_BRACKET", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
                  : StartsWith<pattern, "]"> extends true ? [Token<"RIGHT_BRACKET", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
                    : StartsWith<pattern, "-"> extends true ? [Token<"MINUS", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>]
                      : [Token<"CHAR", FirstChar<pattern>>, ...Lexer<WithoutFirstChar<pattern>>];





//////////////////////////////////////////////////
//////////////////////////////////// PARSER //
//////////////////////////////////////////

type ParseError = []; // "PARSE_ERROR";
type MatchLookahead<lexer extends Token[], tokenName extends string> =
  Extends<lexer[0]["name"], tokenName>;
type Consume<tokenName extends string, lexer extends Token[]> =
  IfElse<
    Extends<lexer[0]["name"], tokenName>,
    WithoutFirstListItem<lexer>,
    ParseError
    >;

type Parser<lexer extends Token<any, any>[]> = ParseExpression<lexer, []>;

type ParseExpression<lexer extends any[], tree extends any[]> =
  IfElse<
    MatchLookahead<lexer, "ALT">,
    [Alteration<[...tree, ParseTerm<lexer, tree>], ParseExpression<Consume<'ALT', lexer>, []>>],
    // Consume<ParserState<LexerOf<state>, [
    //   Alteration<ParsedOf<state>, ParseTerm<state>["lookahead"]["name"]>
    // ]>, "ALT">,
    ParseTerm<lexer, tree>
    >;


type ParseTerm<lexer extends any[], tree extends any[]> = ParseFactor<lexer, tree>; // TODO factor term

type ParseFactor<lexer extends any[], tree extends any[]> = ParsePrimary<lexer, tree>; // TODO
type ParsePrimary<lexer extends any[], tree extends any[]> =
  MatchLookahead<lexer, "LEFT_PAREN"> extends true ?
    Consume<
      "LEFT_PAREN",
      [...tree, ParseTerm<lexer, tree>]
      > :
    Consume<"CHAR", [...tree, lexer[0]]>;


type Group<children> = {
  type: "class-group";
  children: children;
};
type Alteration<left, right> = {
  type: "class-alteration";
  left: left;
  right: right;
};



type regexExample = "ab|(d|ef))|g|[a-zA-Z]*";



type lexed = Lexer<regexExample>;


type parsed = Parser<Lexer<regexExample>>;


type MockParse<lexer extends any[], tree extends any[]> = MockParseInner<lexer, tree>;
type MockParseInner<lexer extends any[], tree extends any[]> =
  Extends<lexer[0], "("> extends true ? [...tree, MockParse<WithoutFirstListItem<lexer>, []>]
  : Extends<lexer[0], ")"> extends true ? never
  : [...tree, lexer[0], ...MockParse<WithoutFirstListItem<lexer>, []>];
type x = MockParse<["a", "b", "(", "c", ")"], []>;


type MockParse2<lexer extends any[], tree extends any[]> = lexer extends [] ? [] : MockParseInner2<lexer, tree>;
type MockParseInner2<lexer extends any[], tree extends any[]> =
  Extends<lexer[0], "("> extends true ? [...tree, MockParse2<WithoutFirstListItem<lexer>, []>]
  : [...tree, lexer[0], ...MockParse2<WithoutFirstListItem<lexer>, []>];
type x2 = MockParse2<["a", "(", "c", ")"], []>;


type MockParse3<lexer extends any[], tree extends any[]> = lexer extends [] ? [] : MockParseInner3<lexer, tree>;
type MockParseInner3<lexer extends any[], tree extends any[]> =
  IfElse<Extends<lexer[0], "(">, [...tree, MockParse3<WithoutFirstListItem<lexer>, []>],[...tree, lexer[0], ...MockParse3<WithoutFirstListItem<lexer>, []>]>;
type x3 = MockParse3<["a", "(", "c", ")"], []>;