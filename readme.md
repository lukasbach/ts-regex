# ts-regex

A regex implementation in Typescript. No not a typed JS library, a Regex matcher that is exclusively
written in the Typing System of TypeScript. Because.. Science, I guess.

```typescript
// --- Positive examples; These all evaluate to true
type DemoPositive01 = Match<"[a-zA-Z]{5}", "Regex">;
type DemoPositive02 = Match<"((hello)|(goodbye)) world [0-9]+", "hello world 123">;
type DemoPositive03 = Match<"([a-z]{5}123)|\\d", "hello123">;

// --- Negative examples; These all evaluate to false
type DemoNegative01 = Match<"[a-zA-Z]{5}", "too long">;
type DemoNegative02 = Match<"[a-z]{3}", "123">;
type DemoNegative03 = Match<"([a-z]{5}123)|[0-9]", "xxx">;
```

See [./ts-regex.ts](./ts-regex.ts) for how it is done.