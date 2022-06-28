# ts-regex

A WIP regex implementation in Typescript. No not a typed JS library, a Regex matcher that is exclusively
written in the Typing System of TypeScript. Because.. Science, I guess.

```typescript
const valid: Regex<"[a-zA-Z]{2}\d"> = "aD4";
const invalid: Regex<"[a-zA-Z]{2}\d"> = "1D4"; // type error at compile time!
```

See [./ts-regex.ts](./ts-regex.ts) for how it is done.