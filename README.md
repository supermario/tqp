
Proof-of-concept for whether Smartpoint TripQuote "Copy text to clipboard" output is parseable.

### Dependencies

Requires [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

```
stack build && stack exec tqp-exe > output.txt
```

Will parse `fixtures/sampleAllSegments.txt` and generate a raw printout of the Haskell objs.

### Next steps

A couple options:

- Use this binary directly to parse+output, will probably need to drop in JSON derivations
- Port to Elm, which has a very similar syntax and similar lexer/parser style libs, run it from the front-end
- Find some parser library in Ruby to do this and re-write it, using this as a helpful reference

See `src/[Flight|Car|Hotel].hs`.
