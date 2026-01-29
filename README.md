# parser-combinators2
PoC parser combinator library with C++26 reflection.

This is a lightweight, header-only parser combinator library designed for C++26. It leverages the P2996 reflection proposal to provide a declarative, DSL-like syntax for building efficient parsers at compile-time. You can define complex grammars with minimal boilerplate while maintaining a relatively high performance and type safety.

For debugging, the library includes a non-intrusive hook system. By performing apply_hook on a parser, you can automatically generate execution traces to monitor how the engine traverses your grammar.

__Do not use this for production purposes.__

The parser generator itself utilizes `std::tuple` `std::variant` etc, which have extremely poor performance. Perhaps with reflection I can generate simple tuple/variant structs at compile time, thus preventing the standard library shit. And the Y-combinator seems to have poor performance as well, which is quite absurd.

Benchmark result with an 70MB JSON file:

```
Run on (8 X 1996 MHz CPU s)
CPU Caches:
  L1 Data 32 KiB (x8)
  L1 Instruction 32 KiB (x8)
  L2 Unified 512 KiB (x8)
  L3 Unified 4096 KiB (x2)
--------------------------------------------------------------------------------------------------------------------
Benchmark                                                          Time             CPU   Iterations UserCounters...
--------------------------------------------------------------------------------------------------------------------
BM_JsonParse_Win/iterations:30/repeats:5/threads:1_mean         1581 ms         1570 ms            5 bytes_per_second=49.8319Mi/s
BM_JsonParse_Win/iterations:30/repeats:5/threads:1_median       1584 ms         1573 ms            5 bytes_per_second=49.7516Mi/s
BM_JsonParse_Win/iterations:30/repeats:5/threads:1_stddev       7.82 ms         7.96 ms            5 bytes_per_second=258.784Ki/s
BM_JsonParse_Win/iterations:30/repeats:5/threads:1_cv           0.49 %          0.51 %             5 bytes_per_second=0.51%
```

This benchmark is performed via a modified version which uses:

- a lightweight variant container implemented by me
- mimalloc as the global default allocator
- some zero-copy strategies

I must say, that's pretty surprising... As all there are almost no hand-written boilerplates, just auto generated code via reflection.

Here are some examples. 

A simple parser which parses a sequence like `hellohello12345worldabc`:

```cpp
// the structure of the output sequence
struct ParserOut {
    int value1; // the digit 12345
    std::string_view value2; // 'world'
    std::variant<std::string_view, std::string_view> choice_value; // 'xyz' or 'abc'
    // other values are discarded
};

// parses a digit
constexpr auto digit = 
    pc::dsl::numeric() // 0~9
    .many().collect()  // make it a string sequence
    | pc::dsl::guard<pc::preds::not_empty> // fail on empty strings
    | pc::dsl::fmap<[](std::string_view s) { return std::stoi(std::string(s)); }>; // map into int. you can use any callable type here

// use combine to parse a sequence
constexpr auto parser = pc::dsl::combine<{
        pc::dsl::symbol<"hello">().many().discard(), // many 'hello's, we just discard them
        digit, // use a defined parser here
        pc::dsl::symbol<"world">().expect<"There should be a 'world' here">(), // use expect to provide a verbose error message
        pc::dsl::choice({ // one of 'xyz' and 'abc'
            pc::dsl::symbol<"xyz">(),
            pc::dsl::symbol<"abc">(),
        }),
    }>() | pc::dsl::fmap<pc::dsl::fmap_struct<ParserOut>>; // this will fill in the values in the sequence to the struct respectively
```

Json parser. Full code in `json_parser_example.cpp`.

```cpp
using pc = parser_combinators;

// the main JSON parser
// as JSON is recursive, we use a Y-combinator via dsl::lazy
// and the recursive self (an detail::parser_wrapper) is passed as an argument to the lambda
constexpr auto json_parser = pc::dsl::lazy<[](auto self) {
    // parse JSON arrays
    auto array = pc::dsl::combine({
        lbracket.discard(),
        sepby<comma>(self).optional(), // use optional to allow empty arrays
        rbracket.discard(),
    }) | pc::dsl::fmap<[](std::optional<json_array> opt) -> json_value {
        return opt.value_or(json_array{});
    }>;

    // string and numeric literals
    auto string_lit = pc::dsl::combine({
        pc::dsl::symbol<"\"">().discard(),
        pc::dsl::alphanumeric().many().collect(),
        pc::dsl::symbol<"\"">().discard(),
    }) | pc::dsl::fmap<[](std::string_view s) -> json_value {
        return std::string(s);
    }>;

    auto numeric_lit = pc::dsl::combine({
        pc::dsl::numeric().many().collect(),
        pc::dsl::combine({
            pc::dsl::symbol<".">().discard(), // decimal point
            pc::dsl::numeric().many().collect(),
        }).optional(), // optional fractional part
    }).collect().guard<pc::preds::not_empty>() | pc::dsl::fmap<[](std::string_view s) -> json_value {
        // the collect() will just ignore all underlying structures and produce a single string_view
        // and then we use guard to ensure it's not empty, or it will break std::stod
        // finally, we convert to double
        return std::stod(std::string(s));
    }>;

    auto kwpair = pc::dsl::combine({
        string_lit,
        colon.commit().discard(), // commit to avoid backtracking after colon
        self,
    }) | pc::dsl::fmap<[](auto&& key_val, auto&& value) {
        return std::make_pair(std::get<std::string>(key_val.data), value);
    }>;

    auto object = pc::dsl::combine({
        lbrace.discard(),
        sepby<comma>(kwpair).optional(),
        rbrace.discard(),
    }) | pc::dsl::fmap<[](std::optional<std::vector<std::pair<std::string, json_value>>> opt) -> json_value {
        json_object obj;
        if (opt) {
            for (auto& p : *opt) obj.emplace(std::move(p));
        }
        return obj;
    }>;

    // finally, we use choice to combine all possible JSON value types
    return pc::dsl::choice({
        object,
        array,
        string_lit,
        numeric_lit,
        pc::dsl::symbol<"true">()  | pc::dsl::fmap<[](...) -> json_value { return true; }>,
        pc::dsl::symbol<"false">() | pc::dsl::fmap<[](...) -> json_value { return false; }>,
        pc::dsl::symbol<"null">()  | pc::dsl::fmap<[](...) -> json_value { return nullptr; }>,
    }) | pc::dsl::fmap<[](auto&& v) -> json_value {
        // choice() produces a variant (though all the inner types are json_value already)
        // we need to flatten it into json_value
        return std::visit([](auto&& inner_val) -> json_value {
            return std::forward<decltype(inner_val)>(inner_val);
        }, std::forward<decltype(v)>(v));
    }>;
}>.fix<json_value>();

int main() {
    // set up parse context with hook
    hook_impl hook;
    auto parse_context = pc::context();
    parse_context.set_hook(&hook);

    // apply hook to json_parser recursively
    // however due to the Y-combinator, it will only apply to the outermost level
    // to apply to all levels, it's best you define the parsers without Y-combinator first,
    // apply_hook to that, then use dsl::lazy to wrap it into a Y-combinator
    // but for demonstration purposes, this is sufficient
    constexpr auto trace = pc::apply_hook<json_parser>();
    auto str = R"({"key1": "value1", "key2": [true, false, null], "key3": {"nestedKey": 123.4, "anotherKey": [114, 514]}})";
    auto result = pc::parser<trace>::parse(str, parse_context);

    // check result
    if (!result.success) {
        // if failed, print error message and position
        // note that the internal position is a pointer into the original input string
        // us diff to get the offset
        std::println("Parse failed: {}, pos={}", parse_context.error.message, parse_context.error.diff(str));
        return 1;
    }
    std::println("Parse succeeded");

    // print the parsed JSON value
    walk_json(result.parsed);
    return 0;
}

```
