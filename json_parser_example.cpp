// Example JSON parser using parser-combinators2

#include "parser_combinators2.hpp"

#include <map>
#include <print>

namespace pc = parser_combinators;

// parser hook implementation for tracing
struct hook_impl : pc::details::parser_hook {
    int indent_level = 0;
    void on_enter(std::string_view name, std::string_view input) override {
        std::println("{:>{}}--> Entering parser '{}' with input '{}'", "", indent_level * 2, name, input);
        ++indent_level;
    }

    void on_exit(std::string_view name, std::string_view input) override {
        --indent_level;
        std::println("{:>{}}<-- Exiting parser '{}' with remaining input '{}'", "", indent_level * 2, name, input);
    }

    void on_failure(std::string_view name, std::string_view remaining) override {
        std::println("{:>{}}!!  Parser '{}' failed at remaining input '{}'", "", indent_level * 2, name, remaining);
    }
};

// example JSON value representation
struct json_value;
using json_object = std::map<std::string, json_value>;
using json_array = std::vector<json_value>;

struct json_value {
    std::variant<
        std::nullptr_t, 
        bool, 
        double, 
        std::string, 
        json_array, 
        json_object
    > data;

    template<typename T>
    requires (!std::is_same_v<std::remove_cvref_t<T>, json_value>)
    json_value(T&& v) : data(std::forward<T>(v)) {}

    json_value(const json_value&) = default;
    json_value(json_value&&) = default;
    json_value& operator=(const json_value&) = default;
    json_value& operator=(json_value&&) = default;

    json_value() : data(nullptr) {}
};

void walk_json(const json_value& val, int indent = 0) {
    std::string indent_str(indent * 2, ' ');
    std::visit([&](auto&& inner_val) {
        using T = std::decay_t<decltype(inner_val)>;
        if constexpr (std::is_same_v<T, std::nullptr_t>) {
            std::println("{}null", indent_str);
        } else if constexpr (std::is_same_v<T, bool>) {
            std::println("{}{}", indent_str, inner_val ? "true" : "false");
        } else if constexpr (std::is_same_v<T, double>) {
            std::println("{}{}", indent_str, inner_val);
        } else if constexpr (std::is_same_v<T, std::string>) {
            std::println("{}\"{}\"", indent_str, inner_val);
        } else if constexpr (std::is_same_v<T, json_array>) {
            if (inner_val.empty()) {
                std::println("{}[]", indent_str);
                return;
            }
            std::println("{}[", indent_str);
            for (const auto& item : inner_val) {
                walk_json(item, indent + 1);
            }
            std::println("{}]", indent_str);
        } else if constexpr (std::is_same_v<T, json_object>) {
            std::println("{}{{", indent_str);
            for (const auto& [key, value] : inner_val) {
                std::println("{}  \"{}\":", indent_str, key);
                walk_json(value, indent + 2);
            }
            std::println("{}}}", indent_str);
        }
    }, val.data);
}


// example JSON parser

// lexeme parser that skips surrounding whitespace
// P is a detail::parser_wrapper
template <auto P>
constexpr auto lexeme = 
    pc::dsl::combine<{
        pc::dsl::whitespace().many().discard(), // use many to skip zero or more whitespace
        P,                                      // use the parser P
        pc::dsl::whitespace().many().discard(), // use discard to drop the result (don't map into result tuple/struct)
    }>();

// symbol parsers for JSON syntax
constexpr auto lparen = lexeme<pc::dsl::symbol<"(">()>;
constexpr auto rparen = lexeme<pc::dsl::symbol<")">()>;
constexpr auto lbracket = lexeme<pc::dsl::symbol<"[">()>;
constexpr auto rbracket = lexeme<pc::dsl::symbol<"]">()>;
constexpr auto lbrace = lexeme<pc::dsl::symbol<"{">()>;
constexpr auto rbrace = lexeme<pc::dsl::symbol<"}">()>;
constexpr auto colon = lexeme<pc::dsl::symbol<":">()>;
constexpr auto comma = lexeme<pc::dsl::symbol<",">()>;

// helper parser to parse sequences separated by a specific separator
template <auto Sep>
consteval auto sepby(auto P) { 
    return pc::dsl::combine({ // combine first element and rest
        P,                    // first element
        (pc::dsl::combine({   // combine separator and element
            Sep.discard(),    // separator
            P,                // element
        }).many()),           // takes zero or more
    }) | pc::dsl::fmap<[](auto&& first, auto&& rest) {
        // we use fmap to transform the result into a vector
        // note that the type of rest is std::vector<std::tuple<element_type>>
        // this is because each element is wrapped in a tuple due to combine,
        // and the Sep is discarded, so we just get<0> to extract the element
        // if you want to map the result tuple to a specific type, use fmap_struct
        using T = std::decay_t<decltype(first)>;
        std::vector<T> result;
        result.push_back(first);
        for (auto&& val : rest) {
            result.push_back(std::get<0>(val));
        }
        return result;
    }>;
};

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
