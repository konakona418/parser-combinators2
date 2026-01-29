#include "parser_combinators2.hpp"

#include <mimalloc-new-delete.h>
#include <benchmark/benchmark.h>

#include <charconv>
#include <map>
#include <print>
#include <meta>
#include <chrono>
#include <ranges>

namespace pc = parser_combinators;

struct json_value;
using json_kv_pair = std::pair<std::string_view, json_value>;
using json_object  = std::vector<json_kv_pair>;
using json_array = std::vector<json_value>;

struct json_value {
    pc::details::static_variant<
        std::nullptr_t, 
        bool, 
        double, 
        std::string_view, 
        json_array, 
        json_object
    > data;

    template<typename T>
    requires (!std::is_same_v<std::remove_cvref_t<T>, json_value>)
    json_value(T&& v) : data(std::forward<T>(v)) {}

    json_value(const json_value&) = delete;
    json_value& operator=(const json_value&) = delete;
    json_value(json_value&& other) {
        data = std::move(other.data);
    }
    json_value& operator=(json_value&& other) {
        data = std::move(other.data);
        return *this;
    }

    json_value() : data(nullptr) {}
};

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
        result.push_back((std::move(first)));
        for (auto&& val : rest) {
            result.push_back(std::move(std::get<0>(val)));
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
    }) | pc::dsl::fmap<[](auto&& opt) -> json_value {
        if (opt) {
            return json_value{std::move(*opt)};
        } else {
            return json_value{json_array{}};
        }
    }>;

    // string and numeric literals
    auto string_lit = pc::dsl::combine({
        pc::dsl::symbol<"\"">().discard(),
        pc::dsl::alphanumeric().many().collect(),
        pc::dsl::symbol<"\"">().discard(),
    }) | pc::dsl::fmap<[](std::string_view s) -> json_value {
        return s;
    }>;

    auto numeric_lit = pc::dsl::combine({
        pc::dsl::symbol<"-">().optional().discard(), // optional minus sign
        pc::dsl::numeric().many().collect(),
        pc::dsl::combine({
            pc::dsl::symbol<".">().discard(), // decimal point
            pc::dsl::numeric().many().collect(),
        }).optional(), // optional fractional part
    }).collect().guard<pc::preds::not_empty>() | pc::dsl::fmap<[](std::string_view s) -> json_value {
        // the collect() will just ignore all underlying structures and produce a single string_view
        // and then we use guard to ensure it's not empty, or it will break std::stod
        // finally, we convert to double
        double val;
        std::from_chars(s.data(), s.data() + s.size(), val);
        return val;
    }>;

    auto kwpair = pc::dsl::combine({
        string_lit,
        colon.commit().discard(), // commit to avoid backtracking after colon
        self,
    }) | pc::dsl::fmap<[](auto&& key_val, auto&& value) {
        return std::make_pair(key_val.data.template get<std::string_view>(), std::move(value));
    }>;

    auto object = pc::dsl::combine({
        lbrace.discard(),
        sepby<comma>(kwpair).optional(),
        rbrace.discard(),
    }) | pc::dsl::fmap<[](auto&& opt) -> json_value {
        if (opt) {
            return json_object{std::move(*opt)};
        } else {
            return json_object{};
        }
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
        return v.visit([](auto&& inner_val) -> json_value {
            return (std::move(inner_val));
        });
    }>;
}>.fix<json_value>();

#include <fstream>
std::string read_file_to_string(const std::string& filepath) {
    std::ifstream file(filepath, std::ios::binary);
    if (!file) {
        return "";
    }
    std::string content;
    file.seekg(0, std::ios::end);
    content.resize(file.tellg());
    file.seekg(0, std::ios::beg);
    file.read(&content[0], content.size());
    return content;
}

extern "C" {
    using HANDLE = void*;
    using DWORD_PTR = unsigned __int64;
    using BOOL = int;

    __declspec(dllimport) HANDLE __stdcall GetCurrentThread();
    __declspec(dllimport) DWORD_PTR __stdcall SetThreadAffinityMask(HANDLE hThread, DWORD_PTR dwThreadAffinityMask);
    __declspec(dllimport) HANDLE __stdcall GetCurrentProcess();
    __declspec(dllimport) BOOL __stdcall SetPriorityClass(HANDLE hProcess, unsigned long dwPriorityClass);
}

constexpr unsigned long REALTIME_PRIORITY_CLASS = 0x00000100;
constexpr unsigned long HIGH_PRIORITY_CLASS     = 0x00000080;

void SetAffinity(const benchmark::State& state) {
    HANDLE thread = GetCurrentThread();
    SetThreadAffinityMask(thread, 1); // bind to CPU 0

    HANDLE process = GetCurrentProcess();
    SetPriorityClass(process, HIGH_PRIORITY_CLASS);
}

auto parse_context = pc::context();

static void BM_JsonParse_Win(benchmark::State& state) {
    static std::string content = [] {
        auto str = read_file_to_string("test_large.json");
        return str;
    }();

    for (auto _ : state) {
        auto result = pc::parser<json_parser>::parse(content, parse_context);
        benchmark::DoNotOptimize(result);
        volatile bool ok = result.success;
        if (!ok) {
            state.SkipWithError("Parse failed");
        }
    }

    state.SetBytesProcessed(state.iterations() * content.size());
}

BENCHMARK(BM_JsonParse_Win)
    ->Threads(1)
    ->Setup(SetAffinity)
    ->Unit(benchmark::kMillisecond)
    ->Iterations(30)
    ->Repetitions(5)
    ->DisplayAggregatesOnly(true);

BENCHMARK_MAIN();