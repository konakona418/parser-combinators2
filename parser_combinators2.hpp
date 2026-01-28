// parser_combinators2.hpp
// An experimental parser combinator library using C++26 reflection features
//
// Changelog:
//
// 2026-01-27: 
//      basic parser combinators implemented: 
//      symbol, alpha, numeric, optional, many, choice, combine
// 2026-01-27:
//      Added collect, fmap functionalities
// 2026-01-27:
//      Added recursive parsers via Y-combinator
//      Convenient dsl::fmap to prevent writing .template fmap<...>() everywhere
// 2026-01-28:
//      Some refactoring and cleanup
//      Added parse_context for error reporting
//      Added expect decorator for better error messages
//      Added guard decorator for predicate checks

#pragma once

#include <cassert>
#include <meta>

namespace parser_combinators {
    namespace details {
        using std::literals::string_view_literals::operator""sv;

        template <typename T>
        concept parser_trait = requires {
            T::parse(std::declval<std::string_view>(), std::declval<struct parse_context&>());
        };

        template <typename T>
        static constexpr auto is_tuple_v = requires {
            typename std::tuple_size<T>::type;
        };

        template <std::size_t N>
        struct string_literal {
            char value[N];

            constexpr string_literal(const char (&str)[N]) {
                for (std::size_t i = 0; i < N; ++i) {
                    value[i] = str[i];
                }
            }

            constexpr std::string_view str() const {
                return std::string_view(value, N - 1);
            }
        };

        template <typename T, size_t N>
        struct list_wrapper {
            T data[N];

            template <typename... Ps>
            consteval list_wrapper(Ps... ps) : data{ps...} {}

            consteval size_t size() const {
                return N;
            }
        };
        
        template <size_t n>
        consteval auto make_index_array() {
            std::array<size_t, n> arr = {};
            for (size_t i = 0; i < n; ++i) {
                arr[i] = i;
            }
            return arr;
        }

        consteval auto data_member_at_index(std::meta::info struct_info, size_t index) {
            auto ctx = std::meta::access_context::unchecked();
            auto members = std::meta::nonstatic_data_members_of(struct_info, ctx);
            return members[index];
        }

        template <typename OutType = std::string_view>
        struct parse_result {
            using value_type = OutType;

            bool success;
            OutType parsed;
            std::string_view remaining;
        };

        template <typename OutType>
        constexpr auto parse_success(OutType parsed, std::string_view remaining) {
            return parse_result<OutType>{true, parsed, remaining};
        }

        template <typename OutType>
        constexpr auto parse_failure(std::string_view remaining) {
            return parse_result<OutType>{false, OutType{}, remaining};
        }

        template <typename OutType>
        constexpr auto parse_manual_unsafe(bool success, OutType parsed, std::string_view remaining) {
            return parse_result<OutType>{success, parsed, remaining};
        }

        constexpr auto parse_success_s(std::string_view parsed, std::string_view remaining) {
            return parse_result<std::string_view>{true, parsed, remaining};
        }

        constexpr auto parse_failure_s(std::string_view remaining) {
            return parse_result<std::string_view>{false, std::string_view{}, remaining};
        }

        struct parse_error {
            std::string message;
            uintptr_t ptr_pos = 0;

            size_t diff(const char* base_ptr) const {
                return ptr_pos - reinterpret_cast<uintptr_t>(base_ptr);
            }
        };

        struct parse_context {
            parse_error error;

            void add_error(std::string_view message, const char* ptr_position) {
                if (error.ptr_pos <= reinterpret_cast<uintptr_t>(ptr_position)) {
                    // only record the farthest error
                    error.message = message;
                    error.ptr_pos = reinterpret_cast<uintptr_t>(ptr_position);
                }
            }

            void reset() {
                error = parse_error{};
            }
        };

        enum class parser_attribute : int {
            none = 0,
            discard = 1,
            optional = 2,
            many = 4,
        };

        inline constexpr parser_attribute operator|(parser_attribute a, parser_attribute b) {
            return static_cast<parser_attribute>(static_cast<int>(a) | static_cast<int>(b));
        }

        inline constexpr parser_attribute operator&(parser_attribute a, parser_attribute b) {
            return static_cast<parser_attribute>(static_cast<int>(a) & static_cast<int>(b));
        }

        inline constexpr parser_attribute& operator|=(parser_attribute& a, parser_attribute b) {
            a = a | b;
            return a;
        }

        inline constexpr parser_attribute& operator&=(parser_attribute& a, parser_attribute b) {
            a = a & b;
            return a;
        }

        inline constexpr bool has_attribute(parser_attribute attrs, parser_attribute check) {
            return (attrs & check) != parser_attribute::none;
        }

        template <parser_trait BaseParser, parser_attribute Attr>
        struct parser_decorator {
            using value_type = typename BaseParser::value_type;
            static auto parse(std::string_view input, parse_context& ctx) {
                return BaseParser::parse(input, ctx);
            }
        };

        consteval auto split_parser_decorator(std::meta::info decorated_parser) -> std::pair<std::meta::info, parser_attribute> {
            auto args = std::meta::template_arguments_of(decorated_parser);
            assert(args.size() == 2 && "parser_decorator must have exactly 2 template arguments");

            auto base_parser = args[0];
            auto attr_constant = args[1];
            auto attr_value = std::meta::extract<parser_attribute>(attr_constant);

            return {base_parser, attr_value};
        }

        template <string_literal Str>
        struct symbol_parser {
            using value_type = std::string_view;
            static auto parse(std::string_view input, parse_context& ctx) {
                constexpr auto s = Str.str();
                if (!input.starts_with(s)) {
                    ctx.add_error("Expected symbol: " + std::string(s), input.data());
                    return parse_failure_s(input);
                } 
                return parse_success_s(input.substr(0, s.size()), input.substr(s.size()));
            }
        };

        struct alpha_parser {
            using value_type = std::string_view;
            static auto parse(std::string_view input, parse_context& ctx) {
                if (input.empty() || !std::isalpha(input[0])) {
                    ctx.add_error("Expected alphabetic character", input.data());
                    return parse_failure_s(input);
                }
                return parse_success_s(input.substr(0, 1), input.substr(1));
            }
        };

        struct numeric_parser {
            using value_type = std::string_view;
            static auto parse(std::string_view input, parse_context& ctx) {
                if (input.empty() || !std::isdigit(input[0])) {
                    ctx.add_error("Expected numeric character", input.data());
                    return parse_failure_s(input);
                }
                return parse_success_s(input.substr(0, 1), input.substr(1));
            }
        };

        struct alphanumeric_parser {
            using value_type = std::string_view;
            static auto parse(std::string_view input, parse_context& ctx) {
                if (input.empty() || !std::isalnum(input[0])) {
                    ctx.add_error("Expected alphanumeric character", input.data());
                    return parse_failure_s(input);
                }
                return parse_success_s(input.substr(0, 1), input.substr(1));
            }
        };

        struct whitespace_parser {
            using value_type = std::string_view;
            static auto parse(std::string_view input, parse_context& ctx) {
                if (input.empty() || !std::isspace(input[0])) {
                    
                    return parse_failure_s(input);
                }
                return parse_success_s(input.substr(0, 1), input.substr(1));
            }
        };

        template <parser_trait BaseParser>
        struct collect_parser {
            using value_type = std::string_view;
            static auto parse(std::string_view input, parse_context& ctx) {
                auto [success, parsed, remaining] = BaseParser::parse(input, ctx);
                if (!success) {
                    return parse_failure_s(input);
                }
                size_t consumed = input.size() - remaining.size();
                return parse_success_s(input.substr(0, consumed), remaining);
            }
        };

        template <parser_trait BaseParser>
        struct optional_parser {
            using value_type = std::optional<typename BaseParser::value_type>;
            static auto parse(std::string_view input, parse_context& ctx) {
                using ElementType = typename decltype(BaseParser::parse(input, ctx))::value_type;
                auto result = BaseParser::parse(input, ctx);
                if (result.success) {
                    return parse_manual_unsafe<std::optional<ElementType>>(true, result.parsed, result.remaining);
                } else {
                    return parse_manual_unsafe<std::optional<ElementType>>(true, std::optional<ElementType>{std::nullopt}, input);
                }
            }
        };

        template <string_literal Expect, parser_trait BaseParser>
        struct expect_parser {
            using value_type = typename BaseParser::value_type;
            static auto parse(std::string_view input, parse_context& ctx) {
                auto result = BaseParser::parse(input, ctx);
                if (!result.success) {
                    ctx.add_error(std::string(Expect.str()), input.data());
                    return parse_failure<typename BaseParser::value_type>(input);
                }
                return result;
            }
        };

        template <parser_trait BaseParser, auto Predicate>
        struct guard_parser {
            using value_type = BaseParser::value_type;
            static auto parse(std::string_view input, parse_context& ctx) {
                auto result = BaseParser::parse(input, ctx);
                if (!result.success) {
                    return parse_failure<typename BaseParser::value_type>(input);
                }

                if (!Predicate(result.parsed)) {
                    ctx.add_error("Guard condition failed", input.data());
                    return parse_failure<typename BaseParser::value_type>(input);
                }
                return result;
            }
        };

        template <parser_trait BaseParser>
        struct many_parser {
            using value_type = std::vector<typename BaseParser::value_type>;
            static auto parse(std::string_view input, parse_context& ctx) {
                using ElementType = typename BaseParser::value_type;
                std::string_view remaining = input;

                std::vector<ElementType> results;
                while (true) {
                    auto result = BaseParser::parse(remaining, ctx);
                    if (!result.success || result.remaining == remaining) {
                        break;
                    }
                    results.push_back(std::move(result.parsed));
                    remaining = result.remaining;
                }
                return parse_manual_unsafe<std::vector<ElementType>>(true, results, remaining);
            }
        };

        template <string_literal Reason>
        struct invalid_parser {
            using value_type = std::string_view;
            static auto parse(std::string_view input, parse_context& ctx) {
                assert(false && "Invalid parser configuration");
                ctx.add_error("Invalid parser configuration: " + std::string(Reason.str()), input.data());
                return parse_failure<std::string_view>(input);
            }
        };

        template <parser_trait... SubParsers>
        consteval auto make_member_index_map() {
            constexpr auto attrs = std::array<std::meta::info, sizeof...(SubParsers)>{^^SubParsers...};
            std::array<int, sizeof...(SubParsers)> mapping{};
            int current_member = 0;
            for(size_t i = 0; i < attrs.size(); ++i) {
                auto [_, attr] = split_parser_decorator(attrs[i]);
                if (has_attribute(attr, parser_attribute::discard)) {
                    mapping[i] = -1;
                } else {
                    mapping[i] = current_member++;
                }
            }
            return mapping;
        }

        template <parser_trait... SubParsers>
        consteval auto construct_return_tuple_type() {
            constexpr auto mapping = make_member_index_map<SubParsers...>();
            using SubParsersTuple = std::tuple<SubParsers...>;

            std::vector<std::meta::info> types;
            // !!! shit
            // look out for indices...
            template for (constexpr auto i : std::define_static_array(details::make_index_array<sizeof...(SubParsers)>())) {
                if constexpr (mapping[i] != -1) {
                    using SubParser = std::tuple_element_t<i, SubParsersTuple>;
                    using ParsedType = typename SubParser::value_type;
                    types.push_back(^^ParsedType);
                }
            }

            return std::meta::substitute(^^std::tuple, std::define_static_array(types));
        }

        template <parser_trait BaseParser, auto Fmap>
        struct fmap_parser {
            static auto parse(std::string_view input, parse_context& ctx) {
                auto result = BaseParser::parse(input, ctx);
                using mapped_type = decltype(apply_fmap(result.parsed));
                if (!result.success) {
                    return parse_failure<mapped_type>(input);
                }
                return parse_success<mapped_type>(apply_fmap(result.parsed), result.remaining);
            }

            static auto apply_fmap(auto&& val) {
                if constexpr (is_tuple_v<std::decay_t<decltype(val)>>) {
                    return std::apply(Fmap, val);
                } else {
                    return std::invoke(Fmap, std::forward<decltype(val)>(val));
                }
            }

            using value_type = decltype(apply_fmap(std::declval<typename BaseParser::value_type>()));
        };

        template <parser_trait... SubParsers>
        struct choice_parser {
            using value_type = std::variant<typename SubParsers::value_type...>;
            static auto parse(std::string_view input, parse_context& ctx) {
                using ParserOutputs = value_type;

                const auto original_input = input;
                template for (constexpr int i : std::define_static_array(details::make_index_array<sizeof...(SubParsers)>())) {
                    using SubParserTuple = std::tuple<SubParsers...>;
                    using SubParser = std::tuple_element_t<i, SubParserTuple>;

                    auto result = SubParser::parse(original_input, ctx);
                    if (result.success) {
                        ParserOutputs parsed_value;
                        parsed_value.template emplace<i>(std::move(result.parsed));
                        return parse_success<ParserOutputs>(parsed_value, result.remaining);
                    }
                }

                return parse_failure<std::variant<typename SubParsers::value_type...>>(input);
            }
        };
        
        template <parser_trait... SubParsers>
        struct sequential_parser {
            using value_type = [:construct_return_tuple_type<SubParsers...>():];
            static auto parse(std::string_view input, parse_context& ctx) {
                using ReturnTupleType = value_type;
                using SubParsersTuple = std::tuple<SubParsers...>;

                ReturnTupleType result_tuple{};
                std::string_view parse_remaining = input;

                constexpr auto member_index_map = make_member_index_map<SubParsers...>();
                constexpr auto subparser_infos = std::array<std::meta::info, sizeof...(SubParsers)>{^^SubParsers...};
                template for (constexpr int i : std::define_static_array(details::make_index_array<sizeof...(SubParsers)>())) {
                    using SubParser = std::tuple_element_t<i, SubParsersTuple>;
                    constexpr auto split = split_parser_decorator(subparser_infos[i]);
                    constexpr auto base_parser_info = split.first;
                    constexpr auto attr = split.second;
                    auto [succ, parsed, remaining] = SubParser::parse(parse_remaining, ctx);
                    if (!succ) {
                        return parse_failure<ReturnTupleType>(input);
                    }

                    constexpr int target_idx = member_index_map[i];
                    if constexpr (target_idx != -1) {
                        std::get<target_idx>(result_tuple) = parsed;
                    }

                    parse_remaining = remaining;
                }

                return parse_success<ReturnTupleType>(result_tuple, parse_remaining);
            }
        };

        struct parser_wrapper {
            std::meta::info basic_parser;
            bool should_discard = false;
            bool is_optional = false;
            bool is_many = false;

            // discard the result of this parser
            consteval auto discard() const -> parser_wrapper {
                parser_wrapper pw = *this;
                pw.should_discard = true;
                return pw;
            }

            // maybe
            consteval auto optional() const -> parser_wrapper {
                parser_wrapper pw = *this;
                if (is_many) {
                    // many = 0..n, optional = 0..1, conflicting semantics
                    pw.basic_parser = ^^invalid_parser<"Cannot apply 'optional' to a many parser">;
                    return pw;
                }

                pw.is_optional = true;
                pw.basic_parser = std::meta::substitute(
                    ^^optional_parser,
                    {pw.basic_parser}
                );
                return pw;
            }

            template <string_literal Expect>
            consteval auto expect() const -> parser_wrapper {
                parser_wrapper pw = *this;
                pw.basic_parser = std::meta::substitute(
                    ^^expect_parser,
                    {std::meta::reflect_constant(Expect), pw.basic_parser}
                );
                return pw;
            }

            template <auto Predicate>
            consteval auto guard() const -> parser_wrapper {
                parser_wrapper pw = *this;
                pw.basic_parser = std::meta::substitute(
                    ^^guard_parser,
                    {pw.basic_parser, std::meta::reflect_constant(Predicate)}
                );
                return pw;
            }

            // 0..n
            consteval auto many() const -> parser_wrapper {
                parser_wrapper pw = *this;
                if (is_optional) {
                    // same
                    pw.basic_parser = ^^invalid_parser<"Cannot apply 'many' to an optional parser">;
                    return pw;
                }

                pw.is_many = true;
                pw.basic_parser = std::meta::substitute(
                    ^^many_parser,
                    {pw.basic_parser}
                );
                return pw;
            }

            // collect the raw input consumed by this parser
            // note that this will override many/optional settings
            // as well as any structural outputs
            consteval auto collect() const -> parser_wrapper {
                parser_wrapper pw = *this;
                pw.is_many = false;
                pw.is_optional = false;
                pw.basic_parser = std::meta::substitute(
                    ^^collect_parser,
                    {pw.basic_parser}
                );
                return pw;
            }

            // fmap
            // for constexpr lambdas and function pointers, etc
            template <auto Fmap>
            consteval auto fmap() const -> parser_wrapper {
                parser_wrapper pw = *this;
                pw.basic_parser = std::meta::substitute(
                    ^^fmap_parser,
                    {pw.basic_parser, std::meta::reflect_constant(Fmap)}
                );
                return pw;
            }

            // still fmap
            // for capture lambdas
            consteval auto fmap(auto Fmap) const -> parser_wrapper {
                parser_wrapper pw = *this;
                pw.basic_parser = std::meta::substitute(
                    ^^fmap_parser,
                    {pw.basic_parser, std::meta::reflect_constant(Fmap)}
                );
                return pw;
            }
            
            consteval auto parser() const {
                return basic_parser;
            }
        };

        template <typename OutType, auto F>
        struct y_combinator {
            static auto parse(std::string_view input, parse_context& ctx) -> parse_result<OutType> {
                struct recurse_helper {
                    using value_type = OutType;
                    static auto parse(std::string_view input, parse_context& ctx) -> parse_result<OutType> {
                        return y_combinator<OutType, F>::parse(input, ctx);
                    }
                };

                static constexpr auto cached_parser = [] {
                    constexpr auto wrapper = F(parser_wrapper{^^recurse_helper});
                    return wrapper.basic_parser;
                }();

                // i just cant inline this, or clangd freaks out
                // goddamn it
                using parser = [:cached_parser:];
                return parser::parse(input, ctx);
            }
        };
    }

    namespace dsl {
        template <details::string_literal Str>
        consteval auto symbol() {
            auto parser_type = std::meta::substitute(
                ^^details::symbol_parser,
                {std::meta::reflect_constant(Str)}
            );

            return details::parser_wrapper{parser_type};
        }

        consteval auto alpha() {
            return details::parser_wrapper{^^details::alpha_parser};
        }

        consteval auto numeric() {
            return details::parser_wrapper{^^details::numeric_parser};
        }

        consteval auto alphanumeric() {
            return details::parser_wrapper{^^details::alphanumeric_parser};
        }

        consteval auto whitespace() {
            return details::parser_wrapper{^^details::whitespace_parser};
        }

        consteval auto combine(std::span<const details::parser_wrapper> parsers) {
            std::vector<std::meta::info> parser_types;
            for (const auto& p : parsers) {
                details::parser_attribute attr = details::parser_attribute::none;
                attr |= p.should_discard ? details::parser_attribute::discard : details::parser_attribute::none;
                attr |= p.is_optional ? details::parser_attribute::optional : details::parser_attribute::none;
                attr |= p.is_many ? details::parser_attribute::many : details::parser_attribute::none;

                // just put a attr here, or extra case handling
                // whatever, screw it
                parser_types.push_back(
                        std::meta::substitute(
                            ^^details::parser_decorator,
                            {p.basic_parser, std::meta::reflect_constant(attr)}
                        )
                    );
            }

            auto combined_parser = std::meta::substitute(
                ^^details::sequential_parser,
                parser_types
            );

            return details::parser_wrapper{combined_parser};
        }

        consteval auto combine(std::initializer_list<details::parser_wrapper> parsers) {
            return combine(std::span{parsers.begin(), parsers.size()});
        }

        template <size_t N>
        struct parser_list : details::list_wrapper<details::parser_wrapper, N> {
            using details::list_wrapper<details::parser_wrapper, N>::list_wrapper;
        };

        template <typename... Ps>
        parser_list(Ps...) -> parser_list<sizeof...(Ps)>;

        // if you are using y-combinators, this is not going to work
        template <parser_list Initializer>
        consteval auto combine() {
            std::vector<details::parser_wrapper> parsers;
            for (size_t i = 0; i < Initializer.size(); ++i) {
                parsers.push_back(Initializer.data[i]);
            }
            return combine(std::span{parsers.data(), parsers.size()});
        }

        consteval auto choice(std::span<const details::parser_wrapper> parsers) {
            std::vector<std::meta::info> parser_types;
            for (const auto& p : parsers) {
                details::parser_attribute attr = details::parser_attribute::none;
                attr |= p.should_discard ? details::parser_attribute::discard : details::parser_attribute::none;
                attr |= p.is_optional ? details::parser_attribute::optional : details::parser_attribute::none;
                attr |= p.is_many ? details::parser_attribute::many : details::parser_attribute::none;

                parser_types.push_back(
                        std::meta::substitute(
                            ^^details::parser_decorator,
                            {p.basic_parser, std::meta::reflect_constant(attr)}
                        )
                    );
            }

            auto choice_parser = std::meta::substitute(
                ^^details::choice_parser,
                parser_types
            );

            return details::parser_wrapper{choice_parser};
        }

        consteval auto choice(std::initializer_list<details::parser_wrapper> parsers) {
            return choice(std::span{parsers.begin(), parsers.size()});
        }

        // if you are using y-combinators, this is not going to work
        template <parser_list Initializer>
        consteval auto choice() {
            std::vector<details::parser_wrapper> parsers;
            for (size_t i = 0; i < Initializer.size(); ++i) {
                parsers.push_back(Initializer.data[i]);
            }
            return choice(std::span{parsers.data(), parsers.size()});
        }

        template <typename OutType, typename F>
        consteval auto fix(F&& fn) {
            auto parser_type = std::meta::substitute(
                ^^details::y_combinator,
                {^^OutType, std::meta::reflect_constant(fn)}
            );

            return details::parser_wrapper{parser_type};
        }

        template <auto F>
        struct fmap_helper {};

        template <auto F>
        constexpr inline auto fmap = fmap_helper<F>{};

        template <typename ParserWrapper, auto F>
        consteval auto operator|(ParserWrapper pw, fmap_helper<F>) {
            return pw.template fmap<F>();
        }

        template <auto P>
        struct guard_helper {};

        template <auto P>
        constexpr inline auto guard = guard_helper<P>{};

        template <typename ParserWrapper, auto P>
        consteval auto operator|(ParserWrapper pw, guard_helper<P>) {
            return pw.template guard<P>();
        }

        template <typename F>
        struct lazy_helper {
            F func;

            template <typename OutType>
            consteval auto fix() const {
                return dsl::fix<OutType>(func);
            }
        };

        template <auto F>
        constexpr inline auto lazy = lazy_helper{F};

        template <typename OutType>
        struct fmap_struct_t {
            auto operator()(auto&&... args) const {
                std::tuple tpl{std::forward<decltype(args)>(args)...};

                OutType out{};
                constexpr auto arg_size = sizeof...(args);
                template for (constexpr int i : std::define_static_array(details::make_index_array<arg_size>())) {
                    constexpr auto member_info = details::data_member_at_index(^^OutType, i);
                    out.[:member_info:] = std::get<i>(tpl);
                }

                return out;
            }
        };

        template <typename OutType>
        constexpr auto fmap_struct = fmap_struct_t<OutType>{};
    }

    namespace preds {
        constexpr auto always_true = [](auto&&...) {
            return true;
        };

        constexpr auto always_false = [](auto&&...) {
            return false;
        };

        constexpr auto not_empty = [](auto&& val) {
            return !val.empty();
        };
    }

    constexpr auto context() {
        return details::parse_context{};
    }

    template <details::parser_wrapper ParserWrapper>
    struct parser {
        using parser_type = [:ParserWrapper.parser():];

        static auto parse(std::string_view input, details::parse_context& ctx) {
            return parser_type::parse(input, ctx);
        }
    };
}
