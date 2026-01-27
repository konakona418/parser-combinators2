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

#pragma once

#include <cassert>
#include <meta>

namespace parser_combinators {
    namespace details {
        using std::literals::string_view_literals::operator""sv;

        template <typename T>
        concept ParserTrait = requires {
            T::parse(std::declval<std::string_view>());
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

        template <ParserTrait BaseParser, parser_attribute Attr>
        struct parser_decorator {
            static auto parse(std::string_view input) {
                return BaseParser::parse(input);
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
            static auto parse(std::string_view input) {
                constexpr auto s = Str.str();
                if (!input.starts_with(s)) {
                    return parse_failure_s(input);
                } 
                return parse_success_s(input.substr(0, s.size()), input.substr(s.size()));
            }
        };

        struct alpha_parser {
            static auto parse(std::string_view input) {
                if (input.empty() || !std::isalpha(input[0])) {
                    return parse_failure_s(input);
                }
                return parse_success_s(input.substr(0, 1), input.substr(1));
            }
        };

        struct numeric_parser {
            static auto parse(std::string_view input) {
                if (input.empty() || !std::isdigit(input[0])) {
                    return parse_failure_s(input);
                }
                return parse_success_s(input.substr(0, 1), input.substr(1));
            }
        };

        struct alphanumeric_parser {
            static auto parse(std::string_view input) {
                if (input.empty() || !std::isalnum(input[0])) {
                    return parse_failure_s(input);
                }
                return parse_success_s(input.substr(0, 1), input.substr(1));
            }
        };

        struct whitespace_parser {
            static auto parse(std::string_view input) {
                size_t i = 0;
                while (i < input.size() && std::isspace(input[i])) {
                    ++i;
                }
                return parse_success_s(input.substr(0, i), input.substr(i));
            }
        };

        template <ParserTrait BaseParser>
        struct collect_parser {
            static auto parse(std::string_view input) {
                auto [success, parsed, remaining] = BaseParser::parse(input);
                if (!success) {
                    return parse_failure_s(input);
                }
                size_t consumed = input.size() - remaining.size();
                return parse_success_s(input.substr(0, consumed), remaining);
            }
        };

        template <ParserTrait BaseParser>
        struct optional_parser {
            static auto parse(std::string_view input) {
                using ElementType = typename decltype(BaseParser::parse(input))::value_type;
                auto result = BaseParser::parse(input);
                if (result.success) {
                    return parse_manual_unsafe<std::optional<ElementType>>(true, result.parsed, result.remaining);
                } else {
                    return parse_manual_unsafe<std::optional<ElementType>>(true, std::optional<ElementType>{std::nullopt}, input);
                }
            }
        };

        template <ParserTrait BaseParser>
        struct many_parser {
            static auto parse(std::string_view input) {
                using ElementType = typename decltype(BaseParser::parse(input))::value_type;
                std::string_view remaining = input;

                std::vector<ElementType> results;
                while (true) {
                    auto result = BaseParser::parse(remaining);
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
            static auto parse(std::string_view input) {
                assert(false && "Invalid parser configuration");
                return parse_failure<std::string_view>(input);
            }
        };

        template <ParserTrait... SubParsers>
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

        template <ParserTrait... SubParsers>
        consteval auto construct_return_tuple_type() {
            constexpr auto mapping = make_member_index_map<SubParsers...>();
            using SubParsersTuple = std::tuple<SubParsers...>;

            std::vector<std::meta::info> types;
            // !!! shit
            // look out for indices...
            template for (constexpr auto i : std::define_static_array(details::make_index_array<sizeof...(SubParsers)>())) {
                if constexpr (mapping[i] != -1) {
                    using SubParser = std::tuple_element_t<i, SubParsersTuple>;
                    using ParsedType = typename decltype(SubParser::parse(""sv))::value_type;
                    types.push_back(^^ParsedType);
                }
            }

            return std::meta::substitute(^^std::tuple, std::define_static_array(types));
        }

        template <ParserTrait BaseParser, auto Fmap>
        struct fmap_parser {
            static auto parse(std::string_view input) {
                auto result = BaseParser::parse(input);
                if (!result.success) {
                    return parse_failure<decltype(apply_fmap(result.parsed))>(input);
                }
                return parse_success<decltype(apply_fmap(result.parsed))>(apply_fmap(result.parsed), result.remaining);
            }

            static auto apply_fmap(auto&& val) {
                if constexpr (is_tuple_v<std::decay_t<decltype(val)>>) {
                    return std::apply(Fmap, val);
                } else {
                    return std::invoke(Fmap, std::forward<decltype(val)>(val));
                }
            }
        };

        template <ParserTrait... SubParsers>
        struct choice_parser {
            static auto parse(std::string_view input) {
                using ParserOutputs = std::variant<typename decltype(SubParsers::parse(input))::value_type...>;
                bool any_success = false;

                ParserOutputs parsed_value;
                template for (constexpr int i : std::define_static_array(details::make_index_array<sizeof...(SubParsers)>())) {
                    using SubParserTuple = std::tuple<SubParsers...>;
                    using SubParser = std::tuple_element_t<i, SubParserTuple>;

                    auto result = SubParser::parse(input);
                    if (result.success) {
                        any_success = true;
                        parsed_value.template emplace<i>(std::move(result.parsed));
                        input = result.remaining;
                        break;
                    }
                }

                if (!any_success) {
                    return parse_failure<std::variant<typename decltype(SubParsers::parse(input))::value_type...>>(input);
                }

                return parse_success<std::variant<typename decltype(SubParsers::parse(input))::value_type...>>(parsed_value, input);
            }
        };
        
        template <typename OutType, ParserTrait... SubParsers>
        struct [[deprecated("struct_mapper_parser is deprecated, use sequential_parser with fmap functionalities")]] struct_mapper_parser {
            static auto parse(std::string_view input) {
                OutType output{};
                std::string_view parse_remaining = input;

                constexpr auto out_info = ^^OutType;
                constexpr auto member_infos = std::define_static_array(std::meta::nonstatic_data_members_of(out_info,  std::meta::access_context::unchecked()));
                constexpr auto subparser_infos = std::array<std::meta::info, sizeof...(SubParsers)>{^^SubParsers...};
                
                static_assert(member_infos.size() <= subparser_infos.size(), "Too many members in output type for the provided parsers");

                constexpr auto member_index_map = std::define_static_array(make_member_index_map<SubParsers...>());

                // why the fuck...
                // this is just so cplusplus-ish
                using SubParserTuple = std::tuple<SubParsers...>;
                template for (constexpr int i : std::define_static_array(details::make_index_array<sizeof...(SubParsers)>())) {
                    using SubParser = std::tuple_element_t<i, SubParserTuple>;
                    constexpr auto split = split_parser_decorator(subparser_infos[i]);
                    constexpr auto base_parser_info = split.first;
                    constexpr auto attr = split.second;
                    auto [succ, parsed, remaining] = SubParser::parse(parse_remaining);
                    
                    if (!succ) {
                        return parse_failure<OutType>(input);
                    }

                    constexpr int target_idx = member_index_map[i];
                    if constexpr (target_idx != -1) {
                        constexpr auto member_info = member_infos[target_idx];
                        output.[:member_info:] = parsed;
                    }

                    parse_remaining = remaining;
                }

                return parse_success<OutType>(output, parse_remaining);
            }
        };

        
        template <ParserTrait... SubParsers>
        struct sequential_parser {
            static auto parse(std::string_view input) {
                using ReturnTupleType = [:construct_return_tuple_type<SubParsers...>():];
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
                    auto [succ, parsed, remaining] = SubParser::parse(parse_remaining);

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
            consteval auto discard() -> parser_wrapper {
                parser_wrapper pw = *this;
                pw.should_discard = true;
                return pw;
            }

            // maybe
            consteval auto optional() -> parser_wrapper {
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

            // 0..n
            consteval auto many() -> parser_wrapper {
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
            consteval auto collect() -> parser_wrapper {
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
            template <auto Fmap>
            consteval auto map() -> parser_wrapper {
                parser_wrapper pw = *this;
                pw.basic_parser = std::meta::substitute(
                    ^^fmap_parser,
                    {pw.basic_parser, std::meta::reflect_constant(Fmap)}
                );
                return pw;
            }
            
            consteval auto parser() {
                return basic_parser;
            }
        };

        struct combine_partial_parser {
            std::vector<std::meta::info> parser_types;

            template <typename OutType>
            consteval auto map() {
                std::vector<std::meta::info> types;
                types.push_back(^^OutType);
                types.reserve(types.size() + parser_types.size());
                types.insert(types.end(), parser_types.begin(), parser_types.end());

                auto combined_parser = std::meta::substitute(
                    ^^details::struct_mapper_parser,
                    types
                );

                return details::parser_wrapper{combined_parser};
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

        consteval auto combine(std::initializer_list<details::parser_wrapper> parsers) {
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

        consteval auto choice(std::initializer_list<details::parser_wrapper> parsers) {
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
        constexpr fmap_struct_t<OutType> fmap_struct{};
    }
}
