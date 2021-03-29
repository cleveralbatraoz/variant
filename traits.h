#pragma once

#include <cstddef>
#include <type_traits>

template <typename... Types> class variant;

namespace details {

template <size_t Index, typename... Types> struct type_alternative { static_assert(sizeof...(Types) > 0); };

template <size_t Index, typename Head, typename... Tail> struct type_alternative<Index, Head, Tail...> {
  using type = typename type_alternative<Index - 1, Tail...>::type;
};

template <typename Head, typename... Tail> struct type_alternative<0, Head, Tail...> { using type = Head; };

template <size_t Index, typename... Types> using type_alternative_t = typename type_alternative<Index, Types...>::type;

template <size_t Skipped, typename Required, typename... Types> struct type_index_helper {
  static_assert(sizeof...(Types) > 0);
};

template <size_t SkippedCount, typename Required, typename Head, typename... Tail>
struct type_index_helper<SkippedCount, Required, Head, Tail...> {
  constexpr static size_t value = type_index_helper<SkippedCount + 1, Required, Tail...>::value;
};

template <size_t SkippedCount, typename Required, typename... Types>
struct type_index_helper<SkippedCount, Required, Required, Types...> {
  constexpr static size_t value = SkippedCount;
};

template <typename Required, typename... Types> using type_index = type_index_helper<0, Required, Types...>;

template <typename Required, typename... Types>
inline constexpr size_t type_index_v = type_index<Required, Types...>::value;

template <typename Required, typename Head, typename... Tail>
struct type_counter
    : std::integral_constant<size_t, std::is_same_v<Required, Head> + type_counter<Required, Tail...>::value> {};

template <typename Required, typename Head>
struct type_counter<Required, Head> : std::integral_constant<size_t, std::is_same_v<Required, Head>> {};

template <typename Required, typename... Types>
inline constexpr size_t type_counter_v = type_counter<Required, Types...>::value;

template <typename Required, typename... Types>
struct is_unique_type : std::integral_constant<bool, type_counter_v<Required, Types...> == 1> {};

template <typename Required, typename... Types>
inline constexpr bool is_unique_type_v = is_unique_type<Required, Types...>::value;

template <typename A, template <typename...> typename B> struct is_specialization : std::false_type {};

template <template <typename...> typename T, typename... Args>
struct is_specialization<T<Args...>, T> : std::true_type {};

template <typename A, template <typename...> typename B>
inline constexpr bool is_specialization_v = is_specialization<A, B>::value;

template <typename T> struct is_in_place_index_specialization : std::false_type {};

template <size_t Index> struct is_in_place_index_specialization<in_place_index_t<Index>> : std::true_type {};

template <typename T>
inline constexpr bool is_in_place_index_specialization_v = is_in_place_index_specialization<T>::value;

template <typename T> struct array_holder { T array[1]; };

template <typename A, typename B, typename Createable = void> struct creatable : std::false_type {};

template <> struct creatable<bool, bool> : std::true_type {};

template <typename B> struct creatable<bool, B> : std::false_type {};

template <typename A, typename B>
struct creatable<A, B, std::void_t<decltype(array_holder<A>{{std::declval<B>()}})>> : std::true_type {};

template <typename A, typename B> inline constexpr bool creatable_v = creatable<A, B>::value;

template <typename Required, typename Variant, bool Creatable> struct overloader;

template <typename Required, typename HeadType, typename... TailTypes>
struct overloader<Required, variant<HeadType, TailTypes...>, true>
    : overloader<Required, variant<TailTypes...>, creatable_v<type_alternative_t<0, TailTypes...>, Required>> {
  static constexpr HeadType function(HeadType);

  using base = overloader<Required, variant<TailTypes...>, creatable_v<type_alternative_t<0, TailTypes...>, Required>>;
  using base::function;
};

template <typename Required, typename HeadType, typename... TailTypes>
struct overloader<Required, variant<HeadType, TailTypes...>, false>
    : overloader<Required, variant<TailTypes...>, creatable_v<type_alternative_t<0, TailTypes...>, Required>> {
  static constexpr void function();

  using base = overloader<Required, variant<TailTypes...>, creatable_v<type_alternative_t<0, TailTypes...>, Required>>;
  using base::function;
};

template <typename Required, typename T> struct overloader<Required, variant<T>, true> {
  static constexpr T function(T);
};

template <typename Required, typename T> struct overloader<Required, variant<T>, false> {
  static constexpr void function();
};

template <typename Required, typename Variant, typename Result = void> struct type_matcher { using type = void; };

template <typename Required, typename... Types>
struct type_matcher<Required, variant<Types...>,
                    std::void_t<decltype(overloader<Required, variant<Types...>,
                                                    creatable_v<type_alternative_t<0, Types...>,
                                                                Required>>::function(std::declval<Required>()))>> {
  using type =
      decltype(overloader<Required, variant<Types...>,
                          creatable_v<type_alternative_t<0, Types...>, Required>>::function(std::declval<Required>()));
};

template <typename Required, typename Variant> using type_matcher_t = typename type_matcher<Required, Variant>::type;

} // namespace details
