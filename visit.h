#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>

#include "bad_variant_access.h"
#include "non_member.h"
#include "traits.h"
#include "variadic_union.h"

template <typename...> class variant;

namespace details {

template <typename T, size_t... Sizes> struct multi_array {
  T value;

  constexpr T &get() noexcept { return value; }

  constexpr T const &get() const noexcept { return value; }
};

template <typename T, size_t HeadSize, size_t... TailSizes> struct multi_array<T, HeadSize, TailSizes...> {
  multi_array<T, TailSizes...> subarray[HeadSize];

  template <typename HeadIndex, typename... TailIndexes>
  constexpr T &get(HeadIndex head_index, TailIndexes... tail_indexes) noexcept {
    return subarray[head_index].get(tail_indexes...);
  }

  template <typename HeadIndex, typename... TailIndexes>
  constexpr T const &get(HeadIndex head_index, TailIndexes... tail_indexes) const noexcept {
    return subarray[head_index].get(tail_indexes...);
  }
};

template <size_t... Indexes> struct invoker {
  template <typename ResultType, typename Visitor, typename... Variants>
  static constexpr ResultType invoke(Visitor &&visitor, Variants &&...variants) {
    return std::forward<Visitor>(visitor)(get<Indexes>(std::forward<Variants>(variants))...);
  }
};

template <typename ArrayType, typename ReturnType, typename SelectedIndexes, typename PushIndexes> class table;

template <typename ReturnType, size_t HeadSize, size_t... TailSizes, size_t... SelectedIndexes, size_t... PushIndexes,
          typename Visitor, typename... Variants>
class table<multi_array<ReturnType (*)(Visitor, Variants...), HeadSize, TailSizes...>, ReturnType,
            std::index_sequence<SelectedIndexes...>, std::index_sequence<PushIndexes...>> {
  static_assert(sizeof...(SelectedIndexes) + 1 + sizeof...(TailSizes) == sizeof...(Variants));

  using ArrayType = multi_array<ReturnType (*)(Visitor, Variants...), HeadSize, TailSizes...>;
  using SubArrayType = multi_array<ReturnType (*)(Visitor, Variants...), TailSizes...>;
  using invoke_ptr = ReturnType (*)(Visitor, Variants...);

  template <typename A, typename R, typename S, typename P> friend class table;

  ArrayType array;

  static constexpr ArrayType build_array() noexcept {
    constexpr size_t next_variant_size =
        variant_size_v<type_alternative_t<(sizeof...(SelectedIndexes) + 1), std::decay_t<Variants>..., variant<>>>;
    return ArrayType{table<SubArrayType, ReturnType, std::index_sequence<SelectedIndexes..., PushIndexes>,
                           std::make_index_sequence<next_variant_size>>::build_array()...};
  }

public:
  constexpr table() noexcept : array(build_array()) {}

  template <typename... Indexes> constexpr invoke_ptr get(Indexes &&...indexes) const noexcept {
    return array.get(indexes...);
  }
};

template <typename ReturnType, size_t... SelectedIndexes, typename Visitor, typename... Variants>
class table<multi_array<ReturnType (*)(Visitor, Variants...)>, ReturnType, std::index_sequence<SelectedIndexes...>,
            std::index_sequence<>> {
  static_assert(sizeof...(SelectedIndexes) == sizeof...(Variants));

  using ArrayType = multi_array<ReturnType (*)(Visitor, Variants...)>;
  using invoke_ptr = ReturnType (*)(Visitor, Variants...);

  template <typename A, typename R, typename S, typename P> friend class table;

  ArrayType array;

  static constexpr ArrayType build_array() noexcept {
    return ArrayType{&invoker<SelectedIndexes...>::template invoke<ReturnType, Visitor, Variants...>};
  }

public:
  constexpr table() noexcept : array(build_array()) {}

  constexpr invoke_ptr get() const noexcept { return array.get(); }
};

template <typename Visitor, typename... Types>
constexpr void visit(Visitor &&visitor,
                     variadic_union<(std::is_trivially_destructible_v<Types> && ...), Types...> &variadic_union,
                     size_t index) {
  using VariadicUnion = ::details::variadic_union<(std::is_trivially_destructible_v<Types> && ...), Types...>;
  using ArrayType = multi_array<void (*)(Visitor &&, VariadicUnion &), sizeof...(Types)>;
  constexpr auto t = table<ArrayType, void, std::index_sequence<>, std::make_index_sequence<sizeof...(Types)>>{};
  return t.get(index)(std::forward<Visitor>(visitor), variadic_union);
}

template <typename Visitor, typename... Types>
constexpr void visit(Visitor &&visitor,
                     variadic_union<(std::is_trivially_destructible_v<Types> && ...), Types...> const &variadic_union,
                     size_t index) {
  using VariadicUnion = ::details::variadic_union<(std::is_trivially_destructible_v<Types> && ...), Types...>;
  using ArrayType = multi_array<void (*)(Visitor &&, VariadicUnion const &), sizeof...(Types)>;
  constexpr auto t = table<ArrayType, void, std::index_sequence<>, std::make_index_sequence<sizeof...(Types)>>{};
  t.get(index)(std::forward<Visitor>(visitor), variadic_union);
}

} // namespace details

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor &&visitor, Variants &&...variants) {
  if ((variants.valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  using ReturnType = decltype(std::forward<Visitor>(visitor)(get<0>(std::forward<Variants>(variants))...));
  using ArrayType =
      details::multi_array<ReturnType (*)(Visitor &&, Variants && ...), variant_size_v<std::decay_t<Variants>>...>;
  constexpr auto table = details::table<
      ArrayType, ReturnType, std::index_sequence<>,
      std::make_index_sequence<variant_size_v<details::type_alternative_t<0, std::decay_t<Variants>...>>>>{};
  return table.get(variants.index()...)(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}
