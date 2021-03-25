#pragma once

#include <cstddef>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <utility>

#include "bad_variant_access.h"
#include "in_place.h"
#include "traits.h"
#include "variadic_union.h"

template <typename... Types> class variant;

inline constexpr size_t variant_npos = -1;

template <typename T> struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <typename Variant> struct variant_size<Variant const> : variant_size<Variant> {};

template <typename Variant> struct variant_size<Variant volatile> : variant_size<Variant> {};

template <typename Variant> struct variant_size<Variant const volatile> : variant_size<Variant> {};

template <typename Variant> inline constexpr size_t variant_size_v = variant_size<Variant>::value;

template <size_t Index, typename Variant> struct variant_alternative {
  static_assert(std::is_same_v<Variant, variant<>>);
};

template <size_t Index, typename... Types> struct variant_alternative<Index, variant<Types...>> {
  using type = details::type_alternative_t<Index, Types...>;
};

template <size_t Index, typename... Types> struct variant_alternative<Index, variant<Types...> const> {
  using type = std::add_const_t<details::type_alternative_t<Index, Types...>>;
};

template <size_t Index, typename... Types> struct variant_alternative<Index, variant<Types...> volatile> {
  using type = std::add_volatile_t<details::type_alternative_t<Index, Types...>>;
};

template <size_t Index, typename... Types> struct variant_alternative<Index, variant<Types...> const volatile> {
  using type = std::add_cv_t<details::type_alternative_t<Index, Types...>>;
};

template <size_t Index, typename Variant>
using variant_alternative_t = typename variant_alternative<Index, Variant>::type;

template <typename T, typename... Types> constexpr bool holds_alternative(variant<Types...> const &v) noexcept {
  return (v.index() == details::type_index_v<T, Types...>);
}

template <size_t Index, typename... Types>
constexpr variant_alternative_t<Index, variant<Types...>> &get(variant<Types...> &v) {
  if (v.index() != Index || Index == variant_npos) {
    throw bad_variant_access();
  }
  return details::get<Index>(v.storage);
}

template <size_t Index, typename... Types>
constexpr variant_alternative_t<Index, variant<Types...>> const &get(variant<Types...> const &v) {
  if (v.index() != Index || Index == variant_npos) {
    throw bad_variant_access();
  }
  return details::get<Index>(v.storage);
}

template <size_t Index, typename... Types>
constexpr variant_alternative_t<Index, variant<Types...>> &&get(variant<Types...> &&v) {
  if (v.index() != Index || Index == variant_npos) {
    throw bad_variant_access();
  }
  return details::get<Index>(std::move(v.storage));
}

template <size_t Index, typename... Types>
constexpr variant_alternative_t<Index, variant<Types...>> const &&get(variant<Types...> const &&v) {
  if (v.index() != Index || Index == variant_npos) {
    throw bad_variant_access();
  }
  static_assert(std::is_same_v<decltype(details::get<Index>(std::move(v.storage))),
                               variant_alternative_t<Index, variant<Types...>> const &&>);
  return details::get<Index>(std::move(v.storage));
}

template <typename Required, typename... Types> constexpr Required &get(variant<Types...> &v) {
  constexpr size_t Index = details::type_index_v<Required, Types...>;
  return get<Index>(v);
}

template <typename Required, typename... Types> constexpr Required const &get(variant<Types...> const &v) {
  constexpr size_t Index = details::type_index_v<Required, Types...>;
  return get<Index>(v);
}

template <typename Required, typename... Types> constexpr Required &&get(variant<Types...> &&v) {
  constexpr size_t Index = details::type_index_v<Required, Types...>;
  return get<Index>(std::move(v));
}

template <typename Required, typename... Types> constexpr Required const &&get(variant<Types...> const &&v) {
  constexpr size_t Index = details::type_index_v<Required, Types...>;
  return get<Index>(std::move(v));
}

template <size_t Index, typename... Types>
constexpr std::add_pointer_t<details::type_alternative_t<Index, Types...>> get_if(variant<Types...> *pv) noexcept {
  if (pv->index() != Index) {
    return nullptr;
  }
  return std::addressof(get<Index>(*pv));
}

template <size_t Index, typename... Types>
constexpr std::add_pointer_t<details::type_alternative_t<Index, Types...> const>
get_if(variant<Types...> const *pv) noexcept {
  if (pv->index() != Index) {
    return nullptr;
  }
  return std::addressof(get<Index>(*pv));
}

template <typename Type, typename... Types> constexpr std::add_pointer_t<Type> get_if(variant<Types...> *pv) noexcept {
  if (!holds_alternative<Type>(*pv)) {
    return nullptr;
  }
  return std::addressof(get<Type>(*pv));
}

template <typename Type, typename... Types>
constexpr std::add_pointer_t<Type const> get_if(variant<Types...> const *pv) noexcept {
  if (!holds_alternative<Type>(*pv)) {
    return nullptr;
  }
  return std::addressof(get<Type>(*pv));
}
