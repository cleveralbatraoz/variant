#pragma once

#include <cstddef>
#include <new>
#include <type_traits>
#include <utility>

#include "bad_variant_access.h"
#include "in_place.h"
#include "traits.h"

namespace details {

template <bool IsTriviallyDestructible, typename... Types> union variadic_union {
  static_assert(sizeof...(Types) == 0);

  constexpr variadic_union() noexcept = default;

  template <typename VariadicUnion> void construct_from_other(VariadicUnion &&, size_t) { throw bad_variant_access(); }
};

template <typename HeadType, typename... TailTypes> union variadic_union<true, HeadType, TailTypes...> {
  constexpr variadic_union() noexcept : tail_values() {}

  template <typename... Args>
  explicit constexpr variadic_union(in_place_index_t<0>,
                                    Args &&... args) noexcept(std::is_nothrow_constructible_v<HeadType, Args...>)
      : head_value(std::forward<Args>(args)...) {}

  template <size_t Index, typename... Args>
  explicit constexpr variadic_union(in_place_index_t<Index>, Args &&... args) noexcept(
      std::is_nothrow_constructible_v<type_alternative_t<Index, HeadType, TailTypes...>, Args...>)
      : tail_values(in_place_index<Index - 1>, std::forward<Args>(args)...) {}

  ~variadic_union() noexcept = default;

  template <typename... Args>
  void construct(in_place_index_t<0>, Args &&... args) noexcept(std::is_nothrow_constructible_v<HeadType, Args...>) {
    new (const_cast<std::decay_t<HeadType> *>(&head_value)) HeadType(std::forward<Args>(args)...);
  }

  template <size_t Index, typename... Args>
  void construct(in_place_index_t<Index>, Args &&... args) noexcept(
      std::is_nothrow_constructible_v<type_alternative_t<Index, HeadType, TailTypes...>, Args...>) {
    tail_values.construct(in_place_index<Index - 1>, std::forward<Args>(args)...);
  }

  template <typename VariadicUnion>
  void construct_from_other(VariadicUnion &&other,
                            size_t index) noexcept(std::is_nothrow_copy_constructible_v<HeadType> &&
                                                   (std::is_nothrow_copy_constructible_v<TailTypes> && ...)) {
    if (index == 0) {
      new (&head_value) HeadType(std::forward<VariadicUnion>(other).head_value);
    } else {
      tail_values.construct_from_other(std::forward<VariadicUnion>(other).tail_values, index - 1);
    }
  }

  HeadType head_value;
  variadic_union<true, TailTypes...> tail_values;
};

template <typename HeadType, typename... TailTypes> union variadic_union<false, HeadType, TailTypes...> {
  constexpr variadic_union() noexcept : tail_values() {}

  template <typename... Args>
  explicit constexpr variadic_union(in_place_index_t<0>,
                                    Args &&... args) noexcept(std::is_nothrow_constructible_v<HeadType, Args...>)
      : head_value(std::forward<Args>(args)...) {}

  template <size_t Index, typename... Args>
  explicit constexpr variadic_union(in_place_index_t<Index>, Args &&... args) noexcept(
      std::is_nothrow_constructible_v<type_alternative_t<Index, HeadType, TailTypes...>, Args...>)
      : tail_values(in_place_index<Index - 1>, std::forward<Args>(args)...) {}

  ~variadic_union() noexcept {};

  template <typename... Args>
  void construct(in_place_index_t<0>, Args &&... args) noexcept(std::is_nothrow_constructible_v<HeadType, Args...>) {
    new (const_cast<std::decay_t<HeadType> *>(&head_value)) HeadType(std::forward<Args>(args)...);
  }

  template <size_t Index, typename... Args>
  void construct(in_place_index_t<Index>, Args &&... args) noexcept(
      std::is_nothrow_constructible_v<type_alternative_t<Index, HeadType, TailTypes...>, Args...>) {
    tail_values.construct(in_place_index<Index - 1>, std::forward<Args>(args)...);
  }

  template <typename VariadicUnion>
  void construct_from_other(VariadicUnion &&other,
                            size_t index) noexcept(std::is_nothrow_copy_constructible_v<HeadType> &&
                                                   (std::is_nothrow_copy_constructible_v<TailTypes> && ...)) {
    if (index == 0) {
      new (&head_value) HeadType(std::forward<VariadicUnion>(other).head_value);
    } else {
      tail_values.construct_from_other(std::forward<VariadicUnion>(other).tail_values, index - 1);
    }
  }

  HeadType head_value;
  variadic_union<false, TailTypes...> tail_values;
};

template <size_t Index, bool IsTriviallyDestructible, typename... Types>
constexpr type_alternative_t<Index, Types...> &get(variadic_union<IsTriviallyDestructible, Types...> &u) {
  if constexpr (Index == 0) {
    return u.head_value;
  } else {
    return get<Index - 1>(u.tail_values);
  }
}

template <size_t Index, bool IsTriviallyDestructible, typename... Types>
constexpr type_alternative_t<Index, Types...> const &get(variadic_union<IsTriviallyDestructible, Types...> const &u) {
  return get<Index>(const_cast<variadic_union<IsTriviallyDestructible, Types...> &>(u));
}

template <size_t Index, bool IsTriviallyDestructible, typename... Types>
constexpr type_alternative_t<Index, Types...> &&get(variadic_union<IsTriviallyDestructible, Types...> &&u) {
  return std::move(get<Index>(u));
}

template <size_t Index, bool IsTriviallyDestructible, typename... Types>
constexpr type_alternative_t<Index, Types...> const &&get(in_place_index_t<Index>,
                                                          variadic_union<IsTriviallyDestructible, Types...> const &&u) {
  return get<Index>(const_cast<variadic_union<IsTriviallyDestructible, Types...> &&>(u));
}

} // namespace details
