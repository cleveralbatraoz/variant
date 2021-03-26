#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>

#include "in_place.h"
#include "variadic_union.h"
#include "visit.h"

namespace details {

template <bool IsTriviallyDestructible, typename HeadType, typename... TailTypes> struct indexed_storage {
  constexpr indexed_storage() noexcept : current_value_index(variant_npos) {}

  template <size_t Index, typename... Args>
  explicit constexpr indexed_storage(in_place_index_t<Index>, Args &&...args) noexcept(
      std::is_nothrow_constructible_v<type_alternative_t<Index, HeadType, TailTypes...>, Args...>)
      : current_value_index(variant_npos), storage(in_place_index<Index>, std::forward<Args>(args)...) {
    current_value_index = Index;
  }

  ~indexed_storage() noexcept = default;

  void destroy() noexcept {}

  size_t current_value_index;
  variadic_union<true, HeadType, TailTypes...> storage;
};

template <typename HeadType, typename... TailTypes> struct indexed_storage<false, HeadType, TailTypes...> {
  constexpr indexed_storage() noexcept : current_value_index(variant_npos) {}

  template <size_t Index, typename... Args>
  explicit constexpr indexed_storage(in_place_index_t<Index>, Args &&...args) noexcept(
      std::is_nothrow_constructible_v<type_alternative_t<Index, HeadType, TailTypes...>, Args...>)
      : current_value_index(variant_npos), storage(in_place_index<Index>, std::forward<Args>(args)...) {
    current_value_index = Index;
  }

  ~indexed_storage() noexcept {
    if (current_value_index != variant_npos) {
      destroy();
    }
  }

  void destroy() noexcept {
    visit(
        [](auto &value) {
          using Type = std::decay_t<decltype(value)>;
          value.~Type();
        },
        storage, current_value_index);
  }

  size_t current_value_index;
  variadic_union<false, HeadType, TailTypes...> storage;
};

template <typename... Types>
using indexed_storage_base = indexed_storage<(std::is_trivially_destructible_v<Types> && ...), Types...>;

template <bool IsTriviallyCopyConstructible, typename... Types>
struct copy_constructor : indexed_storage_base<Types...> {
  using base = indexed_storage_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  copy_constructor() = default;

  copy_constructor(copy_constructor const &) = default;
};

template <typename... Types> struct copy_constructor<false, Types...> : indexed_storage_base<Types...> {
  using base = indexed_storage_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  copy_constructor() = default;

  copy_constructor(copy_constructor const &other) noexcept((std::is_nothrow_copy_constructible_v<Types> && ...)) {
    current_value_index = variant_npos;
    if (other.current_value_index != variant_npos) {
      storage.construct_from_other(other.storage, other.current_value_index);
      current_value_index = other.current_value_index;
    }
  }
};

template <typename... Types>
using copy_constructor_base = copy_constructor<(std::is_trivially_copy_constructible_v<Types> && ...), Types...>;

template <bool IsTriviallyMoveConstructible, typename... Types>
struct move_constructor : copy_constructor_base<Types...> {
  using base = copy_constructor_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  move_constructor() = default;

  move_constructor(move_constructor const &) = default;

  move_constructor(move_constructor &&) = default;

  move_constructor &operator=(move_constructor const &) = default;
};

template <typename... Types> struct move_constructor<false, Types...> : copy_constructor_base<Types...> {
  using base = copy_constructor_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  move_constructor() = default;

  move_constructor(move_constructor const &) = default;

  move_constructor(move_constructor &&other) noexcept((std::is_nothrow_move_constructible_v<Types> && ...)) {
    current_value_index = variant_npos;
    if (other.current_value_index != variant_npos) {
      storage.construct_from_other(std::move(other.storage), other.current_value_index);
      current_value_index = other.current_value_index;
    }
  }

  move_constructor &operator=(move_constructor const &) = default;
};

template <typename... Types>
using move_constructor_base = move_constructor<(std::is_trivially_move_constructible_v<Types> && ...), Types...>;

template <bool IsTriviallyMoveAssignable, typename... Types> struct move_assignment : move_constructor_base<Types...> {
  using base = move_constructor_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  move_assignment() = default;

  move_assignment(move_assignment const &) = default;

  move_assignment(move_assignment &&) = default;

  move_assignment &operator=(move_assignment const &) = default;

  move_assignment &operator=(move_assignment &&) = default;
};

template <typename... Types> struct move_assignment<false, Types...> : move_constructor_base<Types...> {
  using base = move_constructor_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  move_assignment() = default;

  move_assignment(move_assignment const &) = default;

  move_assignment(move_assignment &&) = default;

  move_assignment &operator=(move_assignment const &) = default;

  constexpr move_assignment &
  operator=(move_assignment &&other) noexcept((std::is_nothrow_move_constructible_v<Types> && ...) &&
                                              (std::is_nothrow_move_assignable_v<Types> && ...)) {
    if (current_value_index == variant_npos && other.current_value_index == variant_npos) {
    } else if (other.current_value_index == variant_npos) {
      this->destroy();
      current_value_index = variant_npos;
    } else if (current_value_index == other.current_value_index) {
      details::visit(
          [&other](auto &value) {
            details::visit(
                [&value](auto &other_value) {
                  using Type = decltype(value);
                  using OtherType = decltype(other_value);
                  if constexpr (std::is_same_v<std::decay_t<Type>, std::decay_t<OtherType>>) {
                    value = std::move(other_value);
                  }
                },
                other.storage, other.current_value_index);
          },
          storage, current_value_index);
    } else {
      this->destroy();
      current_value_index = variant_npos;
      storage.construct_from_other(std::move(other.storage), other.current_value_index);
      current_value_index = other.current_value_index;
    }
    return *this;
  }
};

template <typename... Types>
using move_assignment_base = move_assignment<(std::is_trivially_move_constructible_v<Types> && ...) &&
                                                 (std::is_trivially_move_assignable_v<Types> && ...),
                                             Types...>;

template <bool IsTriviallyCopyAssignable, typename... Types> struct copy_assignment : move_assignment_base<Types...> {
  using base = move_assignment_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  copy_assignment() = default;

  copy_assignment(copy_assignment const &) = default;

  copy_assignment(copy_assignment &&) = default;

  copy_assignment &operator=(copy_assignment const &) = default;

  copy_assignment &operator=(copy_assignment &&) = default;
};

template <typename... Types> struct copy_assignment<false, Types...> : move_assignment_base<Types...> {
  using base = move_assignment_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  copy_assignment() = default;

  copy_assignment(copy_assignment const &) = default;

  copy_assignment(copy_assignment &&) = default;

  constexpr copy_assignment &
  operator=(copy_assignment const &other) noexcept((std::is_nothrow_copy_constructible_v<Types> && ...) &&
                                                   (std::is_nothrow_copy_assignable_v<Types> && ...)) {
    if (current_value_index == variant_npos && other.current_value_index == variant_npos) {
    } else if (other.current_value_index == variant_npos) {
      this->destroy();
      current_value_index = variant_npos;
    } else if (current_value_index == other.current_value_index) {
      details::visit(
          [other](auto &value) {
            details::visit(
                [&value](auto &other_value) {
                  using Type = decltype(value);
                  using OtherType = decltype(other_value);
                  if constexpr (std::is_same_v<std::decay_t<Type>, std::decay_t<OtherType>>) {
                    value = other_value;
                  }
                },
                other.storage, other.current_value_index);
          },
          storage, current_value_index);
    } else {
      details::visit(
          [other, this](auto &value) {
            details::visit(
                [this, other](auto &other_value) {
                  using OtherType = decltype(other_value);
                  if constexpr (std::is_nothrow_copy_constructible_v<OtherType> ||
                                !std::is_nothrow_move_constructible_v<OtherType>) {
                    this->destroy();
                    current_value_index = variant_npos;
                    storage.construct_from_other(other.storage, other.current_value_index);
                    current_value_index = other.current_value_index;
                  } else {
                    this->operator=(copy_assignment(other));
                  }
                },
                other.storage, other.current_value_index);
          },
          storage, current_value_index);
    }
    return *this;
  }

  copy_assignment &operator=(copy_assignment &&) = default;
};

template <typename... Types>
using copy_assignment_base = copy_assignment<(std::is_trivially_copy_constructible_v<Types> && ...) &&
                                                 (std::is_trivially_copy_assignable_v<Types> && ...),
                                             Types...>;

template <typename... Types> struct constructors_implementation : copy_assignment_base<Types...> {
  using base = copy_assignment_base<Types...>;
  using base::base;
  using base::current_value_index;
  using base::storage;

  constexpr constructors_implementation() noexcept(
      std::is_nothrow_default_constructible_v<type_alternative_t<0, Types...>>)
      : base(in_place_index<0>) {}

  constructors_implementation(constructors_implementation const &) = default;

  constructors_implementation(constructors_implementation &&) = default;

  constructors_implementation &operator=(constructors_implementation const &) = default;

  constructors_implementation &operator=(constructors_implementation &&) = default;
};

struct dummy {};

template <bool IsDefaultConstructible> struct conditional_default_constructible {
  constexpr conditional_default_constructible(dummy) noexcept {}

  conditional_default_constructible() = default;

  conditional_default_constructible(conditional_default_constructible const &) = default;

  conditional_default_constructible(conditional_default_constructible &&) = default;

  conditional_default_constructible &operator=(conditional_default_constructible const &) = default;

  conditional_default_constructible &operator=(conditional_default_constructible &&) = default;
};

template <> struct conditional_default_constructible<false> {
  constexpr conditional_default_constructible(dummy) noexcept {}

  conditional_default_constructible() = delete;

  conditional_default_constructible(conditional_default_constructible const &) = default;

  conditional_default_constructible(conditional_default_constructible &&) = default;

  conditional_default_constructible &operator=(conditional_default_constructible const &) = default;

  conditional_default_constructible &operator=(conditional_default_constructible &&) = default;
};

template <typename... Types>
using conditional_default_constructible_base =
    conditional_default_constructible<std::is_default_constructible_v<type_alternative_t<0, Types...>>>;

template <bool IsCopyConstructible> struct conditional_copy_constructible {
  constexpr conditional_copy_constructible(dummy) noexcept {}

  conditional_copy_constructible() = default;

  conditional_copy_constructible(conditional_copy_constructible const &) = default;

  conditional_copy_constructible(conditional_copy_constructible &&) = default;

  conditional_copy_constructible &operator=(conditional_copy_constructible const &) = default;

  conditional_copy_constructible &operator=(conditional_copy_constructible &&) = default;
};

template <> struct conditional_copy_constructible<false> {
  constexpr conditional_copy_constructible(dummy) noexcept {}

  conditional_copy_constructible() = default;

  conditional_copy_constructible(conditional_copy_constructible const &) = delete;

  conditional_copy_constructible(conditional_copy_constructible &&) = default;

  conditional_copy_constructible &operator=(conditional_copy_constructible const &) = default;

  conditional_copy_constructible &operator=(conditional_copy_constructible &&) = default;
};

template <typename... Types>
using conditional_copy_constructible_base =
    conditional_copy_constructible<(std::is_copy_constructible_v<Types> && ...)>;

template <bool IsMoveConstructible> struct conditional_move_constructible {
  constexpr conditional_move_constructible(dummy) noexcept {}

  conditional_move_constructible() = default;

  conditional_move_constructible(conditional_move_constructible const &) = default;

  conditional_move_constructible(conditional_move_constructible &&) = default;

  conditional_move_constructible &operator=(conditional_move_constructible const &) = default;

  conditional_move_constructible &operator=(conditional_move_constructible &&) = default;
};

template <> struct conditional_move_constructible<false> {
  constexpr conditional_move_constructible(dummy) noexcept {}

  conditional_move_constructible() = default;

  conditional_move_constructible(conditional_move_constructible const &) = default;

  conditional_move_constructible(conditional_move_constructible &&) = delete;

  conditional_move_constructible &operator=(conditional_move_constructible const &) = default;

  conditional_move_constructible &operator=(conditional_move_constructible &&) = default;
};

template <typename... Types>
using conditional_move_constructible_base =
    conditional_move_constructible<(std::is_move_constructible_v<Types> && ...)>;

template <bool IsCopyAssignable> struct conditional_copy_assignable {
  constexpr conditional_copy_assignable(dummy) noexcept {}

  conditional_copy_assignable() = default;

  conditional_copy_assignable(conditional_copy_assignable const &) = default;

  conditional_copy_assignable(conditional_copy_assignable &&) = default;

  conditional_copy_assignable &operator=(conditional_copy_assignable const &) = default;

  conditional_copy_assignable &operator=(conditional_copy_assignable &&) = default;
};

template <> struct conditional_copy_assignable<false> {
  constexpr conditional_copy_assignable(dummy) noexcept {}

  conditional_copy_assignable() = default;

  conditional_copy_assignable(conditional_copy_assignable const &) = default;

  conditional_copy_assignable(conditional_copy_assignable &&) = default;

  conditional_copy_assignable &operator=(conditional_copy_assignable const &) = delete;

  conditional_copy_assignable &operator=(conditional_copy_assignable &&) = default;
};

template <typename... Types>
using conditional_copy_assignable_base = conditional_copy_assignable<(std::is_copy_constructible_v<Types> && ...) &&
                                                                     (std::is_copy_assignable_v<Types> && ...)>;

template <bool ismoveassignable> struct conditional_move_assignable {
  constexpr conditional_move_assignable(dummy) noexcept {}

  conditional_move_assignable() = default;

  conditional_move_assignable(conditional_move_assignable const &) = default;

  conditional_move_assignable(conditional_move_assignable &&) = default;

  conditional_move_assignable &operator=(conditional_move_assignable const &) = default;

  conditional_move_assignable &operator=(conditional_move_assignable &&) = default;
};

template <> struct conditional_move_assignable<false> {
  constexpr conditional_move_assignable(dummy) noexcept {}

  conditional_move_assignable() = default;

  conditional_move_assignable(conditional_move_assignable const &) = default;

  conditional_move_assignable(conditional_move_assignable &&) = default;

  conditional_move_assignable &operator=(conditional_move_assignable const &) = default;

  conditional_move_assignable &operator=(conditional_move_assignable &&) = delete;
};

template <typename... Types>
using conditional_move_assignable_base = conditional_move_assignable<(std::is_move_constructible_v<Types> && ...) &&
                                                                     (std::is_move_assignable_v<Types> && ...)>;

template <typename... Types>
struct conditional_constructors : conditional_default_constructible_base<Types...>,
                                  conditional_copy_constructible_base<Types...>,
                                  conditional_move_constructible_base<Types...>,
                                  conditional_copy_assignable_base<Types...>,
                                  conditional_move_assignable_base<Types...> {
  using base1 = conditional_default_constructible_base<Types...>;
  using base2 = conditional_copy_constructible_base<Types...>;
  using base3 = conditional_move_constructible_base<Types...>;
  using base4 = conditional_copy_assignable_base<Types...>;
  using base5 = conditional_move_assignable_base<Types...>;

  constexpr conditional_constructors(dummy) noexcept
      : base1(dummy{}), base2(dummy{}), base3(dummy{}), base4(dummy{}), base5(dummy{}) {}

  conditional_constructors() = default;

  conditional_constructors(conditional_constructors const &) = default;

  conditional_constructors(conditional_constructors &&) = default;

  conditional_constructors &operator=(conditional_constructors const &) = default;

  conditional_constructors &operator=(conditional_constructors &&) = default;
};

} // namespace details
