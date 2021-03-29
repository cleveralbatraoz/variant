#pragma once

#include <algorithm>
#include <cstddef>
#include <type_traits>
#include <utility>

#include "bad_variant_access.h"
#include "bases.h"
#include "in_place.h"
#include "non_member.h"
#include "traits.h"
#include "visit.h"

template <typename... Types>
class variant : details::constructors_implementation<Types...>, details::conditional_constructors<Types...> {
  using functionality_base = details::constructors_implementation<Types...>;
  using conditional_base = details::conditional_constructors<Types...>;

public:
  variant() = default;

  variant(variant const &) = default;

  variant(variant &&) = default;

  template <size_t Index, typename... Args, std::enable_if_t<(Index < sizeof...(Types)), int> = 0,
            std::enable_if_t<(std::is_constructible_v<details::type_alternative_t<Index, Types...>, Args...>), int> = 0>
  explicit constexpr variant(in_place_index_t<Index>, Args &&...args) noexcept(
      std::is_nothrow_constructible_v<details::type_alternative_t<Index, Types...>, Types...>)
      : functionality_base(in_place_index<Index>, std::forward<Args>(args)...), conditional_base(details::dummy{}) {}

  template <typename T, typename... Args, std::enable_if_t<(details::is_unique_type_v<T, Types...>), int> = 0,
            std::enable_if_t<(std::is_constructible_v<T, Args...>), int> = 0>
  explicit constexpr variant(in_place_type_t<T>, Args &&...args) noexcept(std::is_nothrow_constructible_v<T, Args...>)
      : variant(in_place_index<details::type_index_v<T, Types...>>, std::forward<Args>(args)...) {}

  template <typename T, typename Decayed = std::decay_t<T>,
            std::enable_if_t<!details::is_specialization_v<Decayed, in_place_type_t>, int> = 0,
            std::enable_if_t<!details::is_in_place_index_specialization_v<Decayed>, int> = 0,
            std::enable_if_t<!std::is_same_v<Decayed, variant>, int> = 0,
            typename MatchingType = details::type_matcher_t<T, variant>,
            std::enable_if_t<details::is_unique_type_v<MatchingType, Types...>, int> = 0,
            std::enable_if_t<std::is_constructible_v<MatchingType, T>, int> = 0>
  constexpr variant(T &&value) noexcept(std::is_nothrow_constructible_v<MatchingType, T>)
      : variant(in_place_type<MatchingType>, std::forward<T>(value)) {}

  template <typename T, typename Decayed = std::decay_t<T>,
            std::enable_if_t<!std::is_same_v<Decayed, variant>, int> = 0,
            typename MatchingType = details::type_matcher_t<T, variant>,
            std::enable_if_t<details::is_unique_type_v<MatchingType, Types...>, int> = 0,
            std::enable_if_t<std::is_constructible_v<MatchingType, T>, int> = 0,
            std::enable_if_t<std::is_assignable_v<MatchingType &, T>, int> = 0>
  variant &operator=(T &&value) noexcept(
      std::is_nothrow_constructible_v<MatchingType, T> &&std::is_nothrow_assignable_v<MatchingType &, T>) {
    constexpr size_t Index = details::type_index_v<MatchingType, Types...>;
    if (this->current_value_index == Index) {
      get<Index>(*this) = std::forward<T>(value);
    } else {
      if (std::is_nothrow_constructible_v<MatchingType, T> || !std::is_nothrow_move_constructible_v<MatchingType>) {
        this->template emplace<MatchingType>(std::forward<T>(value));
      } else {
        this->operator=(variant(std::forward<T>(value)));
      }
    }
    return *this;
  }

  ~variant() = default;

  variant &operator=(variant const &) = default;

  variant &operator=(variant &&) = default;

  template <typename T, typename... Args, std::enable_if_t<details::is_unique_type_v<T, Types...>, int> = 0,
            std::enable_if_t<std::is_constructible_v<T, Args...>, int> = 0>
  T &emplace(Args &&...args) {
    constexpr size_t Index = details::type_index_v<T, Types...>;
    return emplace<Index>(std::forward<Args>(args)...);
  }

  template <size_t Index, typename... Args,
            std::enable_if_t<std::is_constructible_v<details::type_alternative_t<Index, Types...>, Args...>, int> = 0>
  details::type_alternative_t<Index, Types...> &emplace(Args &&...args) {
    this->destroy();
    this->storage.construct(in_place_index<Index>, std::forward<Args>(args)...);
    this->current_value_index = Index;
    return get<Index>(*this);
  }

  constexpr size_t index() const noexcept { return this->current_value_index; }

  constexpr bool valueless_by_exception() const noexcept { return (this->current_value_index == variant_npos); }

  void swap(variant &other) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                       std::is_nothrow_swappable_v<Types>)&&...)) {
    if (this->current_value_index == variant_npos) {
      if (other.current_value_index != variant_npos) {
        this->storage.construct_from_other(std::move(other.storage), other.current_value_index);
	this->current_value_index = other.current_value_index;
        other.destroy();
      }
    } else if (other.current_value_index == variant_npos) {
      other.storage.construct_from_other(std::move(this->storage), this->current_value_index);
      other.current_value_index = this->current_value_index;
      this->destroy();
    } else if (this->current_value_index == other.current_value_index) {
      visit(
          [](auto &x, auto &y) {
            using Type1 = std::decay_t<decltype(x)>;
            using Type2 = std::decay_t<decltype(y)>;
            if constexpr (std::is_same_v<Type1, Type2>) {
              using std::swap;
              swap(x, y);
            }
          },
          *this, other);
      std::swap(this->current_value_index, other.current_value_index);
    } else {
      auto copy = std::move(other);
      other = std::move(*this);
      *this = std::move(copy);
      return;
    }
  }

  template <size_t Index, typename... Ts>
  friend constexpr variant_alternative_t<Index, variant<Ts...>> &get(variant<Ts...> &);

  template <size_t Index, typename... Ts>
  friend constexpr variant_alternative_t<Index, variant<Ts...>> const &get(variant<Ts...> const &);

  template <size_t Index, typename... Ts>
  friend constexpr variant_alternative_t<Index, variant<Ts...>> &&get(variant<Ts...> &&);

  template <size_t Index, typename... Ts>
  friend constexpr variant_alternative_t<Index, variant<Ts...>> const &&get(variant<Ts...> const &&);
};
