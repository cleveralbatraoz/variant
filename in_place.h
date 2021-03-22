#pragma once

#include <cstddef>

struct in_place_t {
  explicit constexpr in_place_t() noexcept = default;
};

inline constexpr in_place_t in_place{};

template <typename T> struct in_place_type_t { explicit constexpr in_place_type_t() noexcept = default; };

template <typename T> inline constexpr in_place_type_t<T> in_place_type{};

template <size_t Index> struct in_place_index_t { explicit constexpr in_place_index_t() noexcept = default; };

template <size_t Index> inline constexpr in_place_index_t<Index> in_place_index{};
