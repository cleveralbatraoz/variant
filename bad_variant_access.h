#pragma once

#include <exception>

struct bad_variant_access final : std::exception {
  char const *what() const noexcept override { return "Bad variant access."; }
};
