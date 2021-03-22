#pragma once

#include <stdexcept>

struct bad_variant_access : std::runtime_error {
  using base = std::runtime_error;
  using base::base;
  using base::operator=;

  bad_variant_access() : base("Bad variant access") {}
};
