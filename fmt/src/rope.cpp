#include "hhs/fmt/rope.hpp"

namespace hhs::fmt {

Rope::Rope(std::string_view str) : data_(str) {}

std::string Rope::str() const {
    return data_;
}

size_t Rope::size() const {
    return data_.size();
}

} // namespace hhs::fmt
