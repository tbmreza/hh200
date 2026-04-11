#pragma once

#include <string>
#include <memory>
#include <string_view>

namespace hhs::fmt {

/**
 * @brief A simple Rope data structure skeleton.
 */
class Rope {
public:
    Rope() = default;
    explicit Rope(std::string_view str);

    [[nodiscard]] std::string str() const;
    [[nodiscard]] size_t size() const;

private:
    std::string data_; // Initial simple implementation
};

} // namespace hhs::fmt
