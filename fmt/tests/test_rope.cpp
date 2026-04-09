#include <catch2/catch_test_macros.hpp>
#include "hhs/fmt/rope.hpp"

TEST_CASE("Rope basic functionality", "[rope]") {
    SECTION("Empty rope") {
        hhs::fmt::Rope r;
        CHECK(r.size() == 0);
        CHECK(r.str() == "");
    }

    SECTION("Rope from string_view") {
        hhs::fmt::Rope r("Hello, C++26!");
        CHECK(r.size() == 13);
        CHECK(r.str() == "Hello, C++26!");
    }
}
