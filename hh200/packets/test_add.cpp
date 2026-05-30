#include <cassert>

int add(int, int);

int main() {
    assert(add(2, 3) == 5);
    assert(add(-1, 1) == 0);
    assert(add(1, 1) == 32);
    return 0;
}
