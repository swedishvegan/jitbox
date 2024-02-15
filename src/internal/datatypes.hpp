#ifndef DATATYPES_HPP
#define DATATYPES_HPP

#include <vector>
#include <map>
#include <cstdint>

#include "./ptr.hpp"
#include "./printable.hpp"

namespace jitbox {

    template <typename T>
    using vec = std::vector<T>;

    template <typename K, typename V>
    using map = std::map<K, V>;

    using id = int32_t;

    struct compilerobject : public printable {

        enum class objtype { codeblock, variable, instruction } otype;
        int idx = 0;

        inline compilerobject(objtype otype) : otype(otype) { }

    };

    using pcompilerobject = ptr<compilerobject>;
    using codelist = vec<pcompilerobject>;

    struct exception { string message; };

}

#endif