#ifndef INSTRUCTIONSET_HPP
#define INSTRUCTIONSET_HPP

#include "./datatypes.hpp"

// This file defines the instruction set used by the VM

namespace jitbox {

    

    inline bool ishint(inst in) { return (int)in <= (int)inst::rfree; }

    extern const uint32_t instlengths[];
    extern const char* instnames[];

}

#endif