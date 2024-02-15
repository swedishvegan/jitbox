#ifndef INSTRUCTIONSET_HPP
#define INSTRUCTIONSET_HPP

#include "./datatypes.hpp"

// This file defines the instruction set used by the VM

namespace jitbox {

    enum class inst : uint32_t {
        
        begfnc,  // Arguments: 1, Purpose: marks the beginning of a function
        endfnc,  // Arguments: 0, Purpose: marks the end of a function 
        alias,   // Arguments: 2, Purpose: marks arg1 as the owner of arg2, so that changes in arg2 are reflected in arg1
        farg,    // Arguments: 1, Purpose: marks arg1 as an argument in a function call
        regarg,  // Arguments: 1, Purpose: tells the JIT that arg1 is an argument and does not need to be allocated because the caller already allocated it (its runtime address is inferred by the order that these instructions are generated)
        rhint,   // Arguments: 2, Purpose: hints to the JIT to try to allocate arg1 in argument register arg2
        hot,     // Arguments: 1, Purpose: hints to the JIT that arg1 should be placed in a register
        beglp,   // Arguments: 0, Purpose: marks the beginning of loop mode
        endlp,   // Arguments: 0, Purpose: marks the end of loop mode
        rfree,   // Arguments: 1, Purpose: tells the JIT that arg1 is no longer in use and its address can be used by another live variable
        exit,    // Arguments: 0, Purpose: terminates the program with code 0
        j,       // Arguments: 1, Purpose: sets the IP to arg1
        jz,      // Arguments: 2, Purpose: sets the IP to arg1 iff arg2 == 0
        jnz,     // Arguments: 2, Purpose: sets the IP to arg1 iff arg2 != 0
        je,      // Arguments: 3, Purpose: sets the IP to arg1 iff arg2 == arg3
        jne,     // Arguments: 3, Purpose: sets the IP to arg1 iff arg2 != arg3
        jg,      // Arguments: 3, Purpose: sets the IP to arg1 iff arg2 > arg3
        jge,     // Arguments: 3, Purpose: sets the IP to arg1 iff arg2 >= arg3
        callv,   // Arguments: 1, Purpose: calls the function arg1
        call,    // Arguments: 2, Purpose: calls the function arg1 and stores the result in arg2
        retv,    // Arguments: 0, Purpose: sets the IP to the value on top of the stack
        ret,     // Arguments: 1, Purpose: moves arg1 into the return register and sets the IP to the value on top of the stack
        halloc,  // Arguments: 2, Purpose: allocates arg1 bytes on the heap and sets arg2 to the new allocated address
        hinit,   // Arguments: 2, Purpose: allocates arg1 bytes on the heap, zeros the newly allocated memory, and sets arg2 to the new allocated address
        copy,    // Arguments: 3, Purpose: copies arg1 bytes from arg2 into arg3 
        mov,     // Arguments: 2, Purpose: moves the value of arg1 into arg2
        movz,    // Arguments: 4, Purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 == 0
        movnz,   // Arguments: 4, Purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 != 0
        move,    // Arguments: 5, Purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 == arg5
        movne,   // Arguments: 5, Purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 != arg5
        movg,    // Arguments: 5, Purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 > arg5
        movge,   // Arguments: 5, Purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 >= arg5
        ref,     // Arguments: 2, Purpose: moves the address of arg1 into arg2
        deref,   // Arguments: 2, Purpose: moves the value in the address referenced by arg1 into arg2
        objlen,  // Arguments: 2, Purpose: moves the byte size of the heap object at the address referenced by arg1 into arg2
        cast,    // Arguments: 2, Purpose: casts arg1 to a new type and stores the result in arg2
        add,     // Arguments: 3, Purpose: stores the sum of arg1 and arg2 in arg3
        sub,     // Arguments: 3, Purpose: stores the difference of arg1 and arg2 in arg3
        mul,     // Arguments: 3, Purpose: stores the product of arg1 and arg2 in arg3
        div,     // Arguments: 3, Purpose: stores the quotient of arg1 and arg2 in arg3
        mod,     // Arguments: 3, Purpose: stores the remainder of dividing arg1 by arg2 in arg3
        band,    // Arguments: 3, Purpose: stores the bitwise AND of arg1 and arg2 in arg3
        bor,     // Arguments: 3, Purpose: stores the bitwise OR of arg1 and arg2 in arg3
        bxor,    // Arguments: 3, Purpose: stores the bitwise XOR of arg1 and arg2 in arg3
        bnot,    // Arguments: 2, Purpose: stores the bitwise NOT of arg1 in arg2
        zero,    // Arguments: 2, Purpose: stores the boolean value arg1 == 0 in arg2
        nzero,   // Arguments: 2, Purpose: stores the boolean value arg1 != 0 in arg2
        eq,      // Arguments: 3, Purpose: stores the boolean value arg1 == arg2 in arg3
        neq,     // Arguments: 3, Purpose: stores the boolean value arg1 != arg2 in arg3
        gt,      // Arguments: 3, Purpose: stores the boolean value arg1 > arg2 in arg3
        gte,     // Arguments: 3, Purpose: stores the boolean value arg1 >= arg2 in arg3
        shl,     // Arguments: 3, Purpose: stores the bitwise left-shift of arg1 by arg2 in arg3
        shr,     // Arguments: 3, Purpose: stores the bitwise right-shift of arg1 by arg2 in arg3
        rotl,    // Arguments: 3, Purpose: stores the bitwise left-rotation of arg1 by arg2 in arg3
        rotr,    // Arguments: 3, Purpose: stores the bitwise right-rotation of arg1 by arg2 in arg3
        _length  // Number of instructions

    };

    inline bool ishint(inst in) { return (int)in <= (int)inst::rfree; }

    extern const uint32_t instlengths[];
    extern const char* instnames[];

}

#endif