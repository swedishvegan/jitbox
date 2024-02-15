#ifndef ADDRESS_HPP
#define ADDRESS_HPP

#include "./compiletimeconstant.hpp"
#include "./instructionset.hpp"
#include "./type.hpp"

namespace jitbox {
    
    /*
    
    Address format:

        Information:                 [ type ] [ mode ] [ byte width ] [ register hint value ] [ register hint flag ] [ ID ]
        Bytes (lowest to highest):   0-1      2-3      4-5            6-7                     8                      9-31
    
    */

    namespace addresstype { enum : uint32_t { i, u, f }; }
    namespace addressmode { enum : uint32_t { temporary, persistent, immediate, global }; }

    struct address : public printable {

        uint32_t addr;
        compiletimeconstant val;
        
        inline address() : addr(0) { }
        inline address(uint32_t addr) : addr(addr) { }
        inline address(compiletimeconstant val) : val(val) { auto a = address(val.dtype, addressmode::immediate, 0); addr = a.addr; val.dtype = a.dtype().mid; }
        inline address(type dtype, uint32_t mode, uint32_t idx, uint32_t reghint = 4) { addr = 2 * (uint32_t)dtype.isfp() + (uint32_t)dtype.isunsigned() + (mode << 2) + (compressbytewidth(dtype) << 4) + (((reghint < 4) ? 4 + reghint : 0) << 6) + (idx << 9); }  

        string tostring(int) const override;

        inline uint32_t simpledtype() const { return addr % 4; }
        inline type dtype() const;
        inline uint32_t mode() const { return (addr >> 2) % 4; }
        inline uint32_t bytewidth() const { return (addr >> 4) % 4; }
        inline uint32_t reghint() const { return (addr >> 6) % 8; }
        inline uint32_t index() const { return addr >> 9; }

        inline bool operator == (const address& a) { if (addr != a.addr) return false; if ((addr >> 2) % 4 == addressmode::immediate && val != a.val) return false; return true; }
        inline bool operator != (const address& a) { return !(*this == a); }

        inline static uint32_t compressbytewidth(type t) { auto b = t.bytes(); if (b == 1) return 0; if (b == 2) return 1; if (b == 4) return 2; return 3; }

    };

}

inline jitbox::type jitbox::address::dtype() const {

    auto sdt = simpledtype();
    auto bw = bytewidth();

    if (sdt == addresstype::i) return type::i8 + bw;
    else if (sdt == addresstype::u) return type::u8 + bw;
    else return type::f32 + (bw - 2);

}

#endif