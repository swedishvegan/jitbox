#ifndef COMPILETIMECONSTANT_HPP
#define COMPILETIMECONSTANT_HPP

#include "./datatypes.hpp"
#include "./type.hpp"
#include "./instructionset.hpp"

namespace jitbox {

    struct compiletimeconstant : public printable {

        union {

            uint8_t vu8;
            int8_t vi8;
            uint16_t vu16;
            int16_t vi16;
            uint32_t vu32;
            int32_t vi32;
            uint64_t vu64;
            int64_t vi64;
            float vf32;
            double vf64;

        };

        id dtype;
        bool isconst = false;

        inline bool operator == (const compiletimeconstant&);

        inline bool operator != (const compiletimeconstant& ctc) { return !(*this == ctc); }

        string tostringsimple(bool printdtype) const;

        string tostring(int) const override;

    };

    inline compiletimeconstant fold(compiletimeconstant, compiletimeconstant, inst);

#define _defctc(thetype, ctype) \
inline compiletimeconstant const##thetype(ctype val) { compiletimeconstant c; c.v##thetype = val; c.dtype = type::thetype; return c; }

    _defctc(u8, uint8_t);
    _defctc(i8, int8_t);
    _defctc(u16, uint16_t);
    _defctc(i16, int16_t);
    _defctc(u32, uint32_t);
    _defctc(i32, int32_t);
    _defctc(u64, uint64_t);
    _defctc(i64, int64_t);
    _defctc(f32, float);
    _defctc(f64, double);

};

inline bool jitbox::compiletimeconstant::operator == (const compiletimeconstant& ctc) {

    if (dtype != ctc.dtype) return false;

    auto bw = type(dtype).bytes();

    if (bw == 1) return vu8 == ctc.vu8;
    if (bw == 2) return vu16 == ctc.vu16;
    if (bw == 4) return vu32 == ctc.vu32;
    return vu64 == ctc.vu64;

}

jitbox::compiletimeconstant jitbox::fold(compiletimeconstant lh, compiletimeconstant rh, inst in) {

    return lh;

}

#endif