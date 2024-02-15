#ifndef JIT_HPP
#define JIT_HPP

#include "../../deps/sljit_src/sljitLir.h"
#include "./datatypes.hpp"

namespace jitbox {

    enum accesstype : uint8_t { itmp, iprs, ftmp, fprs };

    struct runtimeaddress;

    struct ssa {

        runtimeaddress* location = nullptr;
        runtimeaddress* reghint;
        accesstype atype;
        bool active = true;
        bool spilled = false;
        int lastaccess = -1;

        inline ssa(accesstype atype, runtimeaddress* reghint) : atype(atype), reghint(reghint) { }

        inline void allocate(addressspace*);

        inline void free();

        inline void spill(addressspace*);

        inline void unspill(addressspace*);

    };

    struct runtimeaddress {

        ssa* owner = nullptr;
        sljit_sw a0, a1;
        bool isprs;

    };

    struct jit;

    struct addressspace {
        
        vec<runtimeaddress> vi, vf, vs;
        jit* owner = nullptr;

        inline void reset();

        inline void populateregisteraddrs();

        inline runtimeaddress* nextfreeregaddr(accesstype);

        inline runtimeaddress* nextfreestackaddr();
        
        inline runtimeaddress* getargregister(int idx, bool isfp);

    };

    using jitfunc = void();

    struct jit {

        inline jit(uint32_t* code, uint32_t* stack, uint64_t ip);

        inline void makemov(runtimeaddress* src, runtimeaddress* dst);

        uint32_t *code, *stack;

        jitfunc* func = nullptr;
        sljit_sw execoffset = 0;

        struct hangingjump { 
            
            sljit_sw ipjump, iptarget;

            sljit_jump* jump = nullptr;
            sljit_label* target = nullptr;
            
        };

        struct rewritablefunctioncall {

            int64_t ip_jaddr, ip_callee;

            sljit_jump* jump = nullptr;
            int64_t ip_jaddr_next = -1;

        };

        sljit_compiler* compiler;

        static vec<hangingjump> hangingjumps;
        static vec<rewritablefunctioncall> rewritablefunctioncalls;

    };

}

inline void jitbox::ssa::allocate(addressspace* space) {

    if (reghint && reghint->owner == nullptr) location = reghint;

    else {

        auto nfra = space->nextfreeregaddr(atype);

        if (nfra) location = nfra;
        else location = space->nextfreestackaddr();

    }

    location->owner = this;
    reghint = nullptr;

}

inline void jitbox::ssa::free() { if (!location) return; location->owner = nullptr; active = false; }

inline void jitbox::ssa::spill(addressspace* space) {

    auto prevlocation = location;

    location->owner = nullptr;
    location = space->nextfreestackaddr();
    location->owner = this;

    spilled = true;
    space->owner->makemov(prevlocation, location);

}

inline void jitbox::ssa::unspill(addressspace* space) { 
    
    if (!spilled) return; 

    auto candidate = space->nextfreeregaddr(atype);
    if (!candidate) return;

    location->owner = nullptr;
    candidate->owner = this;
    location = candidate;

    spilled = false;
    space->owner->makemov(location, candidate);
    
}

inline void jitbox::addressspace::reset() {

    if (vi.size() == 0) populateregisteraddrs();

    vi.resize(SLJIT_NUMBER_OF_REGISTERS);
    vf.resize(SLJIT_NUMBER_OF_FLOAT_REGISTERS);
    vs.resize(0);

}

inline void jitbox::addressspace::populateregisteraddrs() {

    for (int i = 0; i < SLJIT_NUMBER_OF_REGISTERS; i++) {

        runtimeaddress newaddr;

        newaddr.a0 = SLJIT_R0 + i;
        newaddr.a1 = 0;
        newaddr.isprs = SLJIT_R0 + i > SLJIT_S0 - SLJIT_NUMBER_OF_SAVED_REGISTERS;

        vi[i] = newaddr;
        
    }

    for (int i = 0; i < SLJIT_NUMBER_OF_FLOAT_REGISTERS; i++) {

        runtimeaddress newaddr;

        newaddr.a0 = SLJIT_FR0 + i;
        newaddr.a1 = 0;
        newaddr.isprs = SLJIT_FR0 + i > SLJIT_FS0 - SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS;

        vf[i] = newaddr;

    } 

}

inline jitbox::runtimeaddress* jitbox::addressspace::nextfreeregaddr(accesstype atype) {

    bool isint = atype < accesstype::ftmp;
    bool istmp = atype % 2 == 0;

    auto& regspace = isint ? vi : vf;

    int r0 = isint ? SLJIT_R0 : SLJIT_FR0;
    int s0 = isint ? SLJIT_S0 : SLJIT_FS0;

    int ntmp = isint ? SLJIT_NUMBER_OF_SCRATCH_REGISTERS : SLJIT_NUMBER_OF_SCRATCH_FLOAT_REGISTERS;
    int nprs = isint ? SLJIT_NUMBER_OF_SAVED_REGISTERS : SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS;

    int loopstart = istmp ? r0 + ntmp - 1 : s0;
    int loopend = istmp ? r0 : s0 - nprs + 1;

    for (int i = loopstart; i >= loopend; i--)
        if (regspace[i].owner == nullptr) return &regspace[i];

    loopstart = istmp ? s0 - nprs + 1 : r0 + ntmp - 1;
    loopend = istmp ? s0 : r0;

    if (istmp) for (int i = loopstart; i <= loopend; i++) if (regspace[i].owner == nullptr) return &regspace[i];
    else for (int i = loopstart; i >= loopend; i--) if (regspace[i].owner == nullptr) return &regspace[i];

    return nullptr;

}

inline jitbox::runtimeaddress* jitbox::addressspace::nextfreestackaddr() {

    for (int i = 0; i < vs.size(); i++) if (vs[i].owner == nullptr) return &vs[i];

    runtimeaddress newaddr;

    newaddr.a0 = SLJIT_MEM;
    newaddr.a1 = (sljit_sw)owner->stack + 8 * (sljit_sw)vs.size();
    newaddr.isprs = true;

    vs.push_back(newaddr);
    return &vs[vs.size() - 1];

}

inline jitbox::runtimeaddress* jitbox::addressspace::getargregister(int idx, bool isfp) { return isfp ? &vf[idx] : &vi[idx]; }

/*inline bool jitbox::jit::preparejumps() {
    
    //std::cout << "Preparing jumps: ip=" << (ip-1) << "\n";
    auto cur_ip = ip - 1;
    auto inst_type = program[cur_ip];
    auto inst_info_idx = Eval::inst_info_offsets[inst_type];
    auto num_args = Eval::inst_info[inst_info_idx];
    auto inst_name = Eval::inst_names[inst_type];

    if (inst_type == Eval::inst::scpbeg) cur_scope++;
    else if (inst_type == Eval::inst::scpend) { cur_scope--; if (cur_scope == 0) { ip = ip_temp; return false; } }

    else if (inst_type == Eval::inst::j || inst_type == Eval::inst::jc) hangingjumps.push_back({hangingjump{ cur_ip, program[ip] }});

    inst_info_idx++;

    for (int arg = 0; arg < num_args; arg++) {
        //std::cout << "\targ" << arg << ": ip=" << (ip-1) << "\n";
        if (Eval::inst_info[inst_info_idx + arg]) ip += (program[ip] % 32 <= Eval::access::imm) ? 2 : 3;
        else ip++;

    }

    ip++;
    return true;

}*/

#endif