#ifndef JITBOXINTERNAL_HPP
#define JITBOXINTERNAL_HPP

#include "./address.hpp"
#include "./bytecode.hpp"
#include "./compiletimeconstant.hpp"
#include "./context.hpp"
#include "./datatypes.hpp"
#include "./instructionset.hpp"
#include "./type.hpp"
#include <iostream>
namespace jitbox {
    
    struct internalvariable;

    struct internalcodeblock : public compilerobject {

        enum codetype { functionblock, conditionalblock, ifblock, elseblock, whileblock } ctype;

        inline void addobject(pcompilerobject);

        inline internalcodeblock() : compilerobject(objtype::codeblock) { code = &buf1; buf = &buf2; }

        inline void makecurrent() const;

        bool isparentof(internalcodeblock* cb) const { if (!cb) return false; auto cc = curchild; while(cc) { if (cc == cb) return true; cc = cc->curchild; } return false; }

        codelist* code;
        codelist* buf;
        codelist buf1, buf2;

        internalcodeblock* parent = nullptr;
        internalcodeblock* curchild = nullptr;

        int idxcount = 0;

        vec<uint32_t> hangingjumps;

        inline void swapbuffers();

        string tostring(int) const override;

    };

    struct internalinstruction;
    
    struct functioncodeblock : public internalcodeblock {

        bool registered = false;
        signature sig;
        bool isinline = false;
        vec<internalvariable> args;
        vec<bool> passbyref;
        bool returnref = false;
        uint32_t nextfreeaddr = 1;
        pbytecode bc;
        int lastvarusageidx = 0;
        internalinstruction* firstfunctioncall = nullptr;
        internalinstruction* lastfunctioncall = nullptr;

        inline functioncodeblock();

        inline void genargs();

    };

    struct ifcodeblock;
    struct elsecodeblock;

    struct conditionalcodeblock : public internalcodeblock { 
        
        inline conditionalcodeblock() { ctype = conditionalblock; } 

        inline ifcodeblock* getifcode() const { return (ifcodeblock*)(*code)[0](); }

        inline elsecodeblock* getelsecode() const { return (code->size() == 2) ? (elsecodeblock*)(*code)[1]() : nullptr; }
        
    };

    struct ifcodeblock : public internalcodeblock { 

        internalinstruction* condition;
        bool haselse = false;
        
        inline ifcodeblock(internalinstruction* condition) : condition(condition) { ctype = ifblock; }
        
    };

    struct elsecodeblock : public internalcodeblock { inline elsecodeblock() { ctype = elseblock; } };

    struct whilecodeblock : public internalcodeblock { 
        
        internalinstruction* condition;
        int whilebegidx = -1;
        
        vec<internalvariable*> loopvars;

        inline whilecodeblock(internalinstruction* condition) : condition(condition) { ctype = whileblock; }

    };

    struct internalinstruction;
    using pinternalinstruction = ptr<internalinstruction>;

    struct internalinstruction : public compilerobject {

        template <typename... Args>
        inline static pinternalinstruction makeinstruction(context&, int nextidx, type, int nargs, inst opcode, Args... args);

        internalvariable** args;
        int nargs;
        inst opcode;
        type dtype;
        internalvariable* lh = nullptr;
        whilecodeblock* forwardhook = nullptr;
        internalinstruction* link = nullptr;
        bool isjump = false;

        inline internalinstruction(type dtype, int nargs, inst opcode) : compilerobject(objtype::instruction), dtype(dtype), args((nargs > 0) ? new internalvariable*[nargs] : nullptr), nargs(nargs), opcode(opcode) { }

        template <typename... Args>
        inline static void processinstruction(internalinstruction*, int, int, internalvariable*, Args...);
        inline static void processinstruction(internalinstruction*, int, int) { }

        inline ~internalinstruction() { if (args) delete[] args; }

        string tostring(int) const override;

        friend struct context;

    };

    struct internalvariable : public compilerobject {

        uint32_t addr;
        type dtype;
        compiletimeconstant value;
        internalcodeblock* owner = nullptr;        
        internalinstruction* rh = nullptr;
        internalvariable* alias = nullptr;
        internalvariable* successor = nullptr;
        internalinstruction* lastfunctioncall = nullptr;
        int reghint = -1;
        int lrend = -1;
        bool isglobal = false;

        inline internalvariable(internalinstruction*);
        inline internalvariable(compiletimeconstant, bool isglobal = false);

        inline internalvariable() : compilerobject(objtype::variable) { }

        inline bool dependson(internalvariable*) const;
        
        string tostring(int) const override;

    };

    using pinternalvariable = ptr<internalvariable>;

    struct internalcompiler {

        context& ctxt;

        inline internalcompiler(context&);

        struct addressindexgenerator { uint32_t itmp = 1, iprs = 1, ftmp = 1, fprs = 1; inline uint32_t& getidx(type, uint32_t); };

        struct bcaddress { bool initialized = false; address addr; int lrend = -1; };
        using addressmap = vec<bcaddress>;

        inline address registeraddress(internalvariable*, functioncodeblock*, addressindexgenerator&, addressmap&);

        inline void flatten(functioncodeblock*, internalcodeblock*, bytecode*, addressindexgenerator&, addressmap&, int&);

    };

}

inline void jitbox::internalcodeblock::addobject(pcompilerobject obj) { 
    
    obj->idx = idxcount;
    code->push_back(obj); 

    auto objtype = obj->otype;
    
    if (objtype == compilerobject::objtype::variable) { 
        
        auto iv = (internalvariable*)obj(); 
        iv->lrend = idxcount;

        if (!iv->value.isconst && !iv->alias) context::curcontext->currentfunction->lastvarusageidx = iv->rh ? iv->rh->idx : idxcount;

    }

    else if (objtype == compilerobject::objtype::codeblock) obj.cast<internalcodeblock>()->idxcount = obj->idx;

    else {

        auto in = (internalinstruction*)obj();
        if (in->opcode == inst::call || in->opcode == inst::farg) for (int i = 0; i < in->nargs; i++) if (!in->args[i]->value.isconst) { context::curcontext->currentfunction->lastvarusageidx = idxcount; break; }

    }

    idxcount++;

}

inline void jitbox::internalcodeblock::makecurrent() const {

    if (!parent) context::curcontext->currentfunction = (functioncodeblock*)this;
    if (curchild) curchild->makecurrent();
    else context::curcontext->currentcode = (internalcodeblock*)this;

}

inline void jitbox::internalcodeblock::swapbuffers() { 
    
    for (auto c : *code) if (c->otype == compilerobject::objtype::codeblock) c.cast<internalcodeblock>()->swapbuffers();
    auto tmp = buf; buf = code; code = tmp;
    
}

inline jitbox::functioncodeblock::functioncodeblock() { 
    
    ctype = functionblock; 
    bc = new bytecode(); 

    auto& ctxt = *context::curcontext;

    sig.identifier = ctxt.nextsigid;
    ctxt.nextsigid++;
    
}

inline void jitbox::functioncodeblock::genargs() {

    int newsize = sig.types.size();

    args.resize(newsize);
    passbyref.resize(newsize);

    for (int i = 0; i < newsize; i++) {

        auto& arg = args[i];

        arg.addr = nextfreeaddr;
        arg.dtype = type(sig.types[i]);
        arg.owner = (internalcodeblock*)this;

        passbyref[i] = false;

        nextfreeaddr++;

    }

}

template <typename... Args>
inline jitbox::pinternalinstruction jitbox::internalinstruction::makeinstruction(context& ctxt, int nextidx, type t, int nargs, inst opcode, Args... args) {

    pinternalinstruction in = new internalinstruction(t, nargs, opcode);

    if (ctxt.makewhilehook) { ctxt.whilehook = in(); ctxt.makewhilehook = false; }

    processinstruction(in(), nextidx, 0, args...);

    return in;

}

inline jitbox::internalvariable::internalvariable(internalinstruction* in) : compilerobject(objtype::variable) {
    
    auto& ctxt = *context::curcontext;
    auto curfunc = ctxt.currentfunction;

    addr = curfunc->nextfreeaddr;
    dtype = in->dtype;
    owner = ctxt.currentcode;
    rh = in;
    lastfunctioncall = curfunc->lastfunctioncall;
    in->lh = this;

    curfunc->nextfreeaddr++;

}

inline jitbox::internalvariable::internalvariable(compiletimeconstant c, bool isglobal): compilerobject(objtype::variable), isglobal(isglobal) {

    auto& ctxt = *context::curcontext;
    auto curfunc = ctxt.currentfunction;

    auto& nextaddr = isglobal ? ctxt.nextglobvaraddr : curfunc->nextfreeaddr;
    addr = nextaddr;
    value = c;
    value.isconst = true;
    dtype = type(c.dtype);

    uint32_t increment = isglobal ? 4 : 1;

    if (!isglobal) {
        
        owner = ctxt.currentcode;
        rh = nullptr;
        lastfunctioncall = curfunc->lastfunctioncall;

    } else std::cout << "\n\nTHIS CALLED! " << ctxt.nextsigid << "\n\n";

    nextaddr += increment;

}

inline bool jitbox::internalvariable::dependson(internalvariable* iv) const {

    if (this == iv) return true;
    if (!this->rh) return false;

    auto instargs = this->rh->args;
    auto nargs = this->rh->nargs;

    for (int i = 0; i < nargs; i++) if (instargs[i]->dependson(iv)) return true;
    return false;

}

inline jitbox::internalcompiler::internalcompiler(context& ctxt) :ctxt(ctxt) {

    addressmap amap;
    
    for (auto c : ctxt.code) {

        addressindexgenerator igen{ };

        auto func = c.cast<functioncodeblock>()();

        amap.clear();
        amap.resize(func->nextfreeaddr, bcaddress{ });
        
        func->bc->addinst(inst::begfnc);
        for (auto arg : func->args) func->bc->addinst(inst::regarg, registeraddress(&arg, func, igen, amap));

        int idx = 0;
        flatten(func, c(), func->bc(), igen, amap, idx);

        func->bc->addinst(inst::endfnc);
        func->bc->end();
        std::cout << c.cast<functioncodeblock>()->bc->print() << "\n";
    } 
    
}

inline uint32_t& jitbox::internalcompiler::addressindexgenerator::getidx(type dtype, uint32_t mode) {

    return 
        (mode == addressmode::temporary)
            ? (dtype.isfp() ? ftmp : itmp)
            : (dtype.isfp() ? fprs : iprs)
        ;

}

inline jitbox::address jitbox::internalcompiler::registeraddress(internalvariable* iv, functioncodeblock* curfunc, addressindexgenerator& igen, addressmap& amap) {

    if (iv->isglobal) return address(iv->dtype, addressmode::global, iv->addr);

    auto& bcaddr = amap[iv->addr];

    if (bcaddr.initialized) return bcaddr.addr;

    if (iv->successor && iv->lrend < iv->successor->idx) {

        if (iv->reghint >= 0) iv->successor->reghint = iv->reghint;

        registeraddress(iv->successor, curfunc, igen, amap);

        if (iv->successor->reghint >= 0) iv->reghint = iv->successor->reghint;

        auto& bcaddrnext = amap[iv->successor->addr];
        bcaddr = bcaddrnext;
        bcaddrnext.lrend = -1;

        return bcaddr.addr;

    }

    uint32_t mode = iv->value.isconst ? addressmode::immediate : addressmode::temporary;
        
    if (mode == addressmode::temporary && iv->lastfunctioncall != curfunc->lastfunctioncall) {

        auto nextfc = iv->lastfunctioncall ? iv->lastfunctioncall->link : curfunc->firstfunctioncall;
        if (nextfc->idx < iv->lrend) mode = addressmode::persistent;

    }
    
    uint32_t reghint = iv->reghint >= 0 ? iv->reghint : 4;
    uint32_t& idx = igen.getidx(iv->dtype, mode);

    address addr(iv->dtype, mode, idx, reghint);
    
    if (mode == addressmode::immediate) addr.val = iv->value;
    else idx++;

    bcaddr.initialized = true;
    bcaddr.addr = addr;
    bcaddr.lrend = addr.val.isconst ? -1 : iv->lrend;

    return addr;

}

inline void jitbox::internalcompiler::flatten(functioncodeblock* curfunc, internalcodeblock* code, bytecode* bc, addressindexgenerator& igen, addressmap& amap, int& idx) {

    for (int codeidx = 0; codeidx < code->code->size(); codeidx++) {

        auto c = code->code->operator[](codeidx);

        if (c->otype == compilerobject::objtype::instruction) {

            auto in = c.cast<internalinstruction>()();

            if (in->forwardhook) in->forwardhook->whilebegidx = bc->size();

            bool redundantmov = false;

            if (in->opcode == inst::mov) {

                auto src = in->args[0];
                auto dst = (in->nargs == 2) ? in->args[1] : in->lh;

                redundantmov = registeraddress(src, curfunc, igen, amap) == registeraddress(dst, curfunc, igen, amap);

            }

            if (redundantmov);

            else if (in->opcode == inst::alias) {

                auto srcaddr = in->args[0]->addr;
                auto dstaddr = in->lh->addr;

                amap[dstaddr] = amap[srcaddr];
                amap[dstaddr].lrend = -1;

            }

            else {

                auto hasreturnarg = in->nargs < instlengths[(uint32_t)in->opcode];
                auto opcode = (int)in->opcode;

                if (hasreturnarg && !in->lh && opcode >= (int)inst::zero && opcode <= (int)inst::gte) {

                    auto newopcode = (inst)(opcode + (int)inst::jz - (int)inst::zero);

                    switch(newopcode) {

                        case inst::jz: newopcode = inst::jnz; break;
                        case inst::jnz: newopcode = inst::jz; break;
                        case inst::je: newopcode = inst::jne; break;
                        case inst::jne: newopcode = inst::je; break;
                        case inst::jg: newopcode = inst::jge; break;
                        case inst::jge: newopcode = inst::jg;

                    }

                    if (newopcode == inst::jg || newopcode == inst::jge) {

                        auto temp = in->args[0];
                        in->args[0] = in->args[1];
                        in->args[1] = temp;

                    }

                    bc->beginst();
                    bc->add(newopcode, constu32(0));

                    auto childblock = code->code->operator[](codeidx + 1).cast<internalcodeblock>()();
                    if (childblock->ctype == internalcodeblock::conditionalblock) childblock = childblock->code->operator[](0).cast<internalcodeblock>()();

                    childblock->hangingjumps.push_back(bc->size() - 1);

                    for (int i = 0; i < in->nargs; i++) bc->add(registeraddress(in->args[i], curfunc, igen, amap));
                    bc->endinst();

                }
                else {

                    bc->beginst();
                    bc->add(in->opcode);
                    for (int i = 0; i < in->nargs; i++) bc->add(registeraddress(in->args[i], curfunc, igen, amap));
                    if (in->lh) bc->add(registeraddress(in->lh, curfunc, igen, amap));
                    else if (hasreturnarg) bc->add(address());
                    bc->endinst();

                }

            }

        }

        else if (c->otype == compilerobject::objtype::variable) {

            auto iv = c.cast<internalvariable>()();

            if (!iv->rh && !iv->value.isconst) bc->addinst(inst::mov, address(iv->value), registeraddress(iv, curfunc, igen, amap));

        }

        else if (c->otype == compilerobject::objtype::codeblock) {

            flatten(curfunc, c.cast<internalcodeblock>()(), bc, igen, amap, idx);
            continue;

        }

        if (idx < curfunc->lastvarusageidx) for (auto& bcaddr : amap) if (idx + 1 == bcaddr.lrend) {

            bc->addinst(inst::rfree, bcaddr.addr);
            bcaddr.lrend = -1;

        }

        idx++;

    }

    if (code->ctype == internalcodeblock::ifblock) {

        auto ifcode = (ifcodeblock*)code;

        if (ifcode->haselse) {

            bc->addinst(inst::j, constu32(0));
            
            auto ifparent = (internalcodeblock*)ifcode->parent;
            auto elsecode = ifparent->code->operator[](1).cast<elsecodeblock>()();

            elsecode->hangingjumps.push_back(bc->size() - 1);

        }

    }

    else if (code->ctype == internalcodeblock::whileblock) {

        auto whilecode = (whilecodeblock*)code;
        bc->addinst(inst::j, constu32(whilecode->whilebegidx));

    }

    for (auto hj : code->hangingjumps) bc->hjqueue.push_back(bytecode::hj{ hj, (uint32_t)bc->size() });

}

#endif