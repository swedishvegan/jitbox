#ifndef BYTECODE_HPP
#define BYTECODE_HPP

#include "./address.hpp"
#include "./compiletimeconstant.hpp"
#include "./datatypes.hpp"
#include "./instructionset.hpp"
#include <iostream>
#define JITBOX_BYTECODELOOKAHEAD 4
#define JITBOX_MAXADDRSPERINST 5
#define JITBOX_BYTECODEMAXLENPERINST (1 + JITBOX_MAXADDRSPERINST * 3)
#define JITBOX_HOTNESSTHRESHOLD 3

namespace jitbox {

    using bytecodelist = vec<uint32_t>;

    #define _pushbackctc(ctc) { if (addr.bytewidth() == 3) { union { uint64_t v; uint32_t vv[2]; } v; v.v = addr.val.vu64; bcbuf.adddata(v.vv[0]); bcbuf.adddata(v.vv[1]); } else bcbuf.adddata(addr.val.vu32); }

    struct bcbuffer {

        struct addrdata { uint32_t addr; uint32_t count; };

        struct element {

            int bufsize = 0;
            int addrsize = 0;
            uint32_t buf[JITBOX_BYTECODEMAXLENPERINST];
            addrdata addrs[JITBOX_MAXADDRSPERINST];

        };

        element buf[JITBOX_BYTECODELOOKAHEAD];
        element* cur = nullptr;
        bool isfull = false;
        int idx = 0;
        int sz = 0;

        inline bcbuffer() { }

        inline void adddata(uint32_t v) { cur->buf[cur->bufsize] = v; cur->bufsize++; sz++; }

        inline void addaddr(address& addr) { if (ishint((inst)cur->buf[0])) return; if (addr.addr == 0) return; auto m = addr.mode(); if (m == addressmode::global || m == addressmode::immediate) return; for (int i = 0; i < cur->addrsize; i++) if (cur->addrs[i].addr == addr.addr) return; cur->addrs[cur->addrsize] = addrdata{ addr.addr, 1 }; cur->addrsize++; }

        inline void flush(bytecodelist& bcl) { cur = &buf[idx]; if (!isfull) return; updateaddrcounts(bcl); for (int i = 0; i < cur->bufsize; i++) bcl.push_back(cur->buf[i]); sz -= cur->bufsize; cur->bufsize = -1; }

        inline void beginst(bytecodelist& bcl) { std::cout << " >> beginning buffer: " << idx << "\n"; flush(bcl); cur = &buf[idx]; cur->bufsize = 0; cur->addrsize = 0; }

        inline void endinst() { idx = (idx + 1) % JITBOX_BYTECODELOOKAHEAD; if (idx == 0) isfull = true; }

        inline void flusheverything(bytecodelist& bcl) { int n = isfull ? JITBOX_BYTECODELOOKAHEAD : idx; if (!isfull) idx = 0; for (int i = 0; i < n; i++) { flush(bcl); idx = (idx + 1) % JITBOX_BYTECODELOOKAHEAD; } }

        inline void updateaddrcounts(bytecodelist&);

    };

    struct bytecode : public printable {

        bytecodelist bc;
        bcbuffer bcbuf;

        struct hj { uint32_t idx, val; };
        vec<hj> hjqueue;

        inline bytecode() { }

        inline int size() const { return bc.size() + bcbuf.sz; }

        inline void beginst() { bcbuf.beginst(bc); }

        inline void endinst() { bcbuf.endinst(); }

        inline void end() { bcbuf.flusheverything(bc); for (auto& hj : hjqueue) bc[hj.idx] = hj.val; }

        template <typename... Args>
        inline void addinst(inst in, Args... args) { beginst(); add(in, args...); endinst(); }

        template <typename... Args>
        inline void add(inst in, Args... args) { add(in); add(args...); }

        template <typename... Args>
        inline void add(address addr, Args... args) { add(addr); add(args...); }

        inline void add(inst in) { bcbuf.adddata((uint32_t)in); }

        inline void add(address addr) { bcbuf.addaddr(addr); bcbuf.adddata(addr.addr); if (addr.mode() == addressmode::immediate) _pushbackctc(addr.val); }

        struct iterator {

            inline iterator(const bytecodelist& bc) { this->bc = &bc; }

            inline bool done() const { return idx == bc->size(); };

            inline inst nextinstruction();
            inline address nextaddress();

            int idx = 0;
            const bytecodelist* bc;


        };

        iterator iterate() const { return iterator(bc); }

        string tostring(int) const override;

    };

    using pbytecode = ptr<bytecode>;

}

inline void jitbox::bcbuffer::updateaddrcounts(bytecodelist& bcl) {

    int searchstart = (idx + 1) % JITBOX_BYTECODELOOKAHEAD;
    std::cout << "Updating addrcounts from " << idx << "\n";
    for (int _ = 1; _ < JITBOX_BYTECODELOOKAHEAD; _++) {

        auto future = &buf[searchstart];
        if (future->bufsize < 0 || future->addrsize == 0) continue;
        std::cout << "\tThis future has " << future->addrsize << " addrs and cur has " << cur->addrsize << "\n";
        for (int i = 0; i < cur->addrsize; i++) for (int j = 0; j < future->addrsize; j++)

            {std::cout << "\t\tComparing " << cur->addrs[i].addr << " and " << future->addrs[j].addr << "\n";if (cur->addrs[i].addr == future->addrs[j].addr) { std::cout << "\t\t\tMATCH! " << cur->addrs[i].count << "\n";cur->addrs[i].count++; break; }}

        searchstart = (searchstart + 1) % JITBOX_BYTECODELOOKAHEAD;

    }

    for (int i = 0; i < cur->addrsize; i++) if (cur->addrs[i].count >= JITBOX_HOTNESSTHRESHOLD) {

        bcl.push_back((uint32_t)inst::hot);
        bcl.push_back(cur->addrs[i].addr);
        std::cout << "Found a HOT variable! " << address(cur->addrs[i].addr).index() << "\n";
    }

}

inline jitbox::inst jitbox::bytecode::iterator::nextinstruction() {

    idx++; std::cout << instnames[bc->operator[](idx-1)] << "\n";
    return (inst)bc->operator[](idx - 1);

}

inline jitbox::address jitbox::bytecode::iterator::nextaddress() {

    auto addr = address(bc->operator[](idx));
    auto mode = addr.mode();

    if (mode == addressmode::immediate) {

        addr.val.isconst = true;

        if (addr.bytewidth() == 3) {

            union { uint64_t v; uint32_t vv[2]; } v;
            v.vv[0] = bc->operator[](idx + 1);
            v.vv[1] = bc->operator[](idx + 2);

            addr.val.vu64 = v.v;
            idx += 2;

        }
        else {

            addr.val.vu32 = bc->operator[](idx + 1);
            idx++;

        }

        addr.val.dtype = addr.dtype().mid;

    }
    
    idx++;
    return addr;

}

#endif