#ifndef CONTEXT_HPP
#define CONTEXT_HPP

#include "./datatypes.hpp"

namespace jitbox {

    struct internalinstruction;
    struct internalvariable;
    struct internalcodeblock;
    struct functioncodeblock;
    struct whilecodeblock;
    using pinternalcodeblock = ptr<internalcodeblock>;
    using pinternalvariable = ptr<internalvariable>;

    // keeps track of global program info, must be created before defining code
    struct context {
    
        inline context() { curcontext = this; currentcode = nullptr; currentfunction = nullptr; currentloop = nullptr; }

        ~context();

        vec<pinternalcodeblock> code;
        vec<pinternalvariable> globalvars;
        internalcodeblock* currentcode;
        functioncodeblock* currentfunction;
        whilecodeblock* currentloop;
        internalinstruction* whilehook = nullptr;
        bool makewhilehook = false;
        id nextsigid = 0;
        uint32_t nextglobvaraddr = 8;

        static context* curcontext;

    };

}

#endif