#ifndef JITBOX_HPP
#define JITBOX_HPP

#include <cstdint>
#include <vector>
#include <map>
#include <string>
#include <cstring>

// this file defines both the public and private jitbox api
//// namespace jitbox ==> public api
//// namespace _jitbox ==> private internal api

#define JITBOX_DEBUG

namespace jitbox {

    using u8 = uint8_t;
    using i8 = int8_t;
    using u16 = uint16_t;
    using i16 = int16_t;
    using u32 = uint32_t;
    using i32 = int32_t;
    using u64 = uint64_t;
    using i64 = int64_t;
    using f32 = float;
    using f64 = double;

    template <typename T>
    using vec = std::vector<T>;

    template <typename K, typename V>
    using map = std::map<K, V>;

    using string = std::string;
                                                                                            // every type is assigned a unique integer id
    using id = i32;   
                                                                                            // every variable gets an SSA id value that is unique within its function if it is local (assigned positive numbers), or unique within its context if it is global (assigned negative numbers); zero is a reserved value that will never be used as an SSA id
    using ssaid = i32;
                                                                                            // 8-bit error code
    using errorcode = u8;                                                                   

    using typelist = vec<id>;    
                                                                                            // used for pretty printing jitbox data
    struct printable {                                                                      

        string print(int alignment = 0, bool addnewline = true) const;      

        virtual ~printable();

        virtual string tostring(int alignment) const = 0;

        static string indent(int);

        static string pad(string, int);

    };

#ifdef JITBOX_DEBUG

    struct error : public printable {

        enum : errorcode {     
                                                                                            // no error
            ok,                                                                
                                                                                            // a type constructor was called using an invalid id
            invalidtypeid,
                                                                                            // a jitbox function was called that requires an active context, but no context was active
            noactivecontext,
                                                                                            // a jitbox function was called that requires an active scope, but no scope was active
            noactivescope,

        };
        
        errorcode code;

        error(errorcode code);

        string tostring(int) const override;

    };

#endif
                                                                                            // each user-defined function or structure gets a unique signature
    struct signature : public printable {					                                
                                                                                            // a typelist alone is not sufficient to distinguish signatures -- after all, two functions could have the same argument types, but they should still not be considered the same function
        id identifier;										                                
        typelist types; 
                                                                                            // only relevant for function signatures
        id returntype = 0;									                                

        bool operator < (const signature&) const;

        string tostring(int) const override;

    };
                                                                                            // creates a 1-1 mapping between types and integer ids
    struct type : public printable {						                                

        enum : id {
                                                                                            // used for void functions
            nothing,                                                                        
                                                                                            // used for creating abstract types
            anything,                                                                       
                                                                                            // abstract primitive type
            primitive,                                                          
                                                                                            // concrete primitive type
            i8, i16, i32, i64, u8, u16, u32, u64, f32, f64,                                 
                                                                                            // abstract derived types
            pointer, array, structure, function,                                            

        };
                                                                                            // default constructor; contains type::nothing
        type();                                                                         
                                                                                            // basic constructor; will throw an error if the id supplied is invalid and JITBOX_DEBUG is defined
        type(id);
                                                                                            // assigns an id to a pointer type based on the type that is being pointed to
        static type makepointer(id);
                                                                                            // assigns an id to an array type based on the type that the array contains
        static type makearray(id);
                                                                                            // assigns an id to a structure type based on a signature
        static type makestructure(const signature&);
                                                                                            // assigns an id to a function type based on a signature
        static type makefunction(const signature&);
                                                                                            // returns the type that is being pointed to; throws an exception if not a pointer type
        id getpointertype() const;
                                                                                            // returns the type that the array contains; throws an exception if not an array type
        id getarraytype() const;
                                                                                            // returns the signature for this structure; throws an exception if not a structure type
        const signature& getstructuretype() const;
                                                                                            // returns the signature for this function; throws an exception if not a function type
        const signature& getfunctiontype() const;
                                                                                            // semantic asymmetrical type comparison, supports comparison of abstract and concrete types
        bool is(type) const; bool is(id i) const;

        id getid() const;
                                                                                            // returns either nothing, anything, primitive, pointer, array, structure, or function
        id getclass() const;

        bool isconcrete() const;

        bool type::isabstract() const;

        bool type::isinteger() const;

        bool type::isunsigned() const;

        bool type::isfp() const;
                                                                                            // number of bytes that an object of this type takes up in memory, returns -1 if size is not known at compile time
        int numbytes() const;

    protected:

        id mid = nothing;
        id mclass = nothing;

        void determineclass();

        string tostring(int) const override;

    };

    bool operator == (const type&, id);
    bool operator == (const type&, const type&);
    bool operator == (id, const type&);

    bool operator != (const type&, id);
    bool operator != (const type&, const type&);
    bool operator != (id, const type&);

    struct constant : public printable {

        union {

            u8 vu8;
            i8 vi8;
            u16 vu16;
            i16 vi16;
            u32 vu32;
            i32 vi32;
            u64 vu64;
            i64 vi64;
            f32 vf32;
            f64 vf64;

        };

        type dtype;
        // if this variable is false, this object is treated as garbage and its value is ignored
        bool isconst = false;

        bool operator == (const constant&);

        bool operator != (const constant& ctc) { return !(*this == ctc); }

        string tostring(int) const override;

    };

}

namespace _jitbox {

    using u8 = uint8_t;
    using i8 = int8_t;
    using u16 = uint16_t;
    using i16 = int16_t;
    using u32 = uint32_t;
    using i32 = int32_t;
    using u64 = uint64_t;
    using i64 = int64_t;
    using f32 = float;
    using f64 = double;

    template <typename T>
    using vec = std::vector<T>;

    template <typename T>
    using map = std::map<T>;

    using ssaid = jitbox::ssaid;                                                            

    template <typename ptrtype>
    struct ptr;
                                                                                            // bytecode instruction set
    enum opcode : u8 {                                                                      

                                                                                            // arguments: 1, purpose: marks the beginning of a function
        begfnc, 
                                                                                            // arguments: 0, purpose: marks the end of a function 
        endfnc, 
                                                                                            // arguments: 2, purpose: marks arg1 as the owner of arg2, so that changes in arg2 are reflected in arg1
        alias,  
                                                                                            // arguments: 1, purpose: marks arg1 as an argument in a function call
        farg,   
                                                                                            // arguments: 1, purpose: tells the JIT that arg1 is an argument and does not need to be allocated because the caller already allocated it (its runtime address is inferred by the order that these instructions are generated)
        regarg, 
                                                                                            // arguments: 2, purpose: hints to the JIT to try to allocate arg1 in argument register arg2
        rhint,  
                                                                                            // arguments: 1, purpose: hints to the JIT that arg1 should be placed in a register
        hot,    
                                                                                            // arguments: 0, purpose: marks the beginning of loop mode
        beglp,  
                                                                                            // arguments: 0, purpose: marks the end of loop mode
        endlp,  
                                                                                            // arguments: 1, purpose: tells the JIT that arg1 is no longer in use and its address can be used by another live variable
        rfree,  
                                                                                            // arguments: 1, purpose: terminates the program with code arg1
        exit,   
                                                                                            // arguments: 1, purpose: sets the IP to arg1
        j,      
                                                                                            // arguments: 2, purpose: sets the IP to arg1 iff arg2 == 0
        jz,     
                                                                                            // arguments: 2, purpose: sets the IP to arg1 iff arg2 != 0
        jnz,    
                                                                                            // arguments: 3, purpose: sets the IP to arg1 iff arg2 == arg3
        je,     
                                                                                            // arguments: 3, purpose: sets the IP to arg1 iff arg2 != arg3
        jne,    
                                                                                            // arguments: 3, purpose: sets the IP to arg1 iff arg2 > arg3
        jg,     
                                                                                            // arguments: 3, purpose: sets the IP to arg1 iff arg2 >= arg3
        jge,    
                                                                                            // arguments: 1, purpose: calls the function arg1
        callv,  
                                                                                            // arguments: 2, purpose: calls the function arg1 and stores the result in arg2
        call,   
                                                                                            // arguments: 0, purpose: sets the IP to the value on top of the stack
        retv,   
                                                                                            // arguments: 1, purpose: moves arg1 into the return register and sets the IP to the value on top of the stack
        ret,    
                                                                                            // arguments: 2, purpose: allocates arg1 bytes on the heap and sets arg2 to the new allocated address
        halloc, 
                                                                                            // arguments: 2, purpose: allocates arg1 bytes on the heap, zeros the newly allocated memory, and sets arg2 to the new allocated address
        hinit,  
                                                                                            // arguments: 3, purpose: copies arg1 bytes from arg2 into arg3 
        copy,   
                                                                                            // arguments: 2, purpose: moves the value of arg1 into arg2
        mov,    
                                                                                            // arguments: 4, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 == 0
        movz,   
                                                                                            // arguments: 4, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 != 0
        movnz,  
                                                                                            // arguments: 5, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 == arg5
        move,   
                                                                                            // arguments: 5, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 != arg5
        movne,  
                                                                                            // arguments: 5, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 > arg5
        movg,   
                                                                                            // arguments: 5, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 >= arg5
        movge,  
                                                                                            // arguments: 2, purpose: moves the address of arg1 into arg2
        ref,    
                                                                                            // arguments: 2, purpose: moves the value in the address referenced by arg1 into arg2
        deref,  
                                                                                            // arguments: 2, purpose: moves the byte size of the heap object at the address referenced by arg1 into arg2
        objlen, 
                                                                                            // arguments: 2, purpose: casts arg1 to a new type and stores the result in arg2
        cast,   
                                                                                            // arguments: 3, purpose: stores the sum of arg1 and arg2 in arg3
        add,    
                                                                                            // arguments: 3, purpose: stores the difference of arg1 and arg2 in arg3
        sub,    
                                                                                            // arguments: 3, purpose: stores the product of arg1 and arg2 in arg3
        mul,    
                                                                                            // arguments: 3, purpose: stores the quotient of arg1 and arg2 in arg3
        div,    
                                                                                            // arguments: 3, purpose: stores the remainder of dividing arg1 by arg2 in arg3
        mod,    
                                                                                            // arguments: 3, purpose: stores the bitwise AND of arg1 and arg2 in arg3
        band,   
                                                                                            // arguments: 3, purpose: stores the bitwise OR of arg1 and arg2 in arg3
        bor,    
                                                                                            // arguments: 3, purpose: stores the bitwise XOR of arg1 and arg2 in arg3
        bxor,   
                                                                                            // arguments: 2, purpose: stores the bitwise NOT of arg1 in arg2
        bnot,   
                                                                                            // arguments: 2, purpose: stores the boolean value arg1 == 0 in arg2
        zero,   
                                                                                            // arguments: 2, purpose: stores the boolean value arg1 != 0 in arg2
        nzero,  
                                                                                            // arguments: 3, purpose: stores the boolean value arg1 == arg2 in arg3
        eq,     
                                                                                            // arguments: 3, purpose: stores the boolean value arg1 != arg2 in arg3
        neq,    
                                                                                            // arguments: 3, purpose: stores the boolean value arg1 > arg2 in arg3
        gt,     
                                                                                            // arguments: 3, purpose: stores the boolean value arg1 >= arg2 in arg3
        gte,    
                                                                                            // arguments: 3, purpose: stores the bitwise left-shift of arg1 by arg2 in arg3
        shl,    
                                                                                            // arguments: 3, purpose: stores the bitwise right-shift of arg1 by arg2 in arg3
        shr,    
                                                                                            // arguments: 3, purpose: stores the bitwise left-rotation of arg1 by arg2 in arg3
        rotl,   
                                                                                            // arguments: 3, purpose: stores the bitwise right-rotation of arg1 by arg2 in arg3
        rotr,   
                                                                                            // number of instructions
        _length 

    };

    extern const u8 instructionlengths[];
    extern const char* instructionnames[];
                                                                                            // base class for instruction, variable, and codeblock -- the three top-level objects that make up the jitbox intermediate representation
    struct compilerobject {                                                                 

        enum class cotype : u8 { instruction, variable, codeblock } type;
                                                                                            // offset of this object within its parent codeblock if this object is an instruction or codeblock; ignored if this object is a variable
        u32 index = 0;                                                                      

        compilerobject(cotype);

    };

    using pcompilerobject = ptr<compilerobject>;

    struct variable;
    struct context;
                                                                                            // intermediate representation for a single instruction; consists of an opcode and an array of instructions
    struct instruction : public compilerobject {                                            
                                                                                            // maximum number of arguments of any instruction
        static const u8 maxnumargs = 5;                                                     
        opcode op;           
        ssaid args[maxnumargs];
                                                                                            // index to the LH if the instruction is the RH of an assignment; zero otherwise
        ssaid lh;                                                                     
                                                                                            // this constructor should never be called anywhere except in the codeblock::addinstruction function
        instruction(opcode, ssaid* srcargs);

    };

    using pinstruction = ptr<instruction>;

    struct codeblock;

    struct variable : public compilerobject {

        ssaid id;
        codeblock* owner = nullptr;
        u32 lrbeg, lrend;
        jitbox::constant val;
                                                                                            // RH value that is assigned to this variable
        instruction* rh;                                                                    

        variable();
                                                                                            // this constructor should never be called anywhere except in the codeblock::addvariable function
        variable(codeblock* owner, jitbox::type);
                                                                                            // this constructor should never be called anywhere except in the codeblock::addconstant function
        variable(codeblock* owner, jitbox::constant);

    };

    using pvariable = ptr<variable>;

    struct codeblock : public compilerobject {
                                                                                            // contains all variables in the function that this codeblock belongs to; memory is allocated and deleted by the top-level scope codeblock
        vec<pcompilerobject> code;
        vec<pvariable>* variables;                                                          
                                                                                            // type id associated with this function
        jitbox::type dtype;                                                                   
        context* owner;
                                                                                            // the starting index of this codeblock relative to its parent's starting index
        u32 begidx = 0;
                                                                                            // adds an instruction to this codeblock
        instruction* addinstruction(opcode, ssaid* args);
                                                                                            // adds a variable to this codeblock
        variable* addvariable(jitbox::type);
                                                                                            // adds a constant to this codeblock
        variable* addconstant(jitbox::constant);

        enum class cbtype : u8 {

                                                                                            // this codeblock is the top-level scope of a function
            function,                                                                       
                                                                                            // this codeblock is a child scope of a function
            child,                                                                          
                                                                                            // this codeblock contains an instruction (loop condition) and a child codeblock (loop body) 
            loop,                                                                           
                                                                                            // this codeblock contains an instruction (if condition), a child codeblock (if body), and an optional second child codeblock (else body)
            conditional                                                                     

        } blocktype;

        inline codeblock(cbtype blocktype);
        inline ~codeblock();

    };

    using pcodeblock = ptr<codeblock>;

    struct context {

        vec<pcodeblock> functions;
                                                                                            // current function being compiled within this context
        codeblock* curfunction = nullptr;                                                   
                                                                                            // current active scope within the current function
        codeblock* curscope = nullptr;                                                      
                                                                                            // current innermost active loop within the current function; might be nullptr
        codeblock* curloop = nullptr;

        static context* curcontext();

    };

    using pcontext = ptr<context>;

    /**
        below is a simple smart pointer implementation I wrote a long time ago before
        I knew that std smart pointers were a thing; I still use it just out of nostalgia
    **/

    template <int _>
    struct ptrcontainerbase {

        const void* p = nullptr;
        int refcount = 0;

        inline ptrcontainerbase(const void*);

    };

    using ptrcontainer = ptrcontainerbase<0>;

    template <typename ptrtype>
    struct ptr {

        template <typename casttype>
        inline ptr<casttype> cast();

        inline ptr();
        inline ptr(const ptr&);
        inline ptr(const ptrtype*);

        inline void operator = (const ptr&);
        inline void operator = (const ptrtype*);

        inline ptrtype* operator -> () const;
        inline ptrtype& operator * () const;

        inline ptrtype* operator () () const;

        inline bool operator == (const ptr&) const;
        inline bool operator == (ptrtype*) const;

        inline bool operator != (const ptr&) const;
        inline bool operator != (ptrtype*) const;

        inline bool operator >= (const ptr&) const;
        inline bool operator >= (ptrtype*) const;

        inline bool operator <= (const ptr&) const;
        inline bool operator <= (ptrtype*) const;

        inline bool operator > (const ptr&) const;
        inline bool operator > (ptrtype*) const;

        inline bool operator < (const ptr&) const;
        inline bool operator < (ptrtype*) const;

        inline  ~ptr();

    protected:

        ptrcontainer* ptrcnt = nullptr;

        inline ptr(ptrcontainer*, void*);

        inline void copy(const ptr&);
        inline void init(const ptrtype*);
        inline void cleanup();

        template <typename friendtype>
        friend struct ptr;

    };

    template <int _>
    inline ptrcontainerbase<_>::ptrcontainerbase(const void* p) : p(p) { }

    template <typename ptrtype>
    template <typename casttype>
    inline ptr<casttype> ptr<ptrtype>::cast() { return ptrcnt ? ptr<casttype>(ptrcnt, nullptr) : ptr<casttype>(); }

    template <typename ptrtype>
    inline ptr<ptrtype>::ptr() { init(nullptr); }

    template <typename ptrtype>
    inline ptr<ptrtype>::ptr(const ptr& ptr) { copy(ptr); }

    template <typename ptrtype>
    inline ptr<ptrtype>::ptr(const ptrtype* ptr) { init(ptr); }

    template <typename ptrtype>
    inline void ptr<ptrtype>::operator = (const ptr& ptr) { cleanup(); copy(ptr); }

    template <typename ptrtype>
    inline void ptr<ptrtype>::operator = (const ptrtype* ptr) { cleanup(); init(ptr); }

    template <typename ptrtype>
    inline ptrtype* ptr<ptrtype>::operator -> () const { return (ptrtype*)ptrcnt->p; }

    template <typename ptrtype>
    inline ptrtype& ptr<ptrtype>::operator * () const { return *(ptrtype*)(ptrcnt->p); }

    template <typename ptrtype>
    inline ptrtype* ptr<ptrtype>::operator () () const { if (ptrcnt) if (ptrcnt->p) return (ptrtype*)ptrcnt->p; return nullptr; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator == (const ptr& rh) const { return ptrcnt ? (rh.ptrcnt ? ptrcnt->p == rh.ptrcnt->p : false) : !rh.ptrcnt; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator == (ptrtype* rh) const { return ptrcnt ? ptrcnt->p == rh : !rh; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator != (const ptr& rh) const { return ptrcnt ? (rh.ptrcnt ? ptrcnt->p != rh.ptrcnt->p : true) : (bool)rh.ptrcnt; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator != (ptrtype* rh) const { return ptrcnt ? ptrcnt->p != rh : (bool)rh; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator >= (const ptr& rh) const {

        if (!ptrcnt && !rh.ptrcnt) return true;
        if (!ptrcnt && rh.ptrcnt) return false;
        if (ptrcnt && !rh.ptrcnt) return true;
        return ptrcnt->p >= rh.ptrcnt->p;

    }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator >= (ptrtype* rh) const { if (!ptrcnt && rh) return false; return ptrcnt->p >= (const void*)rh; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator <= (const ptr& rh) const { return rh >= *this; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator <= (ptrtype* rh) const { if (!ptrcnt && rh) return true; return ptrcnt->p <= (const void*)rh; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator > (const ptr& rh) const {

        if (!ptrcnt && !rh.ptrcnt) return false;
        if (!ptrcnt && rh.ptrcnt) return false;
        if (ptrcnt && !rh.ptrcnt) return true;
        return ptrcnt->p > rh.ptrcnt->p;

    }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator > (ptrtype* rh) const { if (!ptrcnt && rh) return false; return ptrcnt->p > (const void*)rh; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator < (const ptr& rh) const { return rh > *this; }

    template <typename ptrtype>
    inline bool ptr<ptrtype>::operator < (ptrtype* rh) const { if (!ptrcnt && rh) return true; return ptrcnt->p < (const void*)rh; }

    template <typename ptrtype>
    inline ptr<ptrtype>::~ptr() { cleanup(); }

    template <typename ptrtype>
    inline ptr<ptrtype>::ptr(ptrcontainer* ptrcnt, void*) : ptrcnt(ptrcnt) { ptrcnt->refcount++; }

    template <typename ptrtype>
    inline void ptr<ptrtype>::copy(const ptr& ptr) {

        if (ptr.ptrcnt == nullptr) { ptrcnt = nullptr; return; }

        ptrcnt = ptr.ptrcnt;
        ptrcnt->refcount++;
    }

    template <typename ptrtype>
    inline void ptr<ptrtype>::init(const ptrtype* ptr) {

        if (ptr) ptrcnt = new ptrcontainer(ptr);
        else ptrcnt = nullptr;

    }

    template <typename ptrtype>
    inline void ptr<ptrtype>::cleanup() {

        if (!ptrcnt) return;

        if (ptrcnt->refcount == 0) { if (ptrcnt->p) delete (const ptrtype*)ptrcnt->p; delete ptrcnt; }
        else ptrcnt->refcount--;

    }

}

namespace jitbox {
                                                                                            // the user should never directly create an instruction; this alias is just for readability
    using instruction = _jitbox::instruction*;

    constant constu8(u8 val);
    constant consti8(i8 val);
    constant constu16(u16 val);
    constant consti16(i16 val);
    constant constu32(u32 val);
    constant consti32(i32 val);
    constant constu64(u64 val);
    constant consti64(i64 val);
    constant constf32(f32 val);
    constant constf64(f64 val);

    struct var;

    struct context {
                                                                                            // if active is set to true, the newly constructed context will be set to the active context
        context(bool active = true);
                                                                                            // makes this context active
        void use() const;

        ~context();

    protected:
        
        _jitbox::pcontext ctx;
        static _jitbox::context* curcontext;

        friend struct var;
        friend struct _jitbox::context;

    };

    struct var : public printable {

        var(const var& v);
        var(const constant& c);
        var(instruction in);

        void operator = (const var& v);
        void operator = (const constant& c);
        void operator = (instruction in);

        type getdtype() const;
                                                                                            // returns the SSA id of this variable
        ssaid getssaid() const;

        string tostring(int alignment) const override;

    protected:

        _jitbox::variable* v;

        void assign(const var&);
        void assign(const constant&);
        void assign(instruction);

    };

    using varlist = vec<var>;

    struct function : public printable {
                                                                                            // makes this function the active function for code generation
        void use();
                                                                                            // returns the type id associated with this function
        type getdtype() const;
                                                                                            // returns the signature id of this function 
        id getid() const;
                                                                                            // sets the argument types for a function
        function& argtypes(const typelist&);
                                                                                            // sets the return type for a function
        function& rettype(type);

        string tostring(int alignment) const override;

    protected:

        _jitbox::codeblock* definition = nullptr;

        function(_jitbox::codeblock* definition, bool active);

    };
                                                                                            // exits the program with the specified code
    void terminate(var);
                                                                                            // returns from a void function
    void ret();
                                                                                            // returns a value from a function
    void ret(var);
                                                                                            // calls the specified function (with no arguments)
    instruction call(function func);
                                                                                            // calls the specified function with the specified arguments
    instruction call(function func, const varlist& args);
                                                                                            // casts src variable to the specified type
    instruction cast(var src, type to);
                                                                                            // computes the sum of the specified variables
    instruction add(var lh, var rh);
     /*                                                                                       // computes the difference of the specified variables
    instruction sub(var lh, var rh);
                                                                                            // computes the product of the specified variables
    instruction mul(var lh, var rh);
                                                                                            // computes the quotient of the specified variables
    instruction div(var lh, var rh);
                                                                                            // computes the division remainder of the specified variables
    instruction mod(var lh, var rh);
                                                                                            // computes the bitwise AND of the specified variables
    instruction bitwiseand(var lh, var rh);
                                                                                            // computes the bitwise OR of the specified variables
    instruction bitwiseor(var lh, var rh);
                                                                                            // computes the bitwise NOT of the specified variable
    instruction bitwisenot(var);
                                                                                            // computes whether the specified variable is zero; returns type::u8
    instruction zero(var);
                                                                                            // computes whether the specified variable is nonzero; returns type::u8
    instruction nonzero(var);
                                                                                            // computes the == operator on the specified variables; returns type::u8
    instruction equal(var lh, var rh);
                                                                                            // computes the != operator on the specified variables; returns type::u8
    instruction notequal(var lh, var rh);
                                                                                            // computes the > operator on the specified variables; returns type::u8
    instruction greaterthan(var lh, var rh);
                                                                                            // computes the >= operator on the specified variables; returns type::u8
    instruction greaterthanequal(var lh, var rh);
                                                                                            // computes the < operator on the specified variables; returns type::u8
    instruction lessthan(var lh, var rh);
                                                                                            // computes the <= operator on the specified variables; returns type::u8
    instruction lessthanequal(var lh, var rh);
                                                                                            // computes the left bit shift on the specified variables
    instruction bitshiftleft(var lh, var rh);
                                                                                            // computes the right bit shift on the specified variables
    instruction bitshiftright(var lh, var rh);
                                                                                            // computes the left bit rotation on the specified variables
    instruction bitrotateleft(var lh, var rh);
                                                                                            // computes the right bit rotation on the specified variables
    instruction bitrotateright(var lh, var rh);  
                                                                                            // gets argument of the current function by the specified index
    var getarg(int index);*/
                                                                                            // switches the context to a new function codeblock
    function makefunction();

    struct makeif {                                                                         // creates an if codeblock within the current context

        makeif();
                                                                                            // sets the condition for the if codeblock just created; argument must be type::u8
        makeif& condition(instruction);

    };

                                                                                            // creates an else codeblock within the current context
    void makeelse();

    struct makewhile {

                                                                                            // creates a while loop codeblock within the current context
        makewhile();

                                                                                            // sets the condition for the while loop codeblock just created; argument must be type::u8
        makewhile& condition(instruction);

    };

                                                                                            // ends the current codeblock being generated
    void end();

                                                                                            // defines a global variable within the current context; example usage: var example = makeglobalvar(constu8(1));
    var makeglobalvar(constant ctc);

                                                                                            // compiles the current context
    void compile();

}
/*
#include <cstring>
#include "./internal/context.hpp"
#include "./internal/jitboxinternal.hpp"

#define JITBOX_NUMREGARGS 4

namespace jitbox {

                    // represents a high-level instruction
    struct instruction : public printable {

        inline instruction(internalinstruction* in) : definition(in) { }

        inline type dtype() const { return definition->dtype; }

        internalinstruction* definition = nullptr;

        string tostring(int alignment) const override { return definition->tostring(alignment); }

    };

                    // represents a SSA variable
    struct var : public printable {

        inline var() { }
        inline var(const var& v) { copy(v); }
        inline var(instruction);
        inline var(constant);
        inline var(internalvariable*);
        inline void operator=(const var& v) { copy(v); }
        inline void operator=(instruction);
        inline void operator=(constant);
        inline void operator=(internalvariable*) = delete;

        inline type dtype() const { return definition->dtype; }

        internalvariable* definition = nullptr;

        void copy(const var&);

        string tostring(int alignment) const override { return definition->tostring(alignment); }

    };

    using varlist = vec<var*>;

    struct varproxy { var v; inline varproxy(instruction in) : v(in) { } inline varproxy(constant c) : v(c) { } inline varproxy(internalvariable* iv) : v(iv) { } };

                    // represents a block of code, such as a function, the body of a while loop, etc.
    struct codeblock : public printable {

        inline codeblock(functioncodeblock* definition) : definition(definition) { }

                        // makes this codeblock the active codeblock for code generation
        void makecurrent();

                        // returns the signature for this function
        inline signature& getsignature();

                        // returns the number of arguments accepted by this function
        inline int getnumargs();

                        // sets the argument types for a function
        inline codeblock& argtypes(const typelist&);

                        // sets the return type for a function
        inline codeblock& rettype(type);

                        // function will always be inlined
        inline codeblock& makeinline();

                        // function will always pass the specified arguments by reference
        inline codeblock& passbyref(const vec<bool>& args);

                        // function will always return a reference
        inline codeblock& returnref();

        functioncodeblock* definition = nullptr;

        string tostring(int alignment) const override { return definition->tostring(alignment); }

    };

                    // exits the program with code 0
    inline void terminate();

                    // returns from a void function
    inline void ret();

                    // returns a variable from a function
    inline void ret(var&); inline void ret(varproxy v) { ret(v.v); }

                    // calls the specified function (with no arguments)
    inline instruction call(codeblock& func);

                    // calls the specified function with the specified arguments
    inline instruction call(codeblock& func, const varlist& args);

                    // casts src variable to the specified type
    inline instruction cast(var& src, type to); inline instruction cast(varproxy lh, type to) { return cast(lh.v, to); }

                    // computes the sum of the specified variables
    inline instruction add(var& lh, var& rh); inline instruction add(varproxy lh, var& rh) { return add(lh.v, rh); } inline instruction add(var& lh, varproxy rh) { return add(lh, rh.v); } inline instruction add(varproxy lh, varproxy rh) { return add(lh.v, rh.v); }

                    // computes the difference of the specified variables
    inline instruction sub(var& lh, var& rh); inline instruction sub(varproxy lh, var& rh) { return sub(lh.v, rh); } inline instruction sub(var& lh, varproxy rh) { return sub(lh, rh.v); } inline instruction sub(varproxy lh, varproxy rh) { return sub(lh.v, rh.v); }

                    // computes the product of the specified variables
    inline instruction mul(var& lh, var& rh); inline instruction mul(varproxy lh, var& rh) { return mul(lh.v, rh); } inline instruction mul(var& lh, varproxy rh) { return mul(lh, rh.v); } inline instruction mul(varproxy lh, varproxy rh) { return mul(lh.v, rh.v); }

                    // computes the quotient of the specified variables
    inline instruction div(var& lh, var& rh); inline instruction div(varproxy lh, var& rh) { return div(lh.v, rh); } inline instruction div(var& lh, varproxy rh) { return div(lh, rh.v); } inline instruction div(varproxy lh, varproxy rh) { return div(lh.v, rh.v); }

                    // computes the division remainder of the specified variables
    inline instruction mod(var& lh, var& rh); inline instruction mod(varproxy lh, var& rh) { return mod(lh.v, rh); } inline instruction mod(var& lh, varproxy rh) { return mod(lh, rh.v); } inline instruction mod(varproxy lh, varproxy rh) { return mod(lh.v, rh.v); }

                    // computes the bitwise AND of the specified variables
    inline instruction bitwiseand(var& lh, var& rh); inline instruction bitwiseand(varproxy lh, var& rh) { return bitwiseand(lh.v, rh); } inline instruction bitwiseand(var& lh, varproxy rh) { return bitwiseand(lh, rh.v); } inline instruction bitwiseand(varproxy lh, varproxy rh) { return bitwiseand(lh.v, rh.v); }

                    // computes the bitwise OR of the specified variables
    inline instruction bitwiseor(var& lh, var& rh); inline instruction bitwiseor(varproxy lh, var& rh) { return bitwiseor(lh.v, rh); } inline instruction bitwiseor(var& lh, varproxy rh) { return bitwiseor(lh, rh.v); } inline instruction bitwiseor(varproxy lh, varproxy rh) { return bitwiseor(lh.v, rh.v); }

                    // computes the bitwise NOT of the specified variable
    inline instruction bitwisenot(var&); inline instruction bitwisenot(varproxy v) { return bitwisenot(v.v); }

                    // computes whether the specified variable is zero (type::u8 is used as a boolean type)
    inline instruction zero(var&); inline instruction zero(varproxy v) { return zero(v.v); }

                    // computes whether the specified variable is nonzero (type::u8 is used as a boolean type)
    inline instruction notzero(var&); inline instruction notzero(varproxy v) { return notzero(v.v); }

                    // computes the == operator on the specified variables (type::u8 is used as a boolean type)
    inline instruction equal(var& lh, var& rh); inline instruction equal(varproxy lh, var& rh) { return equal(lh.v, rh); } inline instruction equal(var& lh, varproxy rh) { return equal(lh, rh.v); } inline instruction equal(varproxy lh, varproxy rh) { return equal(lh.v, rh.v); }

                    // computes the != operator on the specified variables (type::u8 is used as a boolean type)
    inline instruction notequal(var& lh, var& rh); inline instruction notequal(varproxy lh, var& rh) { return notequal(lh.v, rh); } inline instruction notequal(var& lh, varproxy rh) { return notequal(lh, rh.v); } inline instruction notequal(varproxy lh, varproxy rh) { return notequal(lh.v, rh.v); }

                    // computes the > operator on the specified variables (type::u8 is used as a boolean type)
    inline instruction greaterthan(var& lh, var& rh); inline instruction greaterthan(varproxy lh, var& rh) { return greaterthan(lh.v, rh); } inline instruction greaterthan(var& lh, varproxy rh) { return greaterthan(lh, rh.v); } inline instruction greaterthan(varproxy lh, varproxy rh) { return greaterthan(lh.v, rh.v); }

                    // computes the >= operator on the specified variables (type::u8 is used as a boolean type)
    inline instruction greaterthanequal(var& lh, var& rh); inline instruction greaterthanequal(varproxy lh, var& rh) { return greaterthanequal(lh.v, rh); } inline instruction greaterthanequal(var& lh, varproxy rh) { return greaterthanequal(lh, rh.v); } inline instruction greaterthanequal(varproxy lh, varproxy rh) { return greaterthanequal(lh.v, rh.v); }

                    // computes the < operator on the specified variables (type::u8 is used as a boolean type)
    inline instruction lessthan(var& lh, var& rh); inline instruction lessthan(varproxy lh, var& rh) { return lessthan(lh.v, rh); } inline instruction lessthan(var& lh, varproxy rh) { return lessthan(lh, rh.v); } inline instruction lessthan(varproxy lh, varproxy rh) { return lessthan(lh.v, rh.v); }

                    // computes the <= operator on the specified variables (type::u8 is used as a boolean type)
    inline instruction lessthanequal(var& lh, var& rh); inline instruction lessthanequal(varproxy lh, var& rh) { return lessthanequal(lh.v, rh); } inline instruction lessthanequal(var& lh, varproxy rh) { return lessthanequal(lh, rh.v); } inline instruction lessthanequal(varproxy lh, varproxy rh) { return lessthanequal(lh.v, rh.v); }

                    // computes a left bit shift on the specified variables
    inline instruction bitshiftleft(var& lh, var& rh); inline instruction bitshiftleft(varproxy lh, var& rh) { return bitshiftleft(lh.v, rh); } inline instruction bitshiftleft(var& lh, varproxy rh) { return bitshiftleft(lh, rh.v); } inline instruction bitshiftleft(varproxy lh, varproxy rh) { return bitshiftleft(lh.v, rh.v); }

                    // computes a right bit shift on the specified variables
    inline instruction bitshiftright(var& lh, var& rh); inline instruction bitshiftright(varproxy lh, var& rh) { return bitshiftright(lh.v, rh); } inline instruction bitshiftright(var& lh, varproxy rh) { return bitshiftright(lh, rh.v); } inline instruction bitshiftright(varproxy lh, varproxy rh) { return bitshiftright(lh.v, rh.v); }

                    // computes a left bit rotation on the specified variables
    inline instruction bitrotateleft(var& lh, var& rh); inline instruction bitrotateleft(varproxy lh, var& rh) { return bitrotateleft(lh.v, rh); } inline instruction bitrotateleft(var& lh, varproxy rh) { return bitrotateleft(lh, rh.v); } inline instruction bitrotateleft(varproxy lh, varproxy rh) { return bitrotateleft(lh.v, rh.v); }

                    // computes a right bit rotation on the specified variables
    inline instruction bitrotateright(var& lh, var& rh); inline instruction bitrotateright(varproxy lh, var& rh) { return bitrotateright(lh.v, rh); } inline instruction bitrotateright(var& lh, varproxy rh) { return bitrotateright(lh, rh.v); } inline instruction bitrotateright(varproxy lh, varproxy rh) { return bitrotateright(lh.v, rh.v); }

                    // gets argument of the current function by the specified index; may only be used in the RH side of assignments
    inline internalvariable* getarg(int index);

                    // switches the context to a new function codeblock
    inline codeblock makefunction();

    struct makeif{

                        // creates an if codeblock within the current context
        inline makeif() { }

                        // sets the condition for the if codeblock just created
        inline makeif& condition(instruction);
    
    };

                    // creates an else codeblock within the current context
    inline void makeelse();

    struct makewhile{
        
                        // creates a while loop codeblock within the current context
        inline makewhile() { context::curcontext->makewhilehook = true; }

                        // sets the condition for the while loop codeblock just created
        inline makewhile& condition(instruction);
    
    };

                    // ends the current codeblock being generated
    inline void end();

                    // defines a global variable within the current context -- example usage: var example = makeglobalvar(constu8(1));
    inline internalvariable* makeglobalvar(constant ctc) { pinternalvariable newvar = new internalvariable(ctc, true); context::curcontext->globalvars.push_back(newvar); return newvar(); }

    inline void compile();

}

#define _makevarconstructorbody(ivarg) \
\
auto& ctxt = *context::curcontext; \
\
pinternalvariable newvar = new internalvariable(ivarg); \
ctxt.currentcode->addobject(newvar.cast<compilerobject>()); \
\
definition = newvar()

inline jitbox::var::var(instruction in) { _makevarconstructorbody(in.definition); }

inline jitbox::var::var(constant c) { _makevarconstructorbody(c); }

inline jitbox::var::var(internalvariable* iv) { definition = iv; }

inline void jitbox::var::operator=(instruction in) {

    auto& ctxt = *context::curcontext;

    if (definition && definition->isglobal) {

        in.definition->lh = definition;
        definition->rh = in.definition;

        return;

    }
   
    bool fromparentscope = definition->owner->isparentof(ctxt.currentcode);
   
    if (fromparentscope) {
        
        definition->value.isconst = false;
       
        auto inst = internalinstruction::makeinstruction(ctxt, ctxt.currentcode->idxcount, definition->dtype, 1, inst::alias, definition);
        auto instobj = inst.cast<compilerobject>();
       
        auto curcode = ctxt.currentcode;
        int curcodesize = curcode->code->size() - 1;
        auto tmp = curcode->code->operator[](curcodesize);

        curcode->code->resize(curcodesize);
        curcode->idxcount--;
        curcode->addobject(instobj);
        curcode->addobject(tmp);
       
        pinternalvariable newvar = new internalvariable(inst());
        newvar->owner = definition->owner;
        newvar->alias = definition->alias ? definition->alias : definition;
       
        ctxt.currentcode->addobject(newvar.cast<compilerobject>());
        definition = newvar();
       
        definition->rh = in.definition;
        in.definition->lh = definition;
       
    } else { _makevarconstructorbody(in.definition); }

}

inline void jitbox::var::operator=(constant ctc) {

    auto& ctxt = *context::curcontext;

    if (definition && definition->isglobal) {

        var newconst = ctc;
        auto inst = internalinstruction::makeinstruction(ctxt, ctxt.currentcode->idxcount, definition->dtype, 2, inst::mov, newconst.definition, definition);

        return;
        
    }
   
    bool fromparentscope = definition->owner->isparentof(ctxt.currentcode);
   
    if (fromparentscope) {
        
        definition->value.isconst = false;
       
        auto inst = internalinstruction::makeinstruction(ctxt, ctxt.currentcode->idxcount, definition->dtype, 1, inst::alias, definition);
       
        pinternalvariable newvar = new internalvariable(inst());
        newvar->owner = definition->owner;
        newvar->alias = definition->alias ? definition->alias : definition;
       
        ctxt.currentcode->addobject(inst.cast<compilerobject>());
        ctxt.currentcode->addobject(newvar.cast<compilerobject>());

        pinternalvariable ctcv = new internalvariable(ctc);
        inst = internalinstruction::makeinstruction(ctxt, ctxt.currentcode->idxcount, definition->dtype, 2, inst::mov, ctcv(), newvar());
        
        ctxt.currentcode->addobject(ctcv.cast<compilerobject>());
        ctxt.currentcode->addobject(inst.cast<compilerobject>());

        definition = newvar();
       
    } else { _makevarconstructorbody(ctc); }

}

inline void jitbox::var::copy(const var& v) {  

    auto& ctxt = *context::curcontext;

    if (definition && definition->isglobal) {

        auto inst = internalinstruction::makeinstruction(ctxt, ctxt.currentcode->idxcount, definition->dtype, 2, inst::mov, v.definition, definition);
        ctxt.currentcode->addobject(inst.cast<compilerobject>());

        return;

    }
   
    bool fromparentscope = definition ? definition->owner->isparentof(ctxt.currentcode) : false;

    if (fromparentscope) {
        
        definition->value.isconst = false;

        auto inst = internalinstruction::makeinstruction(ctxt, ctxt.currentcode->idxcount, definition->dtype, 1, inst::alias, definition);
        auto instobj = inst.cast<compilerobject>();

        ctxt.currentcode->addobject(instobj);

        pinternalvariable newvar = new internalvariable(inst());
        newvar->owner = definition->owner;
        newvar->alias = definition->alias ? definition->alias : definition;

        inst = internalinstruction::makeinstruction(ctxt, ctxt.currentcode->idxcount, v.definition->dtype, 1, inst::mov, v.definition);
        instobj = inst.cast<compilerobject>();

        newvar->rh = inst();
        inst->lh = newvar();

        ctxt.currentcode->addobject(instobj);
        ctxt.currentcode->addobject(newvar.cast<compilerobject>());
        
        definition = newvar();

    }

    else {
        
        auto inst = internalinstruction::makeinstruction(ctxt, ctxt.currentcode->idxcount, v.definition->dtype, 1, inst::mov, v.definition);
        pinternalvariable newvar = new internalvariable(inst());

        ctxt.currentcode->addobject(inst.cast<compilerobject>());
        ctxt.currentcode->addobject(newvar.cast<compilerobject>());

        if (!v.definition->isglobal) v.definition->successor = newvar();
        definition = newvar();

    }

}

inline void jitbox::codeblock::makecurrent() { this->definition->makecurrent(); }

inline jitbox::signature& jitbox::codeblock::getsignature() { return definition->sig; }

inline int jitbox::codeblock::getnumargs() { return definition->args.size(); }

inline jitbox::codeblock& jitbox::codeblock::argtypes(const typelist& types) { definition->sig.types = types; definition->genargs(); return *this; }

inline jitbox::codeblock& jitbox::codeblock::rettype(type t) { definition->sig.returntype = t.getid(); return *this; }

inline jitbox::codeblock& jitbox::codeblock::makeinline() { definition->isinline = true; return *this; }

inline jitbox::codeblock& jitbox::codeblock::passbyref(const vec<bool>& args) { definition->passbyref = args; return *this; }

inline jitbox::codeblock& jitbox::codeblock::returnref() { definition->returnref = true; return *this; }

#define _instructiontypecheck(sig) \
if (!lh.dtype().isconcrete()) throw exception{ sig " was called with abstract type " + std::to_string(lh.dtype().getid()) }; \
if (lh.dtype().getclass() != type::primitive) throw exception{ sig " was called with non-primitive type " + std::to_string(lh.dtype().getid()) }

#define _instructiontypecheckbinop(sig) \
if (lh.dtype().getid() != rh.dtype().getid()) throw exception{ sig " was called with mismatching types, " + std::to_string(lh.dtype().getid()) + " and " + std::to_string(rh.dtype().getid()) }; \
_instructiontypecheck(sig)

#define _instructionintcheck(sig) \
if (!lh.dtype().isinteger()) throw exception{ sig " was called with non-integer type " + std::to_string(lh.dtype().getid()) }

inline void jitbox::terminate() {

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::nothing, 0, inst::exit);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());

}

inline void jitbox::ret() {

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::nothing, 0, inst::retv);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());

}

inline void jitbox::ret(var& v) {

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::nothing, 1, inst::ret, v.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());

}

inline jitbox::instruction jitbox::call(codeblock& func) {

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;

    var calldest = constu32(func.getsignature().identifier);

    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, func.definition->sig.returntype, 1, inst::call, calldest.definition);
    ctxt.currentcode->addobject(inst.cast<compilerobject>());

    auto& curfunc = *ctxt.currentfunction;

    if (!curfunc.firstfunctioncall) curfunc.firstfunctioncall = inst();
    if (curfunc.lastfunctioncall) curfunc.lastfunctioncall->link = inst();
    curfunc.lastfunctioncall = inst();

    return instruction(inst());

}

inline jitbox::instruction jitbox::call(codeblock& func, const varlist& args) {

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;

    for (auto arg : args) {

        auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::nothing, 1, inst::farg, arg->definition);
        ctxt.currentcode->addobject(inst.cast<compilerobject>());

    }

    var calldest = constu32(func.getsignature().identifier);

    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, func.definition->sig.returntype, 1, inst::call, calldest.definition);
    ctxt.currentcode->addobject(inst.cast<compilerobject>());

    int ireg = 0;
    int freg = 0;
    int loopend = args.size();
    if (loopend > JITBOX_NUMREGARGS) loopend = JITBOX_NUMREGARGS;

    for (int i = 0; i < loopend; i++) {

        auto arg = args[i];
        int& reg = arg->dtype().isfp() ? freg : ireg;

        if (!arg->definition->isglobal && arg->definition->reghint < 0) arg->definition->reghint = reg;
        reg++;

    }

    auto& curfunc = *ctxt.currentfunction;

    if (!curfunc.firstfunctioncall) curfunc.firstfunctioncall = inst();
    if (curfunc.lastfunctioncall) curfunc.lastfunctioncall->link = inst();
    curfunc.lastfunctioncall = inst();

    return instruction(inst());

}

inline jitbox::instruction jitbox::cast(var& lh, type to) {

    _instructiontypecheck("cast(var, type)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, to, 1, inst::cast, lh.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::add(var& lh, var& rh) {
    
    _instructiontypecheckbinop("add(var, var)"); 
    
    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::add, lh.definition, rh.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::sub(var& lh, var& rh) {
    
    _instructiontypecheckbinop("sub(var, var)"); 
    
    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::sub, lh.definition, rh.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::mul(var& lh, var& rh) {
    
    _instructiontypecheckbinop("mul(var, var)"); 
    
    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::mul, lh.definition, rh.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::div(var& lh, var& rh) {

    _instructiontypecheckbinop("div(var, var)"); 
    
    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::div, lh.definition, rh.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::mod(var& lh, var& rh) {
    
    _instructiontypecheckbinop("mod(var, var)"); 
    _instructionintcheck("mod(var, var)");
    
    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::mod, lh.definition, rh.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::bitwiseand(var& lh, var& rh) {
    
    _instructiontypecheckbinop("bitwiseand(var, var)"); 
    _instructionintcheck("bitwiseand(var, var)");
    
    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::band, lh.definition, rh.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::bitwiseor(var& lh, var& rh) {
    
    _instructiontypecheckbinop("bitwiseor(var, var)"); 
    _instructionintcheck("bitwiseor(var, var)");
    
    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::bor, lh.definition, rh.definition);
    
    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::bitwisenot(var& lh) {

    _instructiontypecheck("bitwisenot(var)");
    _instructionintcheck("bitwisenot(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 1, inst::bnot, lh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::zero(var& lh) {

    _instructiontypecheck("zero(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::u8, 1, inst::zero, lh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::notzero(var& lh) {

    _instructiontypecheck("notzero(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::u8, 1, inst::nzero, lh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::equal(var& lh, var& rh) {

    _instructiontypecheckbinop("equal(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::u8, 2, inst::eq, lh.definition, rh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::notequal(var& lh, var& rh) {

    _instructiontypecheckbinop("notequal(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::u8, 2, inst::neq, lh.definition, rh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::greaterthan(var& lh, var& rh) {

    _instructiontypecheckbinop("greaterthan(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::u8, 2, inst::gt, lh.definition, rh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::greaterthanequal(var& lh, var& rh) {

    _instructiontypecheckbinop("greaterthanequal(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::u8, 2, inst::gte, lh.definition, rh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::lessthan(var& lh, var& rh) {

    _instructiontypecheckbinop("lessthan(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::u8, 2, inst::gt, rh.definition, lh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::lessthanequal(var& lh, var& rh) {

    _instructiontypecheckbinop("lessthanequal(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, type::u8, 2, inst::gte, rh.definition, lh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::bitshiftleft(var& lh, var& rh) {

    _instructiontypecheckbinop("bitshiftleft(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::shl, lh.definition, rh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::bitshiftright(var& lh, var& rh) {

    _instructiontypecheckbinop("bitshiftright(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::shr, lh.definition, rh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::bitrotateleft(var& lh, var& rh) {

    _instructiontypecheckbinop("bitrotateleft(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::rotl, lh.definition, rh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::instruction jitbox::bitrotateright(var& lh, var& rh) {

    _instructiontypecheckbinop("bitrotateright(var)");

    auto& ctxt = *context::curcontext;
    auto nextidx = ctxt.currentcode->idxcount;
    auto inst = internalinstruction::makeinstruction(ctxt, nextidx, lh.dtype(), 2, inst::rotr, lh.definition, rh.definition);

    ctxt.currentcode->addobject(inst.cast<compilerobject>());
    return instruction(inst());

}

inline jitbox::internalvariable* jitbox::getarg(int index) {

    auto& ctxt = *context::curcontext;
    auto curfunc = ctxt.currentfunction;

    return &curfunc->args[index];

}

inline jitbox::codeblock jitbox::makefunction() {

    auto newfunc = new functioncodeblock();
    pinternalcodeblock func = newfunc;

    auto& ctxt = *context::curcontext;

    ctxt.code.push_back(func);
    func->makecurrent();

    return codeblock(newfunc);

}

inline jitbox::makeif& jitbox::makeif::condition(instruction condition) {

    pinternalcodeblock newcond = new conditionalcodeblock();

    auto& ctxt = *context::curcontext;
    auto curcode = ctxt.currentcode;

    curcode->addobject(newcond.cast<compilerobject>());

    curcode->curchild = newcond();
    newcond->parent = curcode;

    curcode = newcond();

    pinternalcodeblock newif = new ifcodeblock(condition.definition);
    curcode->addobject(newif.cast<compilerobject>());

    curcode->curchild = newif();
    newif->parent = curcode;

    ctxt.currentcode = newif();
    condition.definition->isjump = true;

    return *this;

}

inline void jitbox::makeelse() {

    auto& ctxt = *context::curcontext;
    auto curcond = ctxt.currentcode;

    ((ifcodeblock*)curcond)->haselse = true;
    curcond = curcond->parent;

    pinternalcodeblock newelse = new elsecodeblock();
    curcond->addobject(newelse.cast<compilerobject>());

    curcond->curchild = newelse();
    newelse->parent = curcond;

    ctxt.currentcode = newelse();

}

inline jitbox::makewhile& jitbox::makewhile::condition(instruction condition) {

    auto& ctxt = *context::curcontext;
    auto curcode = ctxt.currentcode;

    pinternalcodeblock newwhile = new whilecodeblock(condition.definition);
    curcode->addobject(newwhile.cast<compilerobject>());

    curcode->curchild = newwhile();
    newwhile->parent = curcode;

    ctxt.currentcode = newwhile();
    ctxt.currentloop = newwhile.cast<whilecodeblock>()();
    condition.definition->isjump = true;

    ctxt.whilehook->forwardhook = newwhile.cast<whilecodeblock>()();

    return *this;

}

#define _updatelrend(iv, newidx) { if (!iv->isglobal)  { if (iv->alias) iv->alias->lrend = newidx; else iv->lrend = newidx; } }

inline void jitbox::end() {

    auto& ctxt = *context::curcontext;
    auto curcode = ctxt.currentcode;

    bool doublescoped = curcode->ctype == internalcodeblock::ifblock || curcode->ctype == internalcodeblock::elseblock;

    ctxt.currentcode = curcode->parent;
    if (curcode->parent) curcode->parent->curchild = nullptr;
    
    ctxt.currentloop = nullptr;
    auto p = curcode->parent;

    while (p) {

        if (p->ctype == internalcodeblock::whileblock) { ctxt.currentloop = (whilecodeblock*)p; break; }
        p = p->parent;

    }

    auto nextidx = curcode->idxcount;

    if (curcode->ctype == internalcodeblock::whileblock) {

        for (auto iv : ((whilecodeblock*)curcode)->loopvars) {

            _updatelrend(iv, nextidx - 1)
            if (iv->owner->isparentof(ctxt.currentloop)) ctxt.currentloop->loopvars.push_back(iv);

        }

    }

    if (curcode->parent) curcode->parent->idxcount = nextidx;
    
    if (doublescoped) end();

}

inline void jitbox::compile() {

    auto& ctxt = *context::curcontext;
    internalcompiler icomp(ctxt);

}

template <typename... Args>
inline void jitbox::internalinstruction::processinstruction(internalinstruction* in, int nextidx, int idx, internalvariable* v, Args... varargs) {

    auto& ctxt = *context::curcontext;

    if (v->owner->isparentof(ctxt.currentloop)) ctxt.currentloop->loopvars.push_back(v);

    else {

        auto nextidx = ctxt.currentcode->idxcount;
        if (nextidx > v->lrend) _updatelrend(v, nextidx)

    }

    in->args[idx] = v;
    processinstruction(in, nextidx, idx + 1, varargs...);

}

*/

#endif