#ifndef JITBOX_HPP
#define JITBOX_HPP

#include <utility>
#include <cstdint>
#include <vector>
#include <set>
#include <map>
#include <string>
#include <cstring>

// this file defines both the public and private jitbox api
//// namespace jitbox ==> public api
//// namespace _internal ==> private internal api

#define JITBOX_DEBUG

namespace jitbox {

    using U8 = uint8_t;
    using I8 = int8_t;
    using U16 = uint16_t;
    using I16 = int16_t;
    using U32 = uint32_t;
    using I32 = int32_t;
    using U64 = uint64_t;
    using I64 = int64_t;
    using F32 = float;
    using F64 = double;
    using Bool = bool;
    using CString = const char*;

    template <typename T>
    using Vec = std::vector<T>;

    template <typename T>
    using Set = std::set<T>;

    template <typename K, typename V>
    using Map = std::map<K, V>;

    using String = std::string;
                                                                                            // every type is assigned a unique integer id
    using ID = I32;   
                                                                                            // every variable gets an SSA id value that is unique within its function if it is local (assigned positive numbers), or unique within its context if it is global (assigned negative numbers); zero is a reserved value that will never be used as an SSA id
    using SSAID = I32;
                                                                                            // 8-bit error code
    using ErrorCode = U8;                                                                    
                                                                                            // used for pretty printing jitbox data
    struct Printable {                                                                      

        String print(U8 alignment = 0, Bool addNewline = true) const;      

        virtual ~Printable();

        virtual String toString(U8 alignment) const = 0;

        static String indent(U8);

        static String pad(String, U16);

    };

#ifdef JITBOX_DEBUG

    struct Error {

        enum : ErrorCode{     
                                                                                            // no error
            OK,                                                                
                                                                                            // a type constructor was called using an invalid id
            INVALID_TYPE_ID,
                                                                                            // a getter was called on a type for the wrong type class
            TYPE_GETTER_MISMATCH,
                                                                                            // a jitbox function was called that requires an active context, but no context was active
            NO_ACTIVE_CONTEXT,
                                                                                            // a jitbox function was called that requires an active function, but no function was active within the current active context
            NO_ACTIVE_FUNCTION,
                                                                                            // a jitbox function was called that requires an active scope, but no scope was active within the current active function
            NO_ACTIVE_SCOPE,
                                                                                            // a variable was assigned a second value in global scope
            GLOBAL_REDECLARATION,
                                                                                            // an argument was passed to an instruction with a type that the instruction does not support
            INVALID_ARGUMENT,
                                                                                            // there was an attempt to generate an instruction with incompatible operand types
            TYPE_MISMATCH,
                                                                                            // the same instruction was used as the RH in two different assignments
            INVALID_ASSIGNMENT,
                                                                                            // a jitbox object from a different context was used
            CONTEXT_MISMATCH

        };
        
        ErrorCode code;

        Error(ErrorCode code);
        
    };

#endif

    struct Type; struct StructureSignature; struct FunctionSignature;

    namespace _internal { 

        using StructureData = std::pair<ID, StructureSignature>;
        using FunctionData = FunctionSignature;
        
        const U8 numDerivedTypeClasses = 5;
        const U8 numPrimitiveTypes = 10;
                                                                                            // used internally to represent union types
        struct CanonicalTypeSet {
                                                                                            // union types are not counted for the purpose of this data structure, hence the minus one
            const static U8 numDerivedTypes = numDerivedTypeClasses - 1;

            Type derivedTypes[numDerivedTypes];

            Bool primitiveTypes[numPrimitiveTypes];

            Set<Type> structureTypes;

            Bool signatureTypesNegated = false;

            CanonicalTypeSet();

            CanonicalTypeSet(const CanonicalTypeSet&);
                                                                                            // allows usage in ordered maps
            Bool operator < (const CanonicalTypeSet&);

            void merge(Type);

            void merge(const CanonicalTypeSet&);

            void intersect(Type);

            void intersect(const CanonicalTypeSet&);

            void negate();

        };
        
    }
                                                                                            // creates a 1-1 mapping between integer ids and subsets of the type space
    struct Type : public Printable {	
                                                                                            // ordered list of types; repeats allowed
        using List = Vec<Type>;
                                                                                            // unordered set of types; repeats not counted
        using Set = Set<Type>;

        enum : ID {
                                                                                            // used for void functions
            NOTHING,                                                                        
                                                                                            // used for creating abstract types
            ANYTHING,                                                                       
                                                                                            // abstract primitive type
            PRIMITIVE,                                                          
                                                                                            // concrete primitive type
            I8, I16, I32, I64, U8, U16, U32, U64, F32, F64,                                 
                                                                                            // abstract derived types
            POINTER, ARRAY, STRUCTURE, FUNCTION,
                                                                                            // abstract type representing a union of multiple types
            UNION

        };
                                                                                            // default constructor; contains type::nothing
        Type();                                                                         
                                                                                            // basic constructor; will throw an error if the id supplied is invalid and JITBOX_DEBUG is defined
        Type(ID);

        Type(const Type&) = default;
                                                                                            // assigns an id to a tuple type based on the list of types provided
        static Type Tuple(const List&);
                                                                                            // assigns an id to a pointer type based on the type that is being pointed to
        static Type Pointer(Type);
                                                                                            // assigns an id to an array type based on the type that the array contains
        static Type Array(Type);
                                                                                            // assigns an id to a function type
        static Type Function(const FunctionSignature&); 
                                                                                            // assigns an id to a function type; throws an exception if arguments is not a tuple type      
        static Type Function(Type arguments, Type returnType); 
                                                                                            // assigns an id to a function type        
        static Type Function(const List& arguments, Type returnType);
                                                                                            // assigns an id to a union type based on the given typelist
        static Type Any(const List&); static Type Or(Type, Type);
                                                                                            // assigns an id to an intersection type based on the given typelist
        static Type All(const List&); static Type And(Type, Type);
                                                                                            // assigns an id to a negation type based on the given type
        static Type Not(Type);
                                                                                            // returns a list of the members of this type; throws an exception if not a tuple type
        const List& members() const;
                                                                                            // returns the type that is being pointed to; throws an exception if not a pointer type
        Type pointsTo() const;
                                                                                            // returns the type that the array contains; throws an exception if not an array type
        Type contains() const;
                                                                                            // returns the structure signature for this type; throws an exception if not a structure type
        const StructureSignature& getStructureSignature() const;
                                                                                            // returns the function signature for this type; throws an exception if not a function type
        const FunctionSignature& getFunctionSignature() const;

        Type Or(Type);

        Type And(Type);

        Type Not();
                                                                                            // semantic type comparison, evaluates subset relationship between two subsets of the type space
        Bool is(Type) const;

        ID getID() const;
                                                                                            // returns either nothing, anything, primitive, pointer, array, structure, or function
        ID getClass() const;

        Bool isConcrete() const;

        Bool isAbstract() const;

        Bool isInteger() const;

        Bool isUnsigned() const;

        Bool isFP() const;
                                                                                            // number of bytes that an object of this type takes up in memory, returns -1 if the type is abstract or the size is not known at compile time
        jitbox::I32 numBytes() const;

    protected:

        ID identifier = NOTHING;
        ID typeClass = NOTHING;

                                                                                            // assigns an id to a structure type
        static Type Structure(const StructureSignature&);

        void determineClass();

        String toString(jitbox::U8) const override;

    };

    Bool operator == (const Type&, ID);
    Bool operator == (const Type&, const Type&);
    Bool operator == (ID, const Type&);

    Bool operator != (const Type&, ID);
    Bool operator != (const Type&, const Type&);
    Bool operator != (ID, const Type&);
                                                                                            // needed in order to use an ordered map
    Bool operator < (const Type&, const Type&);

    namespace _internal {

        const U8 firstPrimitive = Type::I8;
        const U8 firstDerivedTypeClass = Type::POINTER;

    }

    struct Constant : public Printable {

        union {

            U8 vU8;
            I8 vI8;
            U16 vU16;
            I16 vI16;
            U32 vU32;
            I32 vI32;
            U64 vU64;
            I64 vI64;
            F32 vF32;
            F64 vF64;

        };

        Type type;
                                                                                            // if this variable is false, this object is treated as garbage and its value is ignored
        Bool isConst = false;

        Bool operator == (const Constant&);

        Bool operator != (const Constant& ctc);

        String toString(U8) const override;

    };

    struct Signature : public Printable {

        Bool operator < (const Signature&) const;

        String toString(U8) const override;

    protected:

        Signature(Type, Type, bool);

        Type t1, t2;
        bool isFunction;

    };

    struct StructureSignature : public Signature {

        const Type::Set& getParents() const;

        Type getParentsType() const;

        const Type::List& getMembers() const;

        Type getMembersType() const;

    };

    struct FunctionSignature : public Signature {
                                                                                            // arguments should be a tuple type, and returnType can be any type
        FunctionSignature(Type arguments, Type returnType);

        FunctionSignature(Type::List& arguments, Type returnType);

        const Type::List& getArguments() const;

        Type getArgumentsType() const;

        Type getReturnType() const;

    };

    struct VarRef;
    struct Function;

    namespace _internal {                                                          

        template <typename PtrType>
        struct Ptr;
                                                                                            // bytecode instruction set
        enum Opcode : U8 {                                                                     
                                                                                            // arguments: 1, purpose: marks the beginning of a function
            BEGFNC, 
                                                                                            // arguments: 0, purpose: marks the end of a function 
            ENDFNC, 
                                                                                            // arguments: 2, purpose: marks arg1 as the owner of arg2, so that changes in arg2 are reflected in arg1
            ALIAS,  
                                                                                            // arguments: 1, purpose: marks arg1 as an argument in a function call
            FARG,   
                                                                                            // arguments: 1, purpose: tells the JIT that arg1 is an argument and does not need to be allocated because the caller already allocated it (its runtime address is inferred by the order that these instructions are generated)
            REGARG, 
                                                                                            // arguments: 2, purpose: hints to the JIT to try to allocate arg1 in argument register arg2
            RHINT,  
                                                                                            // arguments: 1, purpose: hints to the JIT that arg1 should be placed in a register
            HOT,    
                                                                                            // arguments: 0, purpose: marks the beginning of loop mode
            BEGLP,  
                                                                                            // arguments: 0, purpose: marks the end of loop mode
            ENDLP,  
                                                                                            // arguments: 1, purpose: tells the JIT that arg1 is no longer in use and its address can be used by another live variable
            RFREE,  
                                                                                            // arguments: 1, purpose: terminates the program with code arg1
            EXIT,   
                                                                                            // arguments: 1, purpose: sets the IP to arg1
            J,      
                                                                                            // arguments: 2, purpose: sets the IP to arg1 iff arg2 == 0
            JZ,     
                                                                                            // arguments: 2, purpose: sets the IP to arg1 iff arg2 != 0
            JNZ,    
                                                                                            // arguments: 3, purpose: sets the IP to arg1 iff arg2 == arg3
            JE,     
                                                                                            // arguments: 3, purpose: sets the IP to arg1 iff arg2 != arg3
            JNE,    
                                                                                            // arguments: 3, purpose: sets the IP to arg1 iff arg2 > arg3
            JG,     
                                                                                            // arguments: 3, purpose: sets the IP to arg1 iff arg2 >= arg3
            JGE,    
                                                                                            // arguments: 1, purpose: calls the function arg1
            CALLV,  
                                                                                            // arguments: 2, purpose: calls the function arg1 and stores the result in arg2
            CALL,   
                                                                                            // arguments: 0, purpose: sets the IP to the value on top of the stack
            RETV,   
                                                                                            // arguments: 1, purpose: moves arg1 into the return register and sets the IP to the value on top of the stack
            RET,    
                                                                                            // arguments: 2, purpose: allocates arg1 bytes on the heap and sets arg2 to the new allocated address
            HALLOC, 
                                                                                            // arguments: 2, purpose: allocates arg1 bytes on the heap, zeros the newly allocated memory, and sets arg2 to the new allocated address
            HINIT,  
                                                                                            // arguments: 3, purpose: copies arg1 bytes from arg2 into arg3 
            COPY,   
                                                                                            // arguments: 2, purpose: moves the value of arg1 into arg2
            MOV,    
                                                                                            // arguments: 4, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 == 0
            MOVZ,   
                                                                                            // arguments: 4, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 != 0
            MOVNZ,  
                                                                                            // arguments: 5, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 == arg5
            MOVE,   
                                                                                            // arguments: 5, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 != arg5
            MOVNE,  
                                                                                            // arguments: 5, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 > arg5
            MOVG,   
                                                                                            // arguments: 5, purpose: moves the value of either arg1 or arg2 into arg3 depending on whether arg4 >= arg5
            MOVGE,  
                                                                                            // arguments: 2, purpose: moves the address of arg1 into arg2
            REF,    
                                                                                            // arguments: 2, purpose: moves the value in the address referenced by arg1 into arg2
            DEREF,  
                                                                                            // arguments: 2, purpose: moves the byte size of the heap object at the address referenced by arg1 into arg2
            OBJLEN, 
                                                                                            // arguments: 2, purpose: casts arg1 to a new type and stores the result in arg2
            CAST,   
                                                                                            // arguments: 3, purpose: stores the sum of arg1 and arg2 in arg3
            ADD,    
                                                                                            // arguments: 3, purpose: stores the difference of arg1 and arg2 in arg3
            SUB,    
                                                                                            // arguments: 3, purpose: stores the product of arg1 and arg2 in arg3
            MUL,    
                                                                                            // arguments: 3, purpose: stores the quotient of arg1 and arg2 in arg3
            DIV,    
                                                                                            // arguments: 3, purpose: stores the remainder of dividing arg1 by arg2 in arg3
            MOD,    
                                                                                            // arguments: 3, purpose: stores the bitwise AND of arg1 and arg2 in arg3
            BAND,   
                                                                                            // arguments: 3, purpose: stores the bitwise OR of arg1 and arg2 in arg3
            BOR,    
                                                                                            // arguments: 3, purpose: stores the bitwise XOR of arg1 and arg2 in arg3
            BXOR,   
                                                                                            // arguments: 2, purpose: stores the bitwise NOT of arg1 in arg2
            BNOT,   
                                                                                            // arguments: 2, purpose: stores the Boolean value arg1 == 0 in arg2
            ZERO,   
                                                                                            // arguments: 2, purpose: stores the Boolean value arg1 != 0 in arg2
            NZERO,  
                                                                                            // arguments: 3, purpose: stores the Boolean value arg1 == arg2 in arg3
            EQ,     
                                                                                            // arguments: 3, purpose: stores the Boolean value arg1 != arg2 in arg3
            NEQ,    
                                                                                            // arguments: 3, purpose: stores the Boolean value arg1 > arg2 in arg3
            GT,     
                                                                                            // arguments: 3, purpose: stores the Boolean value arg1 >= arg2 in arg3
            GTE,    
                                                                                            // arguments: 3, purpose: stores the bitwise left-shift of arg1 by arg2 in arg3
            SHL,    
                                                                                            // arguments: 3, purpose: stores the bitwise right-shift of arg1 by arg2 in arg3
            SHR,    
                                                                                            // arguments: 3, purpose: stores the bitwise left-rotation of arg1 by arg2 in arg3
            ROTL,   
                                                                                            // arguments: 3, purpose: stores the bitwise right-rotation of arg1 by arg2 in arg3
            ROTR,   
                                                                                            // number of instructions
            _length 

        };

        extern const U8 instructionLengths[];
        extern CString instructionNames[];
                                                                                            // base class for instruction, variable, and codeblock -- the three top-level objects that make up the jitbox intermediate representation
        struct CompilerObject {                                                                 

            enum : U8 { INSTRUCTION, VARIABLE, CODEBLOCK }; 
            
            U8 objectType;                                                                                    
                                                                                            // offset of this object within its parent codeblock if this object is an instruction or codeblock; ignored if this object is a variable
            U32 index = 0;                                                                      

            CompilerObject(U8 objectType);

        };

        using pCompilerObject = Ptr<CompilerObject>;

        struct Variable;
        struct Context;
                                                                                            // intermediate representation for a single instruction; consists of an opcode and an array of instructions
        struct Instruction : public CompilerObject {                                            
                                                                                            // maximum number of arguments of any instruction
            static const U8 MAX_NUM_ARGS = 5;   

            Opcode op;        

            SSAID args[MAX_NUM_ARGS];
        
            Type type;
                                                                                            // this constructor should never be called anywhere except in the codeblock::addinstruction function
            Instruction(Opcode, Type, Variable** srcArgs, I8 nargs);

        };

        using pInstruction = Ptr<Instruction>;

        struct CodeBlock;
        struct Local;
        struct Global;

        struct Variable : public CompilerObject, public Printable {

            SSAID identifier;
            Constant val;
                                                                                            // RH value that is assigned to this variable
            Instruction* rh;   

            Variable();

            Bool isLocal() const;

            Local* toLocal();

            Global* toGlobal();

            String toString(U8 alignment) const override;

        };

        using pVariable = Ptr<Variable>;

        struct Local : public Variable {

            U32 lrBeg, lrEnd;
            CodeBlock* owner;

            Local(CodeBlock* owner, Type);

            Local(CodeBlock* owner, Constant);

        };

        struct Global : public Variable {

            Context* owner;

            Global(Context* owner, Constant);

            Global(Context* owner, Global*);

        };

        struct CodeBlock : public CompilerObject, public Printable {
                                                                                            // contains all variables in the function that this codeblock belongs to; memory is allocated and deleted by the top-level scope codeblock
            Vec<pCompilerObject> code;
        
            Vec<pVariable>* variables;                                                          
                                                                                            // type id associated with this function
            Type type;
                                                                                            // the ID associated with this function's signature (or zero if this codeblock is a child of another codeblock)
            ID identifier = 0;

            Context* owner;
                                                                                            // the starting index of this codeblock relative to its parent's starting index
            U32 begIdx = 0;
                                                                                            // current active scope within this function
            CodeBlock* curScope = nullptr;
                                                                                            // current innermost active loop within the current function; might be nullptr
            CodeBlock* curLoop = nullptr;
                                                                                            // adds an instruction to this codeblock
            Instruction* addInstruction(Opcode, Variable** args, U8 nargs);
                                                                                            // adds a variable to this codeblock
            Variable* addVariable(Type);
                                                                                            // adds a constant to this codeblock
            Variable* addConstant(Constant);
                                                                                            // adds a variable assignment to this codeblock
            void addAssignment(Variable* lh, Instruction* rh);
                                                                                            // infers expected data type that an instruction will return based on its input arguments, performing implicit typecasting if the current context permits it; also checks that argument types are legal if JITBOX_DEBUG is defined (if nargs is -1 this means that the given argument is the LH of an assignment)
            Type inferType(Opcode, Variable** args, U8 nargs);

            enum : U8 {
                // this codeblock is the top-level scope of a function
                FUNCTION,
                // this codeblock is a child scope of a function
                CHILD,
                // this codeblock contains an instruction (loop condition) and a child codeblock (loop body) 
                LOOP,
                // this codeblock contains an instruction (if condition), a child codeblock (if body), and an optional second child codeblock (else body)
                CONDITIONAL

            };
            U8 blockType;

            CodeBlock(Context* owner, U8 blockType, Type type, ID identifier);
        
            ~CodeBlock();

            String toString(U8 alignment) const override;

        };

        using pCodeBlock = Ptr<CodeBlock>;

        struct Context {

            Vec<pCodeBlock> functions;
            Vec<pVariable> variables;
                                                                                            // current function being compiled within this context
            CodeBlock* curFunction = nullptr;                                                                                                        

            CodeBlock* addFunction(const Type::List& argtypes, Type returntype);
                                                                                            // adds global variable to the current context and initializes it to the value of v
            Variable* addGlobalVariable(Variable* v);
                                                                                            // adds global variable to the current context
            Variable* addGlobalVariable(Constant);

            static Context* curContext();

        };

        using pContext = Ptr<Context>;

        Variable* getInternalVar(VarRef);

        CodeBlock* getInternalCode(Function);

        Constant fold(Instruction* in);

        /**
            below is a simple smart pointer implementation I wrote a long time ago before
            I knew that std smart pointers were a thing; I still use it just out of nostalgia
        **/

        template <int _>
        struct PtrContainerBase {

            const void* p = nullptr;
            int refCount = 0;

            inline PtrContainerBase(const void*);

        };

        using PtrContainer = PtrContainerBase<0>;

        template <typename PtrType>
        struct Ptr {

            template <typename CastType>
            inline Ptr<CastType> cast();

            inline Ptr();
            inline Ptr(const Ptr&);
            inline Ptr(const PtrType*);

            inline void operator = (const Ptr&);
            inline void operator = (const PtrType*);

            inline PtrType* operator -> () const;
            inline PtrType& operator * () const;

            inline PtrType* operator () () const;

            inline Bool operator == (const Ptr&) const;
            inline Bool operator == (PtrType*) const;

            inline Bool operator != (const Ptr&) const;
            inline Bool operator != (PtrType*) const;

            inline Bool operator >= (const Ptr&) const;
            inline Bool operator >= (PtrType*) const;

            inline Bool operator <= (const Ptr&) const;
            inline Bool operator <= (PtrType*) const;

            inline Bool operator > (const Ptr&) const;
            inline Bool operator > (PtrType*) const;

            inline Bool operator < (const Ptr&) const;
            inline Bool operator < (PtrType*) const;

            inline ~Ptr();

        protected:

            PtrContainer* ptrCnt = nullptr;

            inline Ptr(PtrContainer*, void*);

            inline void copy(const Ptr&);
            inline void init(const PtrType*);
            inline void cleanup();

            template <typename FriendType>
            friend struct Ptr;

        };

        template <int _>
        inline PtrContainerBase<_>::PtrContainerBase(const void* p) : p(p) { }

        template <typename PtrType>
        template <typename CastType>
        inline Ptr<CastType> Ptr<PtrType>::cast() { return ptrCnt ? Ptr<CastType>(ptrCnt, nullptr) : Ptr<CastType>(); }

        template <typename PtrType>
        inline Ptr<PtrType>::Ptr() { init(nullptr); }

        template <typename PtrType>
        inline Ptr<PtrType>::Ptr(const Ptr& Ptr) { copy(Ptr); }

        template <typename PtrType>
        inline Ptr<PtrType>::Ptr(const PtrType* Ptr) { init(Ptr); }

        template <typename PtrType>
        inline void Ptr<PtrType>::operator = (const Ptr& Ptr) { cleanup(); copy(Ptr); }

        template <typename PtrType>
        inline void Ptr<PtrType>::operator = (const PtrType* Ptr) { cleanup(); init(Ptr); }

        template <typename PtrType>
        inline PtrType* Ptr<PtrType>::operator -> () const { return (PtrType*)ptrCnt->p; }

        template <typename PtrType>
        inline PtrType& Ptr<PtrType>::operator * () const { return *(PtrType*)(ptrCnt->p); }

        template <typename PtrType>
        inline PtrType* Ptr<PtrType>::operator () () const { if (ptrCnt) if (ptrCnt->p) return (PtrType*)ptrCnt->p; return nullptr; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator == (const Ptr& rh) const { return ptrCnt ? (rh.ptrCnt ? ptrCnt->p == rh.ptrCnt->p : false) : !rh.ptrCnt; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator == (PtrType* rh) const { return ptrCnt ? ptrCnt->p == rh : !rh; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator != (const Ptr& rh) const { return ptrCnt ? (rh.ptrCnt ? ptrCnt->p != rh.ptrCnt->p : true) : (Bool)rh.ptrCnt; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator != (PtrType* rh) const { return ptrCnt ? ptrCnt->p != rh : (Bool)rh; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator >= (const Ptr& rh) const {

            if (!ptrCnt && !rh.ptrCnt) return true;
            if (!ptrCnt && rh.ptrCnt) return false;
            if (ptrCnt && !rh.ptrCnt) return true;
            return ptrCnt->p >= rh.ptrCnt->p;

        }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator >= (PtrType* rh) const { if (!ptrCnt && rh) return false; return ptrCnt->p >= (const void*)rh; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator <= (const Ptr& rh) const { return rh >= *this; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator <= (PtrType* rh) const { if (!ptrCnt && rh) return true; return ptrCnt->p <= (const void*)rh; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator > (const Ptr& rh) const {

            if (!ptrCnt && !rh.ptrCnt) return false;
            if (!ptrCnt && rh.ptrCnt) return false;
            if (ptrCnt && !rh.ptrCnt) return true;
            return ptrCnt->p > rh.ptrCnt->p;

        }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator > (PtrType* rh) const { if (!ptrCnt && rh) return false; return ptrCnt->p > (const void*)rh; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator < (const Ptr& rh) const { return rh > *this; }

        template <typename PtrType>
        inline Bool Ptr<PtrType>::operator < (PtrType* rh) const { if (!ptrCnt && rh) return true; return ptrCnt->p < (const void*)rh; }

        template <typename PtrType>
        inline Ptr<PtrType>::~Ptr() { cleanup(); }

        template <typename PtrType>
        inline Ptr<PtrType>::Ptr(PtrContainer* ptrCnt, void*) : ptrCnt(ptrCnt) { ptrCnt->refCount++; }

        template <typename PtrType>
        inline void Ptr<PtrType>::copy(const Ptr& Ptr) {

            if (Ptr.ptrCnt == nullptr) { ptrCnt = nullptr; return; }

            ptrCnt = Ptr.ptrCnt;
            ptrCnt->refCount++;
        }

        template <typename PtrType>
        inline void Ptr<PtrType>::init(const PtrType* Ptr) {

            if (Ptr) ptrCnt = new PtrContainer(Ptr);
            else ptrCnt = nullptr;

        }

        template <typename PtrType>
        inline void Ptr<PtrType>::cleanup() {

            if (!ptrCnt) return;

            if (ptrCnt->refCount == 0) { if (ptrCnt->p) delete (const PtrType*)ptrCnt->p; delete ptrCnt; }
            else ptrCnt->refCount--;

        }

    }
                                                                                            // the user should never directly create an instruction; this alias is just for readability
    using Instruction = _internal::Instruction*;

    Constant ConstU8(U8 val);
    Constant ConstI8(I8 val);
    Constant ConstU16(U16 val);
    Constant ConstI16(I16 val);
    Constant ConstU32(U32 val);
    Constant ConstI32(I32 val);
    Constant ConstU64(U64 val);
    Constant ConstI64(I64 val);
    Constant ConstF32(F32 val);
    Constant ConstF64(F64 val);

    struct Var;

    struct Context {
                                                                                            // if active is set to true, the newly constructed context will be set to the active context
        Context(Bool active = true);
                                                                                            // makes this context active
        void use() const;

        ~Context();

    protected:
        
        _internal::pContext ctx;
        static _internal::Context* curContext;

        friend struct _internal::Context;

    };

    struct VarRef;

    struct Var : public Printable {

        Var(const Var& v);
        Var(const Constant& c);
        Var(Instruction in);

        void operator = (const Var& v);
        void operator = (const Constant& c);
        void operator = (Instruction in);

        Type getType() const;

        String toString(U8 alignment) const override;

    protected:

        _internal::Variable* v = nullptr;

        void assign(const Var& v);
        void assign(const Constant& c);
        void assign(Instruction in);

        friend struct VarRef;

    };
                                                                                            // reference to a var object to avoid calling the copy constructor on SSA variables; only used as arguments for jitbox functions and there is no reason to directly create a VarRef object
    struct VarRef {
        
        VarRef(const Var& v);
        VarRef(const Constant& c);
        VarRef(Instruction in);

    protected:

        _internal::Variable* v;

        friend _internal::Variable* _internal::getInternalVar(VarRef);

    };

    using VarList = Vec<VarRef>;

    struct Function : public Printable {
                                                                                            // switches the context to the newly created function if active is set to true
        Function(const Type::List& argTypes, Type returnType, Bool active = true);
                                                                                            // makes this function the active function for code generation
        void use();
                                                                                            // returns the type id associated with this function
        Type getType() const;

        String toString(U8 alignment) const override;

    protected:

        _internal::CodeBlock* definition = nullptr;

        friend _internal::CodeBlock* _internal::getInternalCode(Function);

    };
                                                                                            // exits the program with the specified code (argument must be u8)
    void Terminate(VarRef);
                                                                                            // returns from a void function
    void Return();
                                                                                            // returns a value from a function
    void Return(VarRef);
                                                                                            // calls the specified function (with no arguments)
    Instruction Call(Function func);
                                                                                            // calls the specified function with the specified arguments
    Instruction Call(Function func, const VarList& args);
                                                                                            // casts src VarRefiable to the specified type
    Instruction Cast(VarRef src, Type to);
                                                                                            // computes the sum of the specified variables
    Instruction Add(VarRef lh, VarRef rh);
     /*                                                                                       // computes the difference of the specified variables
    instruction Sub(VarRef lh, VarRef rh);
                                                                                            // computes the product of the specified variables
    instruction Mul(VarRef lh, VarRef rh);
                                                                                            // computes the quotient of the specified variables
    instruction Div(VarRef lh, VarRef rh);
                                                                                            // computes the division remainder of the specified variables
    instruction Mod(VarRef lh, VarRef rh);
                                                                                            // computes the bitwise AND of the specified variables
    instruction BitwiseAnd(VarRef lh, VarRef rh);
                                                                                            // computes the bitwise OR of the specified variables
    instruction BitwiseOr(VarRef lh, VarRef rh);
                                                                                            // computes the bitwise NOT of the specified variable
    instruction BitwiseNot(VarRef);
                                                                                            // computes whether the specified variable is zero; returns type::u8
    instruction Zero(VarRef);
                                                                                            // computes whether the specified variable is nonzero; returns type::u8
    instruction NotZero(VarRef);
                                                                                            // computes the == operator on the specified variables; returns type::u8
    instruction Equal(VarRef lh, VarRef rh);
                                                                                            // computes the != operator on the specified variables; returns type::u8
    instruction NotEqual(VarRef lh, VarRef rh);
                                                                                            // computes the > operator on the specified variables; returns type::u8
    instruction GreaterThan(VarRef lh, VarRef rh);
                                                                                            // computes the >= operator on the specified variables; returns type::u8
    instruction GreaterThanEqual(VarRef lh, VarRef rh);
                                                                                            // computes the < operator on the specified variables; returns type::u8
    instruction LessThan(VarRef lh, VarRef rh);
                                                                                            // computes the <= operator on the specified variables; returns type::u8
    instruction LessThanEqual(VarRef lh, VarRef rh);
                                                                                            // computes the left bit shift on the specified variables
    instruction BitShiftLeft(VarRef lh, VarRef rh);
                                                                                            // computes the right bit shift on the specified variables
    instruction BitShiftRight(VarRef lh, VarRef rh);
                                                                                            // computes the left bit rotation on the specified variables
    instruction BitRotateLeft(VarRef lh, VarRef rh);
                                                                                            // computes the right bit rotation on the specified variables
    instruction BitRotateRight(VarRef lh, VarRef rh);  
                                                                                            // gets argument of the current function by the specified index
    VarRef Arg(U8 index);*/

    /*struct makeif {                                                                         // creates an if codeblock within the current context

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

    };*/
                                                                                            // ends the current codeblock being generated
    void End();
                                                                                            // defines a global variable within the current context; example usage: var example = makeglobalvar(constu8(1));
    //var makeglobalvar(constant ctc);
                                                                                            // compiles the current context
    //void compile();

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
        inline codeblock& passbyref(const vec<Bool>& args);

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

                    // computes whether the specified variable is zero (type::u8 is used as a Boolean type)
    inline instruction zero(var&); inline instruction zero(varproxy v) { return zero(v.v); }

                    // computes whether the specified variable is nonzero (type::u8 is used as a Boolean type)
    inline instruction notzero(var&); inline instruction notzero(varproxy v) { return notzero(v.v); }

                    // computes the == operator on the specified variables (type::u8 is used as a Boolean type)
    inline instruction equal(var& lh, var& rh); inline instruction equal(varproxy lh, var& rh) { return equal(lh.v, rh); } inline instruction equal(var& lh, varproxy rh) { return equal(lh, rh.v); } inline instruction equal(varproxy lh, varproxy rh) { return equal(lh.v, rh.v); }

                    // computes the != operator on the specified variables (type::u8 is used as a Boolean type)
    inline instruction notequal(var& lh, var& rh); inline instruction notequal(varproxy lh, var& rh) { return notequal(lh.v, rh); } inline instruction notequal(var& lh, varproxy rh) { return notequal(lh, rh.v); } inline instruction notequal(varproxy lh, varproxy rh) { return notequal(lh.v, rh.v); }

                    // computes the > operator on the specified variables (type::u8 is used as a Boolean type)
    inline instruction greaterthan(var& lh, var& rh); inline instruction greaterthan(varproxy lh, var& rh) { return greaterthan(lh.v, rh); } inline instruction greaterthan(var& lh, varproxy rh) { return greaterthan(lh, rh.v); } inline instruction greaterthan(varproxy lh, varproxy rh) { return greaterthan(lh.v, rh.v); }

                    // computes the >= operator on the specified variables (type::u8 is used as a Boolean type)
    inline instruction greaterthanequal(var& lh, var& rh); inline instruction greaterthanequal(varproxy lh, var& rh) { return greaterthanequal(lh.v, rh); } inline instruction greaterthanequal(var& lh, varproxy rh) { return greaterthanequal(lh, rh.v); } inline instruction greaterthanequal(varproxy lh, varproxy rh) { return greaterthanequal(lh.v, rh.v); }

                    // computes the < operator on the specified variables (type::u8 is used as a Boolean type)
    inline instruction lessthan(var& lh, var& rh); inline instruction lessthan(varproxy lh, var& rh) { return lessthan(lh.v, rh); } inline instruction lessthan(var& lh, varproxy rh) { return lessthan(lh, rh.v); } inline instruction lessthan(varproxy lh, varproxy rh) { return lessthan(lh.v, rh.v); }

                    // computes the <= operator on the specified variables (type::u8 is used as a Boolean type)
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
   
    Bool fromparentscope = definition->owner->isparentof(ctxt.currentcode);
   
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
   
    Bool fromparentscope = definition->owner->isparentof(ctxt.currentcode);
   
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
   
    Bool fromparentscope = definition ? definition->owner->isparentof(ctxt.currentcode) : false;

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

inline jitbox::codeblock& jitbox::codeblock::passbyref(const vec<Bool>& args) { definition->passbyref = args; return *this; }

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

    Bool doublescoped = curcode->ctype == internalcodeblock::ifblock || curcode->ctype == internalcodeblock::elseblock;

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