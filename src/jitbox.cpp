#include <iostream>
#include "./jitbox.hpp"
#include "./Timer.hpp"

using namespace jitbox;
namespace jbi = jitbox::_internal;

#define indentChar " "

String Printable::print(U8 alignment, Bool addnewline) const { return indent(alignment) + this->toString(alignment) + (addnewline ? "\n" : ""); }

Printable::~Printable() { }

String Printable::indent(U8 nind) {

    if (nind == 0) return " ";

    String ind;
    for (I32 i = 0; i < nind - 1; i++) ind += indentChar "  ";
    return " " + ind + indentChar " ";

}

String Printable::pad(String str, U16 padnum) {

    I32 reqspace = padnum - (int)str.size();
    if (reqspace < 0) return str.substr(0, padnum - 3) + "...";

    for (I32 i = 0; i < reqspace; i++) str += " ";
    return str;

}

#ifdef JITBOX_DEBUG

Error::Error(ErrorCode code) : code(code) { }

#endif

template <typename KeyType>
inline ID getValueFromKey(Map<KeyType, ID>& kvMap, Map<ID, KeyType>& vkMap, ID& nextFreeID, const KeyType& key) {

    auto findMatch = kvMap.find(key);

    if (findMatch == kvMap.end()) {

        kvMap[key] = nextFreeID;
        vkMap[nextFreeID] = key;

        nextFreeID += incrementID;
        return nextFreeID - incrementID;

    }

    return findMatch->second;

}

template <typename KeyType>
inline const KeyType* getKeyFromValue(Map<ID, KeyType>& vkMap, ID ID) {

    auto findMatch = vkMap.find(ID);

    if (findMatch == vkMap.end()) return nullptr;
    return &findMatch->second;

}

#define implementMaps(TheType, K, V) \
\
Map<K, V> kvMap_##TheType; \
Map<V, K> vkMap_##TheType; \
ID nextFreeID_##TheType = Type::TheType + jbi::numDerivedTypeClasses

implementMaps(POINTER, ID, ID);
implementMaps(ARRAY, ID, ID);
implementMaps(STRUCTURE, Signature, ID);
implementMaps(FUNCTION, Signature, ID);
implementMaps(UNION, jbi::CanonicalTypeSet, ID);

jbi::CanonicalTypeSet::CanonicalTypeSet() { 
    
    for (U8 i = 0; i < numDerivedTypes; i++) derivedTypes[i] = Type::NOTHING;
    std::memset(primitiveTypes, false, sizeof(bool) * numPrimitiveTypes);

}

jbi::CanonicalTypeSet::CanonicalTypeSet(const CanonicalTypeSet& cts) { 
    
    std::memcpy(derivedTypes, cts.derivedTypes, sizeof(Type) * numDerivedTypes);
    std::memcpy(primitiveTypes, cts.primitiveTypes, sizeof(bool) * numPrimitiveTypes);

}

bool jbi::CanonicalTypeSet::operator < (const CanonicalTypeSet& cts) {

    for (U8 i = 0; i < numDerivedTypes; i++) {

        if (derivedTypes[i] < cts.derivedTypes[i]) return true;
        if (cts.derivedTypes[i] < derivedTypes[i]) return false;

    }

    for (U8 i = 0; i < numPrimitiveTypes; i++) {

        if (primitiveTypes[i] < cts.primitiveTypes[i]) return true;
        if (primitiveTypes[i] > cts.primitiveTypes[i]) return false;

    }

    return false;

}

void jbi::CanonicalTypeSet::merge(Type t) {

    auto tClass = t.getClass();

    if (tClass == Type::UNION) { merge(*getKeyFromValue(vkMap_UNION, t.getID())); return; }

    if (tClass == Type::PRIMITIVE) { primitiveTypes[t.getID() - firstPrimitive] = true; return; }

    auto matchingClass = tClass - firstDerivedTypeClass;

    derivedTypes[matchingClass] = derivedTypes[matchingClass].Or(t);

}

void jbi::CanonicalTypeSet::merge(const CanonicalTypeSet& cts) {

    for (U8 i = 0; i < numDerivedTypes; i++) derivedTypes[i] = derivedTypes[i].Or(cts.derivedTypes[i]);
    for (U8 i = 0; i < numPrimitiveTypes; i++) primitiveTypes[i] = primitiveTypes[i] || cts.primitiveTypes[i];

}

void jbi::CanonicalTypeSet::intersect(Type t) {

    auto tClass = t.getClass();

    if (tClass == Type::UNION) { intersect(*getKeyFromValue(vkMap_UNION, t.getID())); return; }

    if (tClass == Type::PRIMITIVE) { 
        
        std::memset(primitiveTypes, false, sizeof(bool) * numPrimitiveTypes); 
        primitiveTypes[t.getID() - firstPrimitive] = true; 
        
        return;
        
    }

    auto matchingClass = tClass - firstDerivedTypeClass;

    derivedTypes[matchingClass] = derivedTypes[matchingClass].And(t);

}

void jbi::CanonicalTypeSet::intersect(const CanonicalTypeSet& cts) {

    for (U8 i = 0; i < numDerivedTypes; i++) derivedTypes[i] = derivedTypes[i].And(cts.derivedTypes[i]);
    for (U8 i = 0; i < numPrimitiveTypes; i++) primitiveTypes[i] = primitiveTypes[i] && cts.primitiveTypes[i];

}

void jbi::CanonicalTypeSet::negate() {

    CanonicalTypeSet ctsNew;

    for (U8 i = 0; i < numDerivedTypes; i++) ctsNew.derivedTypes[i] = 
        (derivedTypes[i] == Type::NOTHING) 
            ? firstDerivedTypeClass + i 
            : derivedTypes[i].Not()
        ;
    
    for (U8 i = 0; i < numPrimitiveTypes; i++) primitiveTypes[i] = !primitiveTypes[i];
    
}

Type::Type() { }

#define validateTypeID(TheType, KeyType) \
\
case Type::TheType: \
    \
    if (!getKeyFromValue(vkMap_##TheType, identifier)) throw Error(Error::INVALID_TYPE_ID); \
    break;

Type::Type(ID identifier) : identifier(identifier) {
    
    determineClass();

#ifdef JITBOX_DEBUG
    
    switch (typeClass) {

        validateTypeID(POINTER, ID);
        validateTypeID(ARRAY, ID);
        validateTypeID(STRUCTURE, Signature);
        validateTypeID(FUNCTION, Signature);
        validateTypeID(UNION, jbi::CanonicalTypeSet);

    }

#endif

}

Bool jitbox::operator == (const Type& lh, ID rh) { return lh.getID() == rh; }

Bool jitbox::operator == (const Type& lh, const Type& rh) { return lh.getID() == rh.getID(); }

Bool jitbox::operator == (ID lh, const Type& rh) { return lh == rh.getID(); }

Bool jitbox::operator != (const Type& lh, ID rh) { return lh.getID() != rh; }

Bool jitbox::operator != (const Type& lh, const Type& rh) { return lh.getID() != rh.getID(); }

Bool jitbox::operator != (ID lh, const Type& rh) { return lh != rh.getID(); }

Bool jitbox::operator < (const Type& lh, const Type& rh) { return lh.getID() < rh.getID(); }

Type Type::Pointer(Type t) { return Type(getValueFromKey(kvMap_POINTER, vkMap_POINTER, nextFreeID_POINTER, t.getID())); }

Type Type::Array(Type t) { return Type(getValueFromKey(kvMap_ARRAY, vkMap_ARRAY, nextFreeID_ARRAY, t.getID())); }

Type Type::Structure(const Signature& s) { return Type(getValueFromKey(kvMap_STRUCTURE, vkMap_STRUCTURE, nextFreeID_STRUCTURE, s)); }

Type Type::Function(const Signature& s) { return Type(getValueFromKey(kvMap_FUNCTION, vkMap_FUNCTION, nextFreeID_FUNCTION, s)); }

Type Type::Any(const Type::List& tl) {

    bool multipleClasses = false;
    ID lastClass = -1;

    for (auto t : tl) {

        auto tid = t.getID();

        if (tid == ANYTHING) return ANYTHING;

        if (lastClass < 0) lastClass = tid;
        else if (lastClass != tid) multipleClasses = true;

    }

    if (!multipleClasses) {

        switch (lastClass) {

            case NOTHING: return NOTHING;
            
            case UNION: {

                Type::Set s;

                TODO: Add exception for max recursion depth exceeded
                TODO: Signature comparison is only based on ID
                TODO: Don't even expose signature constructor to the user
            }

        }

    }

}

#define makeTypeGetter(TheType, getter, KeyType, rettype) \
\
rettype Type::getter() const { return *getKeyFromValue(vkMap_##TheType, identifier); }

Type Type::pointsTo() const {

#ifdef JITBOX_DEBUG
    if (typeClass != POINTER) throw Error(Error::TYPE_GETTER_MISMATCH);
#endif

    return *getKeyFromValue(vkMap_POINTER, identifier);

}

Type Type::contains() const {

#ifdef JITBOX_DEBUG
    if (typeClass != ARRAY) throw Error(Error::TYPE_GETTER_MISMATCH);
#endif

    return *getKeyFromValue(vkMap_ARRAY, identifier);

}

const Signature& Type::getSignature() const {

#ifdef JITBOX_DEBUG
    if (typeClass != FUNCTION && typeClass != STRUCTURE) throw Error(Error::TYPE_GETTER_MISMATCH);
#endif

    return *getKeyFromValue((typeClass == FUNCTION) ? vkMap_FUNCTION : vkMap_STRUCTURE, identifier);

}

const Type::List& Type::getList() const {

#ifdef JITBOX_DEBUG
    if (typeClass != ANY && typeClass != ALL) throw Error(Error::TYPE_GETTER_MISMATCH);
#endif

    return *getKeyFromValue((typeClass == ANY) ? vkMap_ANY : vkMap_ALL, identifier);

}

Bool Type::is(Type t) const {
    std::cout << "Checking whether\n" << print(1) << "is" << t.print(1) << "\n";
    if (t.identifier == ANYTHING) return true;
    if (t.identifier == NOTHING) return false;
    if (identifier == t.identifier) return true;

    if (t.typeClass == NOT) {

        if (t.identifier == NOT) return typeClass == NOT;

        return !(this->is(Not(t)));

    }

    if (typeClass == NOT) {

        if (identifier == NOT) return false;

        return !(Not(*this).is(t));
        // Not(PRIMITIVE).is(ARRAY) = ! (PRIMITIVE.is(ARRAY)) = ! false = true
        
    }

    if (t.typeClass == ANY) {

        if (t.identifier == ANY) return typeClass == ANY;

        const auto& tl = t.getList();

        for (auto ty : tl) if (this->is(ty)) return true;
        return false;

    }

    if (typeClass == ANY) {

        if (identifier == ANY) return false;

        const auto& tl = getList();

        for (auto ty : tl) if (ty.is(t)) return true;
        return false;

    }

    if (t.typeClass == ALL) {

        if (t.identifier == ALL) return typeClass == ALL;

        const auto& tl = t.getList();

        for (auto ty : tl) if (!this->is(ty)) return false;
        return true;

    }

    if (typeClass == ALL) {

        if (identifier == ALL) return false;

        const auto& tl = getList();

        for (auto ty : tl) if (!ty.is(t)) return false;
        return true;

    }

    if (typeClass != t.typeClass) return false;

    if (typeClass == PRIMITIVE && t.identifier == PRIMITIVE) return true;

    if (typeClass == POINTER) {

        if (t.identifier == POINTER) return true;
        return Type(pointsTo()).is(t.pointsTo());

    }

    if (typeClass == ARRAY) {

        if (t.identifier == ARRAY) return true;
        return Type(contains()).is(t.contains());

    }

    if (typeClass == STRUCTURE || typeClass == FUNCTION) {

        if (t.identifier == STRUCTURE || t.identifier == FUNCTION) return true;

        const Signature *lh, *rh;

        if (typeClass == STRUCTURE) { lh = &getSignature(); rh = &t.getSignature(); }
        else { lh = &getSignature(); rh = &t.getSignature(); }

        return lh->identifier == rh->identifier;

    }

    return false;

}

Bool Type::is(ID i) const { return is(Type(i)); }

ID Type::getID() const { return identifier; }

ID Type::getClass() const { return typeClass; }

Bool Type::isConcrete() const {

    if (identifier == NOTHING) return true;
    if (identifier == ANYTHING) return false;
    if (typeClass == PRIMITIVE) return identifier != PRIMITIVE;
    if (typeClass == ANY) return false;

    if (typeClass == POINTER) {

        if (identifier == POINTER) return false;
        return Type(pointsTo()).isConcrete();

    }

    if (typeClass == ARRAY) {

        if (identifier == ARRAY) return false;
        return Type(contains()).isConcrete();

    }

    if (typeClass == STRUCTURE || typeClass == FUNCTION) {

        if (identifier == typeClass) return false;

        auto& sig = (typeClass == STRUCTURE) ? getSignature() : getSignature();

        for (jitbox::I32 k = 0; k < sig.argTypes.size(); k++)
            if (!sig.argTypes[k].isConcrete()) return false;

        return true;

    }

    return false;

}

Bool Type::isAbstract() const { return !isConcrete(); }

Bool Type::isInteger() const { return identifier >= Type::I8 && identifier <= Type::U64; }

Bool Type::isUnsigned() const { return identifier >= Type::U8 && identifier <= Type::U64; }

Bool Type::isFP() const { return identifier == Type::F32 || identifier == Type::F64; }

I32 Type::numBytes() const {

    if (isAbstract()) return -1;

    if (typeClass == PRIMITIVE) {

        if (identifier == Type::I8 || identifier == Type::U8) return 1;
        if (identifier == Type::I16 || identifier == Type::U16) return 2;
        if (identifier == Type::I32 || identifier == Type::U32 || identifier == Type::F32) return 4;
        return 8;

    }

    if (typeClass == ARRAY) return -1;

    if (typeClass == STRUCTURE || typeClass == FUNCTION) {

        auto& sig = (typeClass == STRUCTURE) ? getSignature() : getSignature();
        jitbox::I32 b = 0;

        for (jitbox::I32 k = 0; k < sig.argTypes.size(); k++) b += sig.argTypes[k].numBytes();

        return b;

    }

    if (typeClass == NOTHING) return 0;

    return 8;

}

void Type::determineClass() {

    if (identifier == NOTHING) typeClass = NOTHING;
    else if (identifier == ANYTHING) typeClass = ANYTHING;
    else if (identifier >= PRIMITIVE && identifier < POINTER) typeClass = PRIMITIVE;
    else if ((identifier - POINTER) % incrementID == 0) typeClass = POINTER;
    else if ((identifier - ARRAY) % incrementID == 0) typeClass = ARRAY;
    else if ((identifier - STRUCTURE) % incrementID == 0) typeClass = STRUCTURE;
    else if ((identifier - FUNCTION) % incrementID == 0) typeClass = FUNCTION;
    else if ((identifier - ANY) % incrementID == 0) typeClass = ANY;

}

String Type::toString(jitbox::U8 alignment) const {

    if (identifier == NOTHING) return "nothing";

    if (identifier == ANYTHING) return "anything";

    if (typeClass == PRIMITIVE) {

        static const char* PRIMITIVEs[] = { "primitive", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64" };
        return PRIMITIVEs[identifier - PRIMITIVE];

    }

    if (typeClass == ARRAY) return "array of:\n" + Type(contains()).print(alignment + 1, false);

    if (typeClass == POINTER) return "pointer to:\n" + Type(pointsTo()).print(alignment + 1, false);

    if (typeClass == FUNCTION) return "function:\n" + getSignature().print(alignment + 1, false);

    if (typeClass == STRUCTURE) return "structure:\n" + getSignature().print(alignment + 1, false);

    if (typeClass == ANY) {

        String s = "any:\n";
        const auto& tl = getList();

        for (jitbox::I32 i = 0; i < tl.size(); i++) s += tl[i].print(alignment + 1, i == tl.size() - 1);
        return s;

    }

    return "unknown type";
    
}

Bool Constant::operator == (const Constant& ctc) {

    if (type != ctc.type) return false;

    auto bw = Type(type).numBytes();

    if (bw == 1) return vU8 == ctc.vU8;
    if (bw == 2) return vU16 == ctc.vU16;
    if (bw == 4) return vU32 == ctc.vU32;
    return vU64 == ctc.vU64;

}

Bool Constant::operator != (const Constant& ctc) { return !(*this == ctc); }

#define CTCToStringCase(TheType) else if (type == Type::TheType) s = std::to_string(v##TheType) + #TheType

String Constant::toString(U8) const {

    String s = "unknown";

    if (type.isInteger()) {

        if (false);
        CTCToStringCase(U8);
        CTCToStringCase(I8);
        CTCToStringCase(U16);
        CTCToStringCase(I16);
        CTCToStringCase(U32);
        CTCToStringCase(I32);
        CTCToStringCase(U64);
        CTCToStringCase(I64);

    }
    else {

        if (type == Type::F32) s = std::to_string(vF32);
        else if (type == Type::F64) s = std::to_string(vF64);

    }

    return s;

}

inline Bool Signature::operator < (const Signature& rh) const {
     
    if (identifier < rh.identifier) return true;
    if (identifier > rh.identifier) return false;
    if (returnType.getID() < rh.returnType.getID()) return true;
    if (returnType.getID() > rh.returnType.getID()) return false;

    return argTypes < rh.argTypes;

}

String Signature::toString(U8 alignment) const {

    String s = "identifier:\n" + indent(alignment + 1) + std::to_string(identifier) + "\n";
    s += indent(alignment) + "argument types:\n";

    I32 numargs = argTypes.size();
    for (I32 i = 0; i < numargs; i++) s += argTypes[i].print(alignment + 1, i != numargs - 1);

    s += indent(alignment) + "return type\n";
    s += Type(returnType).print(alignment + 1, false);

    return s;

}

const U8 jbi::instructionLengths[] = {

    0, // begfnc
    0, // endfnc
    2, // alias
    1, // farg
    1, // regarg
    2, // rhint
    1, // hot
    0, // beglp
    0, // endlp
    1, // rfree
    1, // exit
    1, // j
    2, // jz
    2, // jnz
    3, // je
    3, // jne
    3, // jg
    3, // jge
    1, // callv
    2, // call
    0, // retv
    1, // ret
    2, // halloc
    2, // hinit
    3, // copy
    2, // mov
    4, // movz
    4, // movnz
    5, // move
    5, // movne
    5, // movg
    5, // movge
    2, // ref
    2, // deref
    2, // objlen
    2, // cast
    3, // add
    3, // sub
    3, // mul
    3, // div
    3, // mod
    3, // band
    3, // bor
    3, // bxor
    2, // bnot
    2, // zero
    2, // nzero
    3, // eq
    3, // neq
    3, // gt
    3, // gte
    3, // shl
    3, // shr
    3, // rotl
    3, // rotr

};

const char* jbi::instructionNames[] = {

    "begfnc",
    "endfnc",
    "alias",
    "farg",
    "regarg",
    "rhint",
    "hot",
    "beglp",
    "endlp",
    "rfree",
    "exit",
    "j",
    "jz",
    "jnz",
    "je",
    "jne",
    "jg",
    "jge",
    "callv",
    "call",
    "retv",
    "ret",
    "halloc",
    "hinit",
    "copy",
    "mov",
    "movz",
    "movnz",
    "move",
    "movne",
    "movg",
    "movge",
    "ref",
    "deref",
    "objlen",
    "cast",
    "add",
    "sub",
    "mul",
    "div",
    "mod",
    "band",
    "bor",
    "bxor",
    "bnot",
    "zero",
    "nzero",
    "eq",
    "neq",
    "gt",
    "gte",
    "shl",
    "shr",
    "rotl",
    "rotr"

};

jbi::CompilerObject::CompilerObject(U8 objectType) : objectType(objectType) { }

jbi::Instruction::Instruction(Opcode op, Type type, Variable** srcArgs, I8 nargs) : CompilerObject(CompilerObject::INSTRUCTION), op(op), type(type) { 

    memset(args, 0, MAX_NUM_ARGS * sizeof(SSAID));
    for (I32 i = 0; i < nargs; i++) args[i] = srcArgs[i]->identifier;

}

jbi::Variable::Variable() : CompilerObject(CompilerObject::VARIABLE) { }

Bool jbi::Variable::isLocal() const { return identifier > 0; }

jbi::Local* jbi::Variable::toLocal() { return (Local*)this; }

jbi::Global* jbi::Variable::toGlobal() { return (Global*)this; }

String jbi::Variable::toString(U8 alignment) const {

    String s = (identifier > 0) ? "local" : ((identifier < 0) ? "global" : "null");
    auto sID = (identifier > 0) ? identifier : -identifier;

    s += std::to_string(sID);
    
    return s;

}

jbi::Local::Local(CodeBlock* owner, Type type) : owner(owner) {

    identifier = owner->variables->size() + 1;
    lrBeg = owner->code.size() + owner->begIdx;
    lrEnd = lrBeg;
    val.type = type;

}

jbi::Local::Local(CodeBlock* owner, Constant c) : owner(owner) {

    identifier = owner->variables->size() + 1;
    val = c;
    lrBeg = owner->code.size() + owner->begIdx;
    lrEnd = identifier;

}

jbi::Global::Global(Context* owner, Constant c) : owner(owner) {

    identifier = -1 - owner->variables.size();
    val = c;

}

jbi::Global::Global(Context* owner, Global* g) : owner(owner) {

    identifier = -1 - owner->variables.size();
    val = g->val;

}

jbi::Instruction* jbi::CodeBlock::addInstruction(Opcode op, Variable** args, U8 nargs) {

    auto resulttype = inferType(op, args, nargs);
    auto inst = new Instruction(op, resulttype, args, nargs);
    code.push_back(inst);

    return inst;

}

jbi::Variable* jbi::CodeBlock::addVariable(Type type) {

    auto var = new Local(this, type);
    variables->push_back(var);

    return var;

}

jbi::Variable* jbi::CodeBlock::addConstant(jitbox::Constant c) {

    auto var = new Local(this, c);
    variables->push_back(var);

    return var;

}

void jbi::CodeBlock::addAssignment(jbi::Variable* lh, jitbox::Instruction in) {

    //infertype(in->op, &lh, -1);
    in->args[instructionLengths[in->op] - 1] = lh->identifier;

}

I32 fArgIndex;
jbi::CodeBlock* nextFunc;

Type jbi::CodeBlock::inferType(Opcode op, Variable** args, U8 nargs) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG

    for (I32 i = 0; i < nargs; i++) {

        auto arg = args[i];
        auto owner = arg->isLocal() ? arg->toLocal()->owner->owner : arg->toGlobal()->owner;

        if (owner != ctx) throw Error(Error::CONTEXT_MISMATCH);

    }

#endif

    Type restrictions = Type::ANYTHING;
    Type rettype;
    Bool enforceMatchingTypes = false;

    switch (op) {
    case Opcode::MOV:

        rettype = args[0]->val.type;
        break;

    case Opcode::EXIT:

        restrictions = Type::U8;
        break;

    case Opcode::RETV:

        break;

    case Opcode::RET:

        restrictions = ctx->curFunction->type.getSignature().returnType;
        break;

    case Opcode::FARG:

        restrictions = nextFunc->type.getSignature().argTypes[fArgIndex];
        break;

    case Opcode::CALL:

#ifdef JITBOX_DEBUG
        if (nextFunc->owner != ctx) throw Error(Error::CONTEXT_MISMATCH);
#endif

        rettype = nextFunc->type.getSignature().returnType;
        break;

    case Opcode::ADD:
        
        restrictions = Type::PRIMITIVE;
        rettype = args[0]->val.type;
        enforceMatchingTypes = true;

        break;

    }

#ifdef JITBOX_DEBUG

    for (I32 i = 0; i < nargs; i++)
        if (!args[i]->val.type.is(restrictions)) throw Error(Error::INVALID_ARGUMENT);

    if (enforceMatchingTypes) for (I32 i = 0; i < nargs - 1; i++)
        if (args[i]->val.type != args[i + 1]->val.type) throw Error(Error::TYPE_MISMATCH);

#endif

    return rettype;

}

jbi::CodeBlock::CodeBlock(Context* owner, U8 blockType, Type type, ID identifier) : CompilerObject(CompilerObject::CODEBLOCK), owner(owner), blockType(blockType), type(type), identifier(identifier) { 
    
    variables = new Vec<pVariable>();
    curScope = this;

}

jbi::CodeBlock::~CodeBlock() { if (blockType == FUNCTION) delete variables; }

String jbi::CodeBlock::toString(U8 alignment) const {

    String s;
    
    if (blockType == FUNCTION) {

        s = "FUNCTION:\n\n";
        
        for (auto c : code) {

            auto in = c.cast<jbi::Instruction>()();

            s += indent(alignment + 1) + instructionNames[in->op] + ": ";

            I32 nargs = instructionLengths[in->op];

            for (I32 i = 0; i < nargs; i++)
                s += (
                    in->args[i] > 0
                        ? (*variables)[in->args[i] - 1]->toString(0)
                        : owner->variables[-1 - in->args[i]]->toString(0)
                ) + (i < nargs - 1 ? ", " : "");

            s += "\n";

        }

        s += "\n";

    }

    /*else if (CType == conditionalblock) {

        I32 codelen = code->size();
        for (I32 i = 0; i < codelen; i++) s += (*code)[i]->print(alignment, i != codelen - 1);

    }

    else {

        if (CType == whileblock) s += "while:\n\n";
        else if (CType == ifblock) s += "if:\n\n";
        else s += "else:\n\n";

        for (auto c : *code) s += c->print(alignment + 1);

    }*/

    return s;

}

jbi::CodeBlock* jbi::Context::addFunction(const jitbox::Type::List& ArgTypes, Type returntype) {

    Signature sig;

    sig.identifier = functions.size() + 1;
    sig.argTypes = ArgTypes;
    sig.returnType = returntype;

    auto f = new CodeBlock(this, CodeBlock::FUNCTION, Type::Function(sig), sig.identifier);
    functions.push_back(f);

    return f;

}

jbi::Variable* jbi::Context::addGlobalVariable(jbi::Variable* v) {

    auto ctx = jbi::Context::curContext();
    
    auto newvar = new Global(ctx, v->toGlobal());
    ctx->variables.push_back(newvar);

    return newvar;

}

jbi::Variable* jbi::Context::addGlobalVariable(jitbox::Constant c) {

    auto ctx = jbi::Context::curContext();

    auto newvar = new Global(ctx, c);
    ctx->variables.push_back(newvar);

    return newvar;

}

jbi::Context* jbi::Context::curContext() { return jitbox::Context::curContext; }

jbi::Variable* jbi::getInternalVar(VarRef v) { return v.v; }

jbi::CodeBlock* jbi::getInternalCode(Function f) { return f.definition; }

Constant jbi::fold(jbi::Instruction* in) {

    return ConstU32(0);

}

#define defconst(CType) \
Constant jitbox::Const##CType(CType val) { Constant c; c.v##CType = val; c.type = Type::CType; c.isConst = true; return c; }

defconst(U8);
defconst(I8);
defconst(U16);
defconst(I16);
defconst(U32);
defconst(I32);
defconst(U64);
defconst(I64);
defconst(F32);
defconst(F64);

jbi::Context* Context::curContext = nullptr;

Context::Context(Bool active) { ctx = new jbi::Context(); if (active) use(); }

void Context::use() const { curContext = ctx(); }

Context::~Context() { curContext = nullptr; }

Var::Var(const Var& v) { assign(v); }
Var::Var(const Constant& c) { assign(c); }
Var::Var(Instruction in) { assign(in); }

void Var::operator = (const Var& v) { assign(v); }
void Var::operator = (const Constant& c) { assign(c); }
void Var::operator = (Instruction in) { assign(in); }

Type Var::getType() const { return v->val.type; }

String Var::toString(U8 alignment) const { return v->toString(alignment); }

void Var::assign(const Var& rh) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

    if (f) {

        auto scope = f->curScope;

        if (!v || v->identifier > 0) v = scope->addVariable(rh.getType());

        static jbi::Variable* argsbuf[2];
        argsbuf[0] = rh.v;
        argsbuf[1] = v;

        scope->addInstruction(
            jbi::Opcode::MOV,
            argsbuf,
            2
        );

    }

    else {

#ifdef JITBOX_DEBUG
        if (v) throw Error(Error::GLOBAL_REDECLARATION);
#endif

        v = ctx->addGlobalVariable(rh.v);

    }

}

void Var::assign(const Constant& c) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

    if (f) {

        auto scope = f->curScope;

#ifdef JITBOX_DEBUG
        if (!scope) throw Error(Error::NO_ACTIVE_SCOPE);
#endif

        auto newconst = scope->addConstant(c);

        if (v && v->identifier < 0) {

            static jbi::Variable* argsbuf[2];
            argsbuf[0] = newconst;
            argsbuf[1] = v;

            scope->addInstruction(
                jbi::Opcode::MOV,
                argsbuf,
                2
            );

        }
        else v = newconst;

    }
    else {

#ifdef JITBOX_DEBUG
        if (v) throw Error(Error::GLOBAL_REDECLARATION);
#endif

        v = ctx->addGlobalVariable(c);

    }

}

void Var::assign(Instruction in) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

#ifdef JITBOX_DEBUG

    if (!f) for (I32 i = 0; i < jbi::instructionLengths[in->op]; i++)
        if (in->args[i] > 0) throw Error(Error::NO_ACTIVE_FUNCTION);

    if (in->args[jbi::instructionLengths[in->op] - 1] != 0) throw Error(Error::INVALID_ASSIGNMENT);

#endif

    auto scope = f->curScope;

#ifdef JITBOX_DEBUG
    if (!scope) throw Error(Error::NO_ACTIVE_SCOPE);
#endif

    if (!v || v->identifier > 0) v = scope->addVariable(in->type);
    scope->addAssignment(v, in);

}

VarRef::VarRef(const Var& v) : v(v.v) { }

VarRef::VarRef(const Constant& c) {

    Var newVar(c);
    v = newVar.v;

}

VarRef::VarRef(Instruction in) {

    Var newVar(in);
    v = newVar.v;

}

Function::Function(const Type::List& argTypes, Type returnType, Bool active) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    definition = ctx->addFunction(argTypes, returnType);

    if (active) ctx->curFunction = definition;

}

void Function::use() { }

Type Function::getType() const { return definition->type; }

String Function::toString(U8 alignment) const { return definition->toString(alignment); }

void jitbox::Terminate(VarRef exitcode) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

#ifdef JITBOX_DEBUG
    if (!f) throw Error(Error::NO_ACTIVE_FUNCTION);
#endif

    auto scope = f->curScope;

#ifdef JITBOX_DEBUG
    if (!scope) throw Error(Error::NO_ACTIVE_SCOPE);
#endif

    static jbi::Variable* argsbuf[1];
    argsbuf[0] = jbi::getInternalVar(exitcode);

    scope->addInstruction(
        jbi::Opcode::EXIT,
        argsbuf,
        1
    );

}

void jitbox::Return() {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

#ifdef JITBOX_DEBUG
    if (!f) throw Error(Error::NO_ACTIVE_FUNCTION);
#endif

    auto scope = f->curScope;

#ifdef JITBOX_DEBUG
    if (!scope) throw Error(Error::NO_ACTIVE_SCOPE);
#endif

    scope->addInstruction(
        jbi::Opcode::RETV,
        nullptr,
        0
    );

}

void jitbox::Return(VarRef v) {
    
    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

#ifdef JITBOX_DEBUG
    if (!f) throw Error(Error::NO_ACTIVE_FUNCTION);
#endif

    auto scope = f->curScope;

#ifdef JITBOX_DEBUG
    if (!scope) throw Error(Error::NO_ACTIVE_SCOPE);
#endif
    
    static jbi::Variable* argsbuf[1];
    argsbuf[0] = jbi::getInternalVar(v);

    scope->addInstruction(
        jbi::Opcode::RET,
        argsbuf,
        1
    );

}

Instruction jitbox::Call(Function func) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

#ifdef JITBOX_DEBUG
    if (!f) throw Error(Error::NO_ACTIVE_FUNCTION);
#endif

    auto scope = f->curScope;

#ifdef JITBOX_DEBUG
    if (!scope) throw Error(Error::NO_ACTIVE_SCOPE);
#endif

    static jbi::Variable* argsbuf[2];

    nextFunc = jbi::getInternalCode(func);

    auto v = scope->addConstant(ConstI32(jbi::getInternalCode(func)->identifier));
    argsbuf[0] = v;
    argsbuf[1] = 0;

    return scope->addInstruction(
        jbi::Opcode::CALL,
        argsbuf,
        1
    );

}

Instruction jitbox::Call(Function func, const VarList& args) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

#ifdef JITBOX_DEBUG
    if (!f) throw Error(Error::NO_ACTIVE_FUNCTION);
#endif

    auto scope = f->curScope;

#ifdef JITBOX_DEBUG
    if (!scope) throw Error(Error::NO_ACTIVE_SCOPE);
#endif

    static jbi::Variable* argsbuf[2];

    fArgIndex = 0;
    nextFunc = jbi::getInternalCode(func);

    for (auto arg : args) {

        argsbuf[0] = jbi::getInternalVar(arg);
        scope->addInstruction(
            jbi::Opcode::FARG,
            argsbuf,
            1
        );

        fArgIndex++;

    }

    auto v = scope->addConstant(ConstI32(nextFunc->identifier));
    argsbuf[0] = v;
    argsbuf[1] = 0;

    return scope->addInstruction(
        jbi::Opcode::CALL,
        argsbuf,
        1
    );

}

Instruction jitbox::Cast(VarRef src, Type to) {

    throw Error(Error::OK);

}

Instruction jitbox::Add(VarRef lh, VarRef rh) {

    auto ctx = jbi::Context::curContext();

#ifdef JITBOX_DEBUG
    if (!ctx) throw Error(Error::NO_ACTIVE_CONTEXT);
#endif

    auto f = ctx->curFunction;

#ifdef JITBOX_DEBUG
    if (!f) throw Error(Error::NO_ACTIVE_FUNCTION);
#endif

    auto scope = f->curScope;

#ifdef JITBOX_DEBUG
    if (!scope) throw Error(Error::NO_ACTIVE_SCOPE);
#endif

    static jbi::Variable* argsbuf[3];
    argsbuf[0] = jbi::getInternalVar(lh);
    argsbuf[1] = jbi::getInternalVar(rh);
    argsbuf[2] = 0;

    return scope->addInstruction(
        jbi::Opcode::ADD,
        argsbuf,
        2
    );

}

void jitbox::End() {



}

const char* Errorcodes[] = {
    "no Error",
    "invalID type ID",
    "no active context",
    "no active FUNCTION",
    "no active scope",
    "Global redeclaration",
    "invalID argument",
    "type mismatch",
    "invalID assignment",
    "context mistmatch"
};

I32 main() {

    Context ctxt;

    try {

        Timer timer;
        
        Var globalfuck = ConstU32(100);
        Var globaldick = globalfuck;

        Function f({ Type::U32 }, Type::U32);

            Var x = ConstU32(420);
            Var y = x;
            Var z = Add(globaldick, y);
            globalfuck = z;
            globaldick = ConstU64(100);
            z = Add(z, z);

            Return(Add(z, globalfuck));

        End();
        
        std::cout << f.print();

        /*auto f = makeFUNCTION().ArgTypes({Type::u32}).rettype(Type::u8);

            makeif().condition(equal(getarg(0), constu32(2)));
                ret(constu8(1));
            end();

            makeif().condition(zero(mod(getarg(0), constu32(2))));
                ret(constu8(0));
            end();

            var k = constu32(2);

            makewhile().condition(lessthan(k, getarg(0)));
            
                makeif().condition(zero(mod(getarg(0), k)));
                    ret(constu8(0));
                end();

                k = add(k, constu32(2));

            end();
            
            ret(constu8(1));
            
        end();

        auto g = makeFUNCTION();

            var xyz = constu32(97);
            var holyshit = bitwiseand(xyz, constu32(911));
            xyz = add(xyz, xyz);

            call(f, {xyz});

            var zz = call(f, {xyz});

            call(g, {zz, zz});

            var rr = bitwisenot(zz);

            var count = constf64(0.0);

            makewhile().condition(lessthan(count, constf64(1000.0)));

                var tnuoc = count;

                makewhile().condition(lessthan(tnuoc, constf64(1000.0)));

                    tnuoc = add(tnuoc, constf64(1.0));

                end();

                count = constf64(-69.0);// add(count, constf64(1.0));

            end();

            ret(count);

            var aa = constu16(0); aa = div(aa, aa); // NOTE: reghI32 should not be generated if there is a FUNCTION call inbetween the var's creation and the var's usage as an argument
            var bb = aa;                            // NOTE: check to make sure that FUNCTION arguments are able to be marked as persistent, also probably remove reghints for FUNCTION arguments
            var cc = bb;
            var dd = cc;
            var ee = dd;// ee = constu16(420);

            call(f, {&ee, &aa});

            ret(ee);

        end();

        //std::cout << f.print();
        //std::cout << g.print();

        auto h = makeFUNCTION().ArgTypes({Type::f32}).rettype(Type::f32);

            var x = add(getarg(0), constf32(420.69));

            makeif().condition(equal(x, constf32(0.0)));

                ret(getarg(0));

            makeelse();

                ret(x);

            end();

        end();

        //std::cout << h.print();

        var glob = makeGlobalVar(constf32(69.0)); // Error: This constructor is called twice! Why???

        auto globtest = makeFUNCTION();

            var xq = glob;
            glob = xq;
            glob = glob;

            xq = xq;

            glob = call(globtest, {&xq, &glob});

            //makewhile().condition(lessthan(glob, constf32(9000.0)));

                xq = add(xq, xq);
                xq = add(xq, xq);
                xq = add(xq, xq);

            //end();

        end();

        //std::cout << globtest.print();
        compile();
        std::cout << "Took: " << timer.time() << "\n";
        */

    }
    catch (Error e) { std::cout << "you fucked up: " << Errorcodes[(int)e.code] << "\n"; return 1; }

    return 0;

}
/*
String address::toString(I32 alignment) const {

    if (addr == 0) return "null";

    uint32_t type = addr % 4;
    uint32_t mode = (addr >> 2) % 4;
    uint32_t bytewIDth = (addr >> 4) % 4;
    uint32_t rhI32 = (addr >> 6) % 8;
    uint32_t IDx = addr >> 9;

    String s;

    static const char types[] = { 'i', 'u', 'f' };
    s += types[type];
    s += std::to_String(8 << bytewIDth);
    s += ':';

    static const char* modes[] = { "tmp", "prs", "imm", "gbl" };
    s += modes[mode];

    if (mode == addressmode::immediate) s += ":" + val.toStringsimple(false);
    else s += ':' + std::to_String(IDx);

    if (rhI32 > 0) s += "(" + std::to_String(rhI32 - 4) + ")";

    return s;

}

String bytecode::toString(I32 alignment) const {

    String s = "bytecode:\n\n";
    auto i = iterate();

    while (!i.done()) {

        s += indent(alignment + 1) + std::to_String(i.IDx) + ".";
        auto in = i.nextInstruction();
        std::cout << "in I32 = " << (int)in << "\n";
        s += instnames[(uint32_t)in];
        s += "\n";
        std::cout << "inst length = " << instlengths[(uint32_t)in] << "\n";
        for (I32 _ = 0; _ < instlengths[(uint32_t)in]; _++) {

            auto addr = i.nextaddress();
            s += addr.print(alignment + 2);

        }

    }

    return s;

}

String internalCodeBlock::toString(I32 alignment) const {

    String s = "\n" + indent(alignment);
    
    if (CType == FUNCTIONblock) {

        auto func = (FUNCTIONCodeBlock*)this;

        s = String("FUNCTION") + (func->isinline ? " (inline)" : "") +  ":\n\n";
        s += indent(alignment + 1) + "arguments:\n\n";

        I32 numargs = func->args.size();
        for (I32 i = 0; i < numargs; i++) {

            s += indent(alignment + 2) + "variable T" + std::to_String(func->args[i].addr) + (func->passbyref[i] ? " (passed by reference)" : "") + " with dType:\n\n";
            s += func->args[i].type.print(alignment + 3) + "\n";

        }

        s += indent(alignment + 1) + "return type" + (func->returnref ? " (returns reference)" : "") + ":\n\n";
        s += type(func->sig.returntype).print(alignment + 2) + "\n";

        s += indent(alignment + 1) + "*** begin code ***\n\n";

        for (auto c : *code) s += c->print(alignment + 2);

        s += indent(alignment + 1) + "*** end code ***\n";

    }

    else if (CType == conditionalblock) {

        I32 codelen = code->size();
        for (I32 i = 0; i < codelen; i++) s += (*code)[i]->print(alignment, i != codelen - 1);

    }

    else {

        if (CType == whileblock) s += "while:\n\n";
        else if (CType == ifblock) s += "if:\n\n";
        else s += "else:\n\n";

        for (auto c : *code) s += c->print(alignment + 1);

    }

    return s;

}

String internalInstruction::toString(I32 alignment) const {

    String s;

    if (lh) s += pad("T" + std::to_String(lh->addr), 4) + " =   ";
    else s += "?    =   ";
    
    s += pad(instnames[(int)Opcode], 6) + " ";

    for (I32 i = 0; i < nargs; i++)
        s += pad("T" + std::to_String(args[i]->addr), 4) + "  ";

    return s;

}

String internalvariable::toString(I32 alignment) const {

    if (rh) return "";
    return pad("T" + std::to_String(addr), 4) + " =   " + value.toString(0);

}

#define clearMaps(TheType) \
\
TheType##kvMap.clear(); \
TheType##vkMap.clear(); \
TheType##nextFreeID = Type::TheType + incrementID

Context::~context() {

    clearMaps(POINTER);
    clearMaps(ARRAY);
    clearMaps(STRUCTURE);
    clearMaps(FUNCTION);

    curContext = nullptr;

}
*/