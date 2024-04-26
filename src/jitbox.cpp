#include <iostream>
#include "./jitbox.hpp"
#include "./Timer.hpp"

using namespace jitbox;
namespace jbi = _jitbox; // short for 'jitbox internal'

#define indentchar " "

string printable::print(int alignment, bool addnewline) const { return indent(alignment) + this->tostring(alignment) + (addnewline ? "\n" : ""); }

printable::~printable() { }

string printable::indent(int nind) {

    if (nind == 0) return " ";

    string ind;
    for (int i = 0; i < nind - 1; i++) ind += indentchar "  ";
    return " " + ind + indentchar " ";

}

string printable::pad(string str, int padnum) {

    int reqspace = padnum - (int)str.size();
    if (reqspace < 0) return str.substr(0, padnum - 3) + "...";

    for (int i = 0; i < reqspace; i++) str += " ";
    return str;

}

#ifdef JITBOX_DEBUG

error::error(errorcode code) : code(code) { }

#endif

inline bool signature::operator < (const signature& rh) const {
     
    if (identifier < rh.identifier) return true;
    if (identifier > rh.identifier) return false;
    if (returntype < rh.returntype) return true;
    if (returntype > rh.returntype) return false;

    return types < rh.types;

}

string signature::tostring(int alignment) const {

    string s = "identifier:\n" + indent(alignment + 1) + std::to_string(identifier) + "\n";
    s += indent(alignment) + "argument types:\n";

    int numargs = types.size();
    for (int i = 0; i < numargs; i++) s += type(types[i]).print(alignment + 1, i != numargs - 1);

    s += indent(alignment) + "return type\n";
    s += type(returntype).print(alignment + 1, false);

    return s;

}

const id incrementid = 4;

template <typename keytype>
inline id getvaluefromkey(map<keytype, id>& kvmap, map<id, keytype>& vkmap, id& nextfreeid, const keytype& key) {

    auto findmatch = kvmap.find(key);

    if (findmatch == kvmap.end()) {

        kvmap[key] = nextfreeid;
        vkmap[nextfreeid] = key;

        nextfreeid += incrementid;
        return nextfreeid - incrementid;

    }

    return findmatch->second;

}

template <typename keytype>
inline const keytype* getkeyfromvalue(map<id, keytype>& vkmap, id id) {

    auto findmatch = vkmap.find(id);

    if (findmatch == vkmap.end()) return nullptr;
    return &findmatch->second;

}

#define implementmaps(thetype, K, V) \
\
map<K, V> thetype##kvmap; \
map<V, K> thetype##vkmap; \
id thetype##nextfreeid = type::thetype + incrementid

implementmaps(pointer, id, id);
implementmaps(array, id, id);
implementmaps(structure, signature, id);
implementmaps(function, signature, id);

type::type() { }

#define validatetypeid(thetype, keytype) \
\
case type::thetype: \
    \
    if (!getkeyfromvalue<keytype>(thetype##vkmap, mid)) throw error(error::invalidtypeid); \
    break;

type::type(id mid) : mid(mid) { 
    
    determineclass();

#ifdef JITBOX_DEBUG
    
    switch (mclass) {

        validatetypeid(pointer, id);
        validatetypeid(array, id);
        validatetypeid(structure, signature);
        validatetypeid(function, signature);

    }

#endif

}

bool jitbox::operator == (const type& lh, id rh) { return lh.getid() == rh; }

bool jitbox::operator == (const type& lh, const type& rh) { return lh.getid() == rh.getid(); }

bool jitbox::operator == (id lh, const type& rh) { return lh == rh.getid(); }

bool jitbox::operator != (const type& lh, id rh) { return lh.getid() != rh; }

bool jitbox::operator != (const type& lh, const type& rh) { return lh.getid() != rh.getid(); }

bool jitbox::operator != (id lh, const type& rh) { return lh != rh.getid(); }

#define maketype(thetype, argtype, fullargtype) \
type type::make##thetype(fullargtype arg) { return type(getvaluefromkey<argtype>(thetype##kvmap, thetype##vkmap, thetype##nextfreeid, arg)); }

maketype(pointer, id, id);
maketype(array, id, id);
maketype(structure, signature, const signature&);
maketype(function, signature, const signature&);

#define gettype(thetype, keytype, rettype) \
\
rettype type::get##thetype##type() const { return *getkeyfromvalue<keytype>(thetype##vkmap, mid); }

gettype(pointer, id, id);
gettype(array, id, id);
gettype(structure, signature, const signature&);
gettype(function, signature, const signature&);

bool type::is(type t) const {

    if (t.mid == anything) return true;
    if (mid == t.mid) return true;
    if (mclass != t.mclass) return false;

    if (mclass == primitive && t.mid == primitive) return true;

    if (mclass == pointer) {

        if (t.mid == pointer) return true;
        return type(getpointertype()).is(t.getpointertype());

    }

    if (mclass == array) {

        if (t.mid == array) return true;
        return type(getarraytype()).is(t.getarraytype());

    }

    if (mclass == structure || mclass == function) {

        if (t.mid == structure || t.mid == function) return true;

        const signature* lh, * rh;

        if (mclass == structure) { lh = &getstructuretype(); rh = &t.getstructuretype(); }
        else { lh = &getfunctiontype(); rh = &t.getfunctiontype(); }

        if (lh->identifier != rh->identifier) return false;

        for (int k = 0; k < lh->types.size(); k++)
            if (!type(lh->types[k]).is(rh->types[k])) return false;

        return true;

    }

    return false;

}

bool type::is(id i) const { return is(type(i)); }

id type::getid() const { return mid; }

id type::getclass() const { return mclass; }

bool type::isconcrete() const {

    if (mid == nothing) return true;
    if (mid == anything) return false;
    if (mclass == primitive) return mid != primitive;

    if (mclass == pointer) {

        if (mid == pointer) return false;
        return type(getpointertype()).isconcrete();

    }

    if (mclass == array) {

        if (mid == array) return false;
        return type(getarraytype()).isconcrete();

    }

    if (mclass == structure || mclass == function) {

        if (mid == mclass) return false;

        auto& sig = (mclass == structure) ? getstructuretype() : getfunctiontype();

        for (int k = 0; k < sig.types.size(); k++)
            if (!type(sig.types[k]).isconcrete()) return false;

        return true;

    }

    return false;

}

bool type::isabstract() const { return !isconcrete(); }

bool type::isinteger() const { return mid >= i8 && mid <= u64; }

bool type::isunsigned() const { return mid >= u8 && mid <= u64; }

bool type::isfp() const { return mid == f32 || mid == f64; }

int type::numbytes() const {

    if (mclass == primitive) {

        if (mid == i8 || mid == u8) return 1;
        if (mid == i16 || mid == u16) return 2;
        if (mid == i32 || mid == u32 || mid == f32) return 4;
        return 8;

    }

    if (mclass == array) return -1;

    if (mclass == structure || mclass == function) {

        auto& sig = (mclass == structure) ? getstructuretype() : getfunctiontype();
        int b = 0;

        for (int k = 0; k < sig.types.size(); k++) b += type(sig.types[k]).numbytes();

        return b;

    }

    if (mclass == nothing) return 0;

    return 8;

}

void type::determineclass() {

    if (mid == nothing) mclass = nothing;
    else if (mid == anything) mclass = anything;
    else if (mid >= primitive && mid < pointer) mclass = primitive;
    else if ((mid - pointer) % 4 == 0) mclass = pointer;
    else if ((mid - array) % 4 == 0) mclass = array;
    else if ((mid - structure) % 4 == 0) mclass = structure;
    else if ((mid - function) % 4 == 0) mclass = function;

}

string type::tostring(int alignment) const {

    if (mid == nothing) return "nothing";

    if (mid == anything) return "anything";

    if (mclass == primitive) {

        static const char* primitives[] = { "primitive", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64" };
        return primitives[mid - primitive];

    }

    if (mclass == array) return "array of:\n" + type(getarraytype()).print(alignment + 1, false);

    if (mclass == pointer) return "pointer to:\n" + type(getpointertype()).print(alignment + 1, false);

    if (mclass == function) return "function:\n" + getfunctiontype().print(alignment + 1, false);

    if (mclass == structure) return "structure:\n" + getstructuretype().print(alignment + 1, false);

    return "unknown type";

}

bool constant::operator == (const constant& ctc) {

    if (dtype != ctc.dtype) return false;

    auto bw = type(dtype).numbytes();

    if (bw == 1) return vu8 == ctc.vu8;
    if (bw == 2) return vu16 == ctc.vu16;
    if (bw == 4) return vu32 == ctc.vu32;
    return vu64 == ctc.vu64;

}

bool constant::operator != (const constant& ctc) { return !(*this == ctc); }

#define ctctostringcase(thetype) else if (dtype == type::thetype) s = std::to_string(v##thetype)

string constant::tostring(int) const {

    string s = "unknown";

    if (type(dtype).isinteger()) {

        if (false);
        ctctostringcase(u8);
        ctctostringcase(i8);
        ctctostringcase(u16);
        ctctostringcase(i16);
        ctctostringcase(u32);
        ctctostringcase(i32);
        ctctostringcase(u64);
        ctctostringcase(i64);

    }
    else {

        if (dtype == type::f32) s = std::to_string(vf32);
        else if (dtype == type::f64) s = std::to_string(vf64);

    }

    return s;

}

const u8 _jitbox::instructionlengths[] = {

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

const char* _jitbox::instructionnames[] = {

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

jbi::compilerobject::compilerobject(jbi::compilerobject::cotype type) : type(type) { }

jbi::instruction::instruction(opcode op, ssaid* srcargs) : compilerobject(compilerobject::cotype::instruction), op(op) { std::memcpy(args, srcargs, sizeof(ssaid) * instructionlengths[op]); }

jbi::variable::variable() : compilerobject(compilerobject::cotype::variable) { }

jbi::variable::variable(codeblock* owner, jitbox::type dtype) : compilerobject(compilerobject::cotype::variable), owner(owner) {
    
    id = owner->variables->size();
    lrbeg = owner->code.size() + owner->begidx;
    lrend = id;
    val.dtype = dtype;

}

jbi::variable::variable(codeblock* owner, jitbox::constant c) : compilerobject(compilerobject::cotype::variable), owner(owner), val(c) {

    id = owner->variables->size();
    lrbeg = owner->code.size() + owner->begidx;
    lrend = id;

}

jbi::instruction* jbi::codeblock::addinstruction(opcode op, ssaid* args) {

    auto inst = new instruction(op, args);
    code.push_back(inst);

    return inst;

}

jbi::variable* jbi::codeblock::addvariable(jitbox::type vtype) {

    auto var = new variable(this, vtype);
    variables->push_back(var);

    return var;

}

jbi::variable* jbi::codeblock::addconstant(jitbox::constant c) {

    auto var = new variable(this, c);
    variables->push_back(var);

    return var;

}

jbi::codeblock::codeblock(cbtype blocktype) : compilerobject(compilerobject::cotype::codeblock), blocktype(blocktype) { if (blocktype == cbtype::function) variables = new vec<pvariable>(); }

jbi::codeblock::~codeblock() { if (blocktype == cbtype::function) delete variables; }

jbi::context* jbi::context::curcontext() { return jitbox::context::curcontext; }

#define defconst(ctype) \
constant jitbox::const##ctype(ctype val) { constant c; c.v##ctype = val; c.dtype = type::ctype; c.isconst = true; return c; }

defconst(u8);
defconst(i8);
defconst(u16);
defconst(i16);
defconst(u32);
defconst(i32);
defconst(u64);
defconst(i64);
defconst(f32);
defconst(f64);

jbi::context* context::curcontext = nullptr;

context::context(bool active) { curcontext = new jbi::context(); if (active) use(); }

void context::use() const { curcontext = ctx(); }

context::~context() { curcontext = nullptr; }

var::var(const var& v) { assign(v); }
var::var(const constant& c) { assign(c); }
var::var(instruction in) { assign(in); }

void var::operator = (const var& v) { assign(v); }
void var::operator = (const constant& c) { assign(c); }
void var::operator = (instruction in) { assign(in); }

type var::getdtype() const { return v->val.dtype; }

ssaid var::getssaid() const { return v->id; }

string var::tostring(int alignment) const {

        

}

void var::assign(const var&) {


}

void var::assign(const constant&) {


}

void var::assign(instruction) {


}

void function::use() { }

type function::getdtype() const { return definition->dtype; }

function& function::argtypes(const typelist& types) {



}

function& function::rettype(type rtype) {



}

string function::tostring(int alignment) const {
    


}

function::function(_jitbox::codeblock* definition, bool active) : definition(definition) { }

void jitbox::terminate(var exitcode) {

    auto ctx = jbi::context::curcontext();

#ifdef JITBOX_DEBUG

    if (ctx) throw error(error::noactivecontext);

#endif

    auto scope = ctx->curscope;

#ifdef JITBOX_DEBUG

    if (!scope) throw error(error::noactivescope);

#endif

    static ssaid argsbuf[1];
    argsbuf[0] = exitcode.getssaid();

    scope->addinstruction(
        jbi::opcode::exit, argsbuf
    );

}

void jitbox::ret() {

    auto ctx = jbi::context::curcontext();

#ifdef JITBOX_DEBUG

    if (ctx) throw error(error::noactivecontext);

#endif

    auto scope = ctx->curscope;

#ifdef JITBOX_DEBUG

    if (!scope) throw error(error::noactivescope);

#endif

    scope->addinstruction(
        jbi::opcode::retv, nullptr
    );

}

void jitbox::ret(var v) {

    auto ctx = jbi::context::curcontext();

#ifdef JITBOX_DEBUG

    if (ctx) throw error(error::noactivecontext);

#endif

    auto scope = ctx->curscope;

#ifdef JITBOX_DEBUG

    if (!scope) throw error(error::noactivescope);

#endif

    static ssaid argsbuf[1];
    argsbuf[0] = v.getssaid();

    scope->addinstruction(
        jbi::opcode::ret, argsbuf
    );

}

instruction jitbox::call(function func) {

    auto ctx = jbi::context::curcontext();

#ifdef JITBOX_DEBUG

    if (ctx) throw error(error::noactivecontext);

#endif

    auto scope = ctx->curscope;

#ifdef JITBOX_DEBUG

    if (!scope) throw error(error::noactivescope);

#endif

    static ssaid argsbuf[2];

    auto v = scope->addconstant(consti32(func.getid()));
    argsbuf[0] = v->id;
    argsbuf[1] = 0;

    return scope->addinstruction(
        jbi::opcode::call, argsbuf
    );

}

instruction jitbox::call(function func, const varlist& args) {

    auto ctx = jbi::context::curcontext();

#ifdef JITBOX_DEBUG

    if (ctx) throw error(error::noactivecontext);

#endif

    auto scope = ctx->curscope;

#ifdef JITBOX_DEBUG

    if (!scope) throw error(error::noactivescope);

#endif

    static ssaid argsbuf[2];

    for (const auto& arg : args) {

        argsbuf[0] = arg.getssaid();
        scope->addinstruction(
            jbi::opcode::farg, argsbuf
        );

    }

    auto v = scope->addconstant(consti32(func.getid()));
    argsbuf[0] = v->id;
    argsbuf[1] = 0;

    return scope->addinstruction(
        jbi::opcode::call, argsbuf
    );

}

instruction jitbox::cast(var src, type to) {

    throw error(error::ok);

}

instruction jitbox::add(var lh, var rh) {

    auto ctx = jbi::context::curcontext();

#ifdef JITBOX_DEBUG

    if (ctx) throw error(error::noactivecontext);

#endif

    auto scope = ctx->curscope;

#ifdef JITBOX_DEBUG

    if (!scope) throw error(error::noactivescope);

#endif

    static ssaid argsbuf[3];
    argsbuf[0] = lh.getssaid();
    argsbuf[1] = rh.getssaid();
    argsbuf[2] = 0;

    scope->addinstruction(
        jbi::opcode::add, argsbuf
    );

}

int main() {

    context ctxt;

    try {

        Timer timer;

        auto f = makefunction().argtypes({type::u32}).rettype(type::u8);

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

        auto g = makefunction();

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

            var aa = constu16(0); aa = div(aa, aa); // NOTE: reghint should not be generated if there is a function call inbetween the var's creation and the var's usage as an argument
            var bb = aa;                            // NOTE: check to make sure that function arguments are able to be marked as persistent, also probably remove reghints for function arguments
            var cc = bb;
            var dd = cc;
            var ee = dd;// ee = constu16(420);

            call(f, {&ee, &aa});

            ret(ee);

        end();

        //std::cout << f.print();
        //std::cout << g.print();

        auto h = makefunction().argtypes({type::f32}).rettype(type::f32);

            var x = add(getarg(0), constf32(420.69));

            makeif().condition(equal(x, constf32(0.0)));

                ret(getarg(0));

            makeelse();

                ret(x);

            end();

        end();

        //std::cout << h.print();

        var glob = makeglobalvar(constf32(69.0)); // ERROR: This constructor is called twice! Why???

        auto globtest = makefunction();

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

    } catch (error e) { std::cout << e.code << "\n"; return 1; }

    return 0;

}
/*
string address::tostring(int alignment) const {

    if (addr == 0) return "null";

    uint32_t dtype = addr % 4;
    uint32_t mode = (addr >> 2) % 4;
    uint32_t bytewidth = (addr >> 4) % 4;
    uint32_t rhint = (addr >> 6) % 8;
    uint32_t idx = addr >> 9;

    string s;

    static const char dtypes[] = { 'i', 'u', 'f' };
    s += dtypes[dtype];
    s += std::to_string(8 << bytewidth);
    s += ':';

    static const char* modes[] = { "tmp", "prs", "imm", "gbl" };
    s += modes[mode];

    if (mode == addressmode::immediate) s += ":" + val.tostringsimple(false);
    else s += ':' + std::to_string(idx);

    if (rhint > 0) s += "(" + std::to_string(rhint - 4) + ")";

    return s;

}

string bytecode::tostring(int alignment) const {

    string s = "bytecode:\n\n";
    auto i = iterate();

    while (!i.done()) {

        s += indent(alignment + 1) + std::to_string(i.idx) + ".";
        auto in = i.nextinstruction();
        std::cout << "in int = " << (int)in << "\n";
        s += instnames[(uint32_t)in];
        s += "\n";
        std::cout << "inst length = " << instlengths[(uint32_t)in] << "\n";
        for (int _ = 0; _ < instlengths[(uint32_t)in]; _++) {

            auto addr = i.nextaddress();
            s += addr.print(alignment + 2);

        }

    }

    return s;

}

string internalcodeblock::tostring(int alignment) const {

    string s = "\n" + indent(alignment);
    
    if (ctype == functionblock) {

        auto func = (functioncodeblock*)this;

        s = string("function") + (func->isinline ? " (inline)" : "") +  ":\n\n";
        s += indent(alignment + 1) + "arguments:\n\n";

        int numargs = func->args.size();
        for (int i = 0; i < numargs; i++) {

            s += indent(alignment + 2) + "variable T" + std::to_string(func->args[i].addr) + (func->passbyref[i] ? " (passed by reference)" : "") + " with dtype:\n\n";
            s += func->args[i].dtype.print(alignment + 3) + "\n";

        }

        s += indent(alignment + 1) + "return type" + (func->returnref ? " (returns reference)" : "") + ":\n\n";
        s += type(func->sig.returntype).print(alignment + 2) + "\n";

        s += indent(alignment + 1) + "*** begin code ***\n\n";

        for (auto c : *code) s += c->print(alignment + 2);

        s += indent(alignment + 1) + "*** end code ***\n";

    }

    else if (ctype == conditionalblock) {

        int codelen = code->size();
        for (int i = 0; i < codelen; i++) s += (*code)[i]->print(alignment, i != codelen - 1);

    }

    else {

        if (ctype == whileblock) s += "while:\n\n";
        else if (ctype == ifblock) s += "if:\n\n";
        else s += "else:\n\n";

        for (auto c : *code) s += c->print(alignment + 1);

    }

    return s;

}

string internalinstruction::tostring(int alignment) const {

    string s;

    if (lh) s += pad("T" + std::to_string(lh->addr), 4) + " =   ";
    else s += "?    =   ";
    
    s += pad(instnames[(int)opcode], 6) + " ";

    for (int i = 0; i < nargs; i++)
        s += pad("T" + std::to_string(args[i]->addr), 4) + "  ";

    return s;

}

string internalvariable::tostring(int alignment) const {

    if (rh) return "";
    return pad("T" + std::to_string(addr), 4) + " =   " + value.tostring(0);

}

#define clearmaps(thetype) \
\
thetype##kvmap.clear(); \
thetype##vkmap.clear(); \
thetype##nextfreeid = type::thetype + incrementid

context::~context() {

    clearmaps(pointer);
    clearmaps(array);
    clearmaps(structure);
    clearmaps(function);

    curcontext = nullptr;

}
*/