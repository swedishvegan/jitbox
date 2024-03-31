
#include "./jitbox.hpp"
#include "./Timer.hpp"

using namespace jitbox;
#include <iostream>
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

            call(f, {&xyz});

            var zz = call(f, {&xyz});

            call(g, {&zz, &zz});

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

    } catch (exception e) { std::cout << e.message << "\n"; return 1; }

    return 0;

}

/*

TODO:

    Second pass:

        - Flatten nested code
        - Fold constants

            - Check if a constant is foldable --- if it's a loop variable then it's not
            - 

        - Insert rhint and free instructions
        - Insert jumps, both conditional and unconditional
        - Convert addresses to typed addresses & calculate whether each address is temp or saved
        - Fix instruction formats

*/


    /*

    T0 = arg(0)
    
    IF:

        CONDITION:

            je T0, imm(2)

        BODY:

            ret imm(1)

    IF:

        CONDITION:

            T1 = mod T0, 2
            jz T1

        BODY:

            ret imm(0)

    T2 = imm(2)

    WHILE:

        CONDITION:

            jge T2, T0

        BODY:

            IF:

                T3 = mod T0, T2
                jz T3

            BODY:

                ret imm(0)

            T4 = add T2, imm(2) // T4 is flagged as a dependency variable

    ret imm(1)

    - Each variable's live range is calculated
    - Once it reaches the end of its live range, if it has a curfew, write it back into its home

    SECOND PASS:

    - saved values = need to be preserved across function calls
    - temp values = no curfew 

    - Scan N instructions ahead
    - For each assignment:

        - If it's a mov assignment, simply set address(dstidx) = address(srcidx)
        - Otherwise, depending on use:
            - alloctemp(idx)
            - allocsaved(idx)

    - Once live range comes to an end:

        - restore if variable has curfew
        - depending on use:
            - freetemp(idx)
            - freesaved(idx)

        - Calculate the importance of each one, and do the following in order of importance:

            - If it's used as a function call argument, put it in the corresponding slot
            - Otherwise, if it's a saved value, assign it the next available saved slot
            - Otherwise, assign it the next available temp slot
            
    At runtime, temp(i) and saved(i) are resolved by the JIT.

        - SLJIT is told the maximum number of saved registers ever used at beginning of function
        - Each time a temp value is created, num_avail_saved is decremented if applicable
        - Each time a saved value is created, num_avail_temp is decremented if applicable

    TODO:

        - Compile-time constant evaluation
        - Context system
        - codeblock system
        - jitbox.hpp implementations
        - tostring implementations
        - disable copy constructors where applicable
        - Make sure macros don't fuck up intellisense
    
    */

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

#define _ctctostringcase(thetype) else if (dtype == type::thetype) s = ( printdtype ? #thetype ":" : "") + std::to_string(v##thetype)

string compiletimeconstant::tostringsimple(bool printdtype) const {

    string s = "unknown";

    if (type(dtype).isinteger()) {
        
        if (false);
        _ctctostringcase(u8);
        _ctctostringcase(i8);
        _ctctostringcase(u16);
        _ctctostringcase(i16);
        _ctctostringcase(u32);
        _ctctostringcase(i32);
        _ctctostringcase(u64);
        _ctctostringcase(i64);

    }
    else { 
        
        if (dtype == type::f32) s = (printdtype ? "f32:" : "") + std::to_string(vf32); 
        else if (dtype == type::f64) s = (printdtype ? "f64:" : "") + std::to_string(vf64); 
    
    }

    return s;

}

string compiletimeconstant::tostring(int alignment) const { return tostringsimple(true); }

context* context::curcontext = nullptr;

const uint32_t jitbox::instlengths[] = {

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
    0, // exit
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

const char* jitbox::instnames[] = {

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

string printable::print(int alignment, bool addnewline) const { return indent(alignment) + this->tostring(alignment) + (addnewline ? "\n" : ""); }

printable::~printable() { }

string printable::indent(int nind) {

	if (nind == 0) return " ";

	string ind;
	for (int i = 0; i < nind - 1; i++) ind += INDENT_CHAR "  ";
	return " " + ind + INDENT_CHAR " ";

}

string printable::pad(string str, int padnum) {

	int reqspace = padnum - (int)str.size();
	if (reqspace < 0) return str.substr(0, padnum - 3) + "...";

	for (int i = 0; i < reqspace; i++) str += " ";
	return str;

}

printablestring::printablestring(string str) : str(str) { }
printablestring::printablestring(const char* str) : str(str) { }

string printablestring::tostring(int) const { return str; }

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

inline bool signature::operator<(const signature& rh) const {

	if (identifier < rh.identifier) return true;
	if (identifier > rh.identifier) return false;
	if (returntype < rh.returntype) return true;
	if (returntype > rh.returntype) return false;

	return types < rh.types;

}

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
inline const keytype& getkeyfromvalue(map<id, keytype>& vkmap, id id, bool& error) {

	static keytype dummy{ };

	auto findmatch = vkmap.find(id);

	if (findmatch == vkmap.end()) { error = true; return dummy; }
	return findmatch->second;

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

type::type(id mid) : mid(mid) { determineclass(); }

#define maketype(thetype, argtype, fullargtype) \
type type::make##thetype(fullargtype arg) { return type(getvaluefromkey<argtype>(thetype##kvmap, thetype##vkmap, thetype##nextfreeid, arg)); }

maketype(pointer, id, id);
maketype(array, id, id);
maketype(structure, signature, const signature&);
maketype(function, signature, const signature&);

#define gettype(thetype, keytype, rettype) \
\
rettype type::get##thetype##type() const { \
	\
	bool error = false; \
	const auto& ret = getkeyfromvalue<keytype>(thetype##vkmap, mid, error); \
	\
	if (error) throw exception{ "get" #thetype "type() called on id " + std::to_string(mid) + ", which is not a " #thetype " type" }; \
	return ret; \
	\
}

gettype(pointer, id, id);
gettype(array, id, id);
gettype(structure, signature, const signature&);
gettype(function, signature, const signature&);

bool type::is(type t) const { 

	if (t.mid == anything) return true;
	if (mid == t.mid) return true;
	if (mclass != t.mclass) return false;

    if (mclass == primitive & t.mid == primitive) return true;

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

		const signature *lh, *rh;

		if (mclass == structure) { lh = &getstructuretype(); rh = &t.getstructuretype(); }
		else { lh = &getfunctiontype(); rh = &t.getfunctiontype(); }

		if (lh->identifier != rh->identifier) return false;

		for (int k = 0; k < lh->types.size(); k++)
			if (!type(lh->types[k]).is(rh->types[k])) return false;

		return true;

	}

	return false;

}

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

int type::bytes() const { 

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

		for (int k = 0; k < sig.types.size(); k++) b += type(sig.types[k]).bytes();

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