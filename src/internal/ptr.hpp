#ifndef PTRHPP
#define PTRHPP

// simple smart pointer implementation I wrote a long time ago before I knew that
// std smart pointers were a thing; I still use it just out of nostalgia

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
inline bool ptr<ptrtype>::operator < (const ptr& rh) const { return rh > * this; }

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

#endif