#ifndef TYPE_HPP
#define TYPE_HPP

#include "./datatypes.hpp"

// implementation of a type system

namespace jitbox {

	using typelist = vec<id>;
		
	struct signature : public printable {					// each user-defined function or structure gets a unique signature

		id identifier;										// a typelist alone is not sufficient to distinguish signatures -- after all, two functions could have the same argument types, but they should still not be considered the same function
		typelist types;	
		id returntype = 0;									// only relevant for function signatures

		bool operator < (const signature&) const;

		string tostring(int) const override;

	};
	
	struct type : public printable  {						// creates a 1-1 mapping between types and integer ids

		enum : id {

			nothing,                                        // used for void functions
			anything,                                       // used for creating abstract types
			primitive,                                      // abstract primitive type
			i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, // concrete primitive types
			pointer, array, structure, function,            // abstract derived types

		};

		inline type() { }

															// basic constructor; will throw exception if the id supplied is invalid
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
		bool is(type) const; inline bool is(id i) const { return is(type(i)); }

		inline id getid() const { return mid; }

															// returns either nothing, anything, primitive, pointer, array, structure, or function
		inline id getclass() const { return mclass; }

		bool isconcrete() const;

		inline bool isabstract() const { return !isconcrete(); }

		inline bool isinteger() const { return mid >= i8 && mid <= u64; }

		inline bool isunsigned() const { return mid >= u8 && mid <= u64; }

		inline bool isfp() const { return mid == f32 || mid == f64; }

															// number of bytes that an object of this type takes up in memory, returns -1 if size is not known at compile time (e.g. arrays)
		int bytes() const;
		
		id mid = 0;
		id mclass = 0;

		void determineclass();

		string tostring(int) const override;

	};
	
}

#endif