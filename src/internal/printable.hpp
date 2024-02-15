#ifndef PRINTABLE_HPP
#define PRINTABLE_HPP

#define INDENT_CHAR " "

#include <string>

namespace jitbox {

	using string = std::string;

	struct printable {

		string print(int alignment = 0, bool addnewline = true) const;

		virtual ~printable();

		virtual string tostring(int alignment) const = 0;

		static string indent(int);

		static string pad(string, int);

	};

	struct printablestring : public printable {

		string str;

		printablestring(string);
		printablestring(const char*);

		string tostring(int) const override;

	};
	
}

#endif