#!/usr/bin/env luajit

local C_H_Parser = require 'c-h-parser.parser'
local headers = C_H_Parser()

assert(headers[[
/*
typedef void T_v;
typedef int T_i;
typedef char * T_pc;
*/

// TODO typedef and symbol / function decl should overlap
// typedef int y[20];

int i;
/*
char * pc; // pointers don't work?
float f_20[20];
float * pf_20[20];
char c_20_30[20][30];

enum {
	A,B,C
};
*/
]])

-- maybe an AST is good, for re-serialization ...
print'typedefs:'
for i,ctype in ipairs(headers.declTypes) do
	print('#'..i, ctype:toC()..';')
end
print()

print'non-typedef enums:'
for i,enumDef in ipairs(headers.anonEnumValues) do
	print('#'..i, enumDef:toC()..';')
end
print()

print'symbols:'
for i,symbol in ipairs(headers.symbolsInOrder) do
	print('#'..i, symbol:toC()..';')
end

--[[
bnf?

stmt:: [{['static' | 'inline' | 'extern']} | 'typedef'] decls ';';

# like int a, *b, c[2], d; makes int a, int*b, int[2]c, int d;
names are optional too.
decls::
	startType 
	[
		field 
		{',' field}
	]

startType::
	['const'] type ['const'] {'*' ['const']} 

type::
	(
		('struct' | 'union') [name] ['{' {decls ';'} '}']
	) |
	(
		'enum' [name] '{'
			name ['=' number]
			{',' name ['=' number]}
			[',']
		'}'
	)
	name
	;

# array on the name means the defining field is an array
# array on the end of this means the type is an array type.
field::
	{'*' ['const']}
	(
		name [array]
		[funcname] | 
	)
	['(' arglist ')']
	[array]
	[':' number]
	;

funcname::
	'(' '*' name [array] ')'

array::
	{'[' number ']'}
	
--]]
