#!/usr/bin/env luajit

local C_H_Parser = require 'c-h-parser'
local headers = C_H_Parser()

assert(headers(
-- [[
	require'ext.path''test.h':read()
--]]
))

--[=[
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
--]=]

-- maybe an AST is good, for re-serialization ...
print'typedefs:'
for i,ctype in ipairs(headers.declTypes) do
	print('#'..i, ctype:toC()..';')
end
print()

-- this should be a collection of enumdef's 
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

stmt:: [
		stmtQuals
		| 'typedef'			# can't have typedef and stmt-quals (excpet const qual, which is really a subdecl qual, which it's in structDeclQuals)
	] stmtDecls ';';

stmtQuals::
	{'static' | 'inline' | 'extern' | 'const' | 'volatile'}

# putting const volatile etc left or right of the start type name will contribute to the stmt-decl-type.
# but as soon as you put a *, it enters into the per-subDecl's type
# ... maybe merge this with `stmt`
stmtDecls::
	startType
	stmtQuals
	
	-- 1-or-many, or 0-or-many?
	subDecl
	{',' subDecl}

# This rule is for statements or struct/union fields.
# Like int a, *b, c[2], d; makes int a, int*b, int[2]c, int d;
# names are optional too.
structDecls::
	startType	
	
	# if 'const' & 'volatile' also goes in subDecl then I can remove this one rule-expr right?
	# but likewise, if structDecls and stmtDecls overlaps then this can harmlessly go here.
	structDeclQuals		# struct fields can have proceeding 'const' / 'volatile' quals
	
	subDecl {',' subDecl}
	;

startType::
	(
		('struct' | 'union') [name] [
			'{' 
				{ 
					structDeclQuals		# struct fields can have preceding 'const' / 'volatile', no other quals
					structDecls 
					';'
				}
			'}'
		]
	) |
	(
		'enum' [name] '{' [
			name ['=' number]
			{',' name ['=' number]}
			[',']
		] '}'
	)
	name
	;

structDeclQuals:: { 'const' | 'volatile' } ;

# array on the name means the defining subDecl is an array
# array after function args means we're returning an array - which is an error.
# array before function args but outside parenthesis of a function-def means an array-of-functions which is bad.
# only array inside all parenthesis, next to the name, and with (* ), will tell it array-of-function-pointers.
subDecl::
	'(' subDecl ')' |
	'*' subDecl |
	structDeclQuals subDecl |
	
	# arrays or function-args can go anywhere in the parenthesis list.
	# so long as they are rhs of the name.
	# but in all the parenthesis-list, only one function-arg list can exist.
	# and if any function-arg list exists then any arrays can only go in the inner-most parenthesis next to the name.
	subDecl (					# function-args or array but not both.
		[
			'(' [
				funcArg {',' funcArg}
			] ')'
		] |
		{'[' number ']'}		# where does the array rule go?  in all the decl, don't allow arrays if function-args were ever found, unless the array is next to the inner-most name ... smh C ...
	) |
	
	cpSubDecl
	;

# subDecl for stmtDecl or structDecl needs a name, subDecl for funcArg doesn't.
# NOTICE bitfields only for subDecls of structs...
cpSubDecl:: name [':' number];

# really this is just 
# 1) stmtDecls
# 2) ... but enforcing 'structDeclQuals' i.e. non-stmt-decl-quals, i.e. 'const' / 'volatile' ... tho without stmt decl quals you can even avoid this check and merge it into the subDecl rule...
# 3) ... and without multiple names per type
funcArg::
	structDeclQuals			# really this rule should be named nonStmt decl quals, for struct and func-args
	startType
	subDecl					# NOTICE for this one, via funcarg, name is optiona.

--]]
