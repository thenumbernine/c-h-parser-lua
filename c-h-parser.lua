--[[
C header parser
Maybe I'll using my parsing project's tokenizer and datareader for this...
Maybe some day I'll grow the parsing project to be able to create its own parser from a provided grammar...

Takes in C header code with ctor / subsequent call() info.
Collects definitions into the parser:
	.structs for structs/unions
	.typedefs
	.funcs
	.vars
--]]
local class = require 'ext.class'
local string = require 'ext.string'
local table = require 'ext.table'
local assert = require 'ext.assert'

local Tokenizer = require 'parser.base.tokenizer'
local C_H_Tokenizer = Tokenizer:subclass()

function C_H_Tokenizer:initSymbolsAndKeywords()
	for w in ([[* ( ) { } [ ] ; : , =]]):gmatch('%S+') do
		self.symbols:insert(w)
	end

	-- self.keywords lets the tokenizer flag if this is a reserved word or not, that's all
	for w in ([[
const enum extern
struct union
typedef
static
extern
inline
__inline
__inline__
volatile
	]]):gmatch'%w+' do
		self.keywords[w] = true
	end
end

C_H_Tokenizer.singleLineComment = string.patescape'//'

function C_H_Tokenizer:parseBlockComment()
	local r = self.r
	if not r:canbe'/%*' then return end
	local start = r.index
	if not r:seekpast'%*/' then
		error{msg="expected closing block comment"}
	end
	r.lasttoken = r.data:sub(start, r.index - #r.lasttoken - 1)
	return true --r.lasttoken
end

-- TODO extend Tokenizer:parseNumber to handle multiple number types, not just hex/dec


local Parser = require 'parser.base.parser'
local C_H_Parser = Parser:subclass()

C_H_Parser.ast = require 'c-h-parser.ast'

-- forward to __call
function C_H_Parser:init(args)
	-- used for referencing later
	self.builtinTypes = {}

	-- put declared types in-order here
	self.declTypes = table()

	-- put symbol defs here
	self.symbols = {}
	self.symbolsInOrder = table()

	-- TOOD var defs in order

	-- TODO function defs in order

	self.anonEnumValues = table()

	-- init ctypes ...
	-- https://en.wikipedia.org/wiki/C_data_types
	-- TODO I don't need these anymore, except enum points to them but doesn't use it
	for _,name in ipairs(string.split(string.trim[[
void
bool
char

signed
signed char
signed int
signed short
signed short int
signed long
signed long int
signed long long
signed long long int

unsigned
unsigned char
unsigned int
unsigned short
unsigned short int
unsigned long
unsigned long int
unsigned long long
unsigned long long int

short
short int
long
long int
long double
long long
long long int

int
float
double
int8_t
uint8_t
int16_t
uint16_t
int32_t
uint32_t
int64_t
uint64_t
int_least8_t
uint_least8_t
int_least16_t
uint_least16_t
int_least32_t
uint_least32_t
int_least64_t
uint_least64_t
int_fast8_t
uint_fast8_t
int_fast16_t
uint_fast16_t
int_fast32_t
uint_fast32_t
int_fast64_t
uint_fast64_t
intptr_t
uintptr_t
intmax_t
uintmax_t
size_t
ssize_t
ptrdiff_t
]], '\n')) do
		self.builtinTypes[name] = self:node('_ctype', name)
	end

	if args then
		assert(self(args))
	end
end

--[[
obj(data)
obj(args)
args = table of:
	data

I'm using the same API style in preproc and here: successive calls to succesively parse content.

Should I do it in my parser/ project as well?
--]]
function C_H_Parser:__call(args)
	if type(args) == 'string' then
		args = {data=args}
	elseif type(args) ~= 'table' then
		error("can't handle args")
	end

	local data = assert.index(args, 'data')

	local result = table.pack(C_H_Parser.super.setData(self, data))
	if not result[1] then
		print(result:unpack())
		return result:unpack()
	end

	return true
end

function C_H_Parser:buildTokenizer(data)
	return C_H_Tokenizer(data)
end

-- parser/base/parser calls this after setData
function C_H_Parser:parseTree()
	while self.t.token ~= nil do
		self:parseStmt()
		self:mustbe(';', 'symbol')
	end
end

--[[
Each arg is a string of a qualifier to look for.
Duplicates should produce warnings.
The arg can also be a table if multiple keywords map to the same qualifier, in which case the first is used.
All args are expected to be keywords.

Returns a table with keys of each qualifier found.
--]]
function C_H_Parser:parseQualifiers(keywords, qualifiers)
	qualifiers = qualifiers or {}
	local found
	repeat
		found = nil
		for _,keyword in ipairs(keywords) do
			if type(keyword) == 'string' then
				if self:canbe(keyword, 'keyword') then
					-- warning Wduplicate-decl-specifier
					--assert(not qualifiers[keyword], {msg="warning duplciate '"..keyword.."' qualifier"})
					qualifiers[keyword] = true
					found = true
				end
			elseif type(keyword) == 'table' then
				for _,keywordOption in ipairs(keyword) do
					if self:canbe(keywordOption, 'keyword') then
						-- warning Wduplicate-decl-specifier
						--assert(not qualifiers[keyword[1]], {msg="warning duplciate '"..keyword[1].."' qualifier"})
						qualifiers[keyword[1]] = true
						found = true
						break
					end
				end
			else
				error("unknown arg type "..type(keyword))
			end
		end
	until not found
	return qualifiers
end

function C_H_Parser:parseStmtQuals(qualifiers)
	return self:parseQualifiers({
		'static',
		'extern',
		{'inline', '__inline', '__inline__'},
		'const',
		'volatile',
	}, qualifiers)
end

function C_H_Parser:parseStmt()
--DEBUG:print('C_H_Parser:parseStmt nexttoken='..tostring(self.t.token))

	-- lhs of the type name ...
	local isTypedef = self:canbe('typedef', 'keyword')
	local stmtQuals = self:parseStmtQuals()

	assert(not isTypedef or not stmtQuals.static, {msg="cannot combine static and typedef"})
	assert(not isTypedef or not stmtQuals.extern, {msg="cannot combine extern and typedef"})
	assert(not isTypedef or not stmtQuals.inline, {msg="cannot combine inline and typedef"})

	--stmtQuals.static	-- function or variable
	--stmtQuals.extern	-- function only
	--stmtQuals.inline	-- function only

	-- forward on 'const' and 'volatile' to the type-qualifiers

--DEBUG:print('pre-decl qualifiers:', require'ext.tolua'(stmtQuals))

	local decl = self:parseDecl(stmtQuals, false, false)	-- false == not a struct, false == not a function-arg
--DEBUG:assert(decl)

	if self.ast._fwdDecl:isa(decl) then

		if self.ast._enumType:isa(decl[1])
		and not decl[1].name
		then
			-- nameless enum without decls?
			-- put all its fields in the global list
			self.anonEnumValues:append(assert(decl[1].enumFields))
		else
			self.declTypes:insert(decl)
		end	
	elseif isTypedef then
		-- typedef struct name; is invalid, so this should have subdecls after it ...
		self.declTypes:insert(self:node('_typedef', decl))
	else
		-- if decl's type is a struct with a body then it goes in declTypes
		local lhsType = decl.baseType
		while self.ast._const:isa(lhsType) 
		or self.ast._volatile:isa(lhsType) 
		do
			lhsType = lhsType[1]
		end
	
		if self.ast._structType:isa(lhsType)
		or self.ast._enumType:isa(lhsType)
		then
			self.declTypes:insert(decl)
		else
			self.symbolsInOrder:insert(decl)
		end
	end
end

--[[
This is a combination of `stmtDecls` and `structDecls`
parse lhs start-type of our decls
then parse added quals
- if it's a stmt then parse stmt-quals (and append to the qual list given, if there)
- if it's a struct then parse struct-decls , which is just 'const'
--]]
function C_H_Parser:parseDecl(quals, isStructDecl, isFuncArg)
--DEBUG:print('C_H_Parser:parseDecl', quals, isStructDecl, isFuncArg)
	-- parse the start of the type
	-- notice that stmt quals can still come after the name, so long as they are before the comma
	-- in fact, of all stmt-qualifiers, it seems the subsequent types of the decls only cares about 'const'
	local startType = self:parseStartType()

	if isStructDecl then
		self:parseCVQuals(quals)	-- look for const rhs of the startType
	else
		-- More stmt qualifeirs can come after the first type.
		-- Yes you can define a struct then do 'volatile', so long as you haven't done any *'s or names
		-- You can also do `int const volatile static a, b, c` and the const goes to all subsequent a,b,c;
		--  evne though the const is for the fields while the volatile static is for the statement.
		self:parseStmtQuals(quals)
	end


	-- hack here, if we're doing a stmt decl and our base type is a struct
	-- and it's a struct without a body
	-- names are optional - in that event it's a fwd-declare.
	if (
		self.ast._structType:isa(startType)
		or self.ast._enumType:isa(startType)
	)
	--and self:canbe(';', 'symbol') -- don't consume it, leave it for the end of stmt
	and self.t.token == ';'
	then
		-- NOTICE if we do `const struct {};` that's an error
		-- so here we should assert there's no CV qualifiers if it's a plain `struct {...};` declaration
		assert.eq(next(quals), nil, {msg="if you just have a struct definition without variables, it can't have qualifiers"})

		-- TODO not necessary? Just check for a struct-decl that has no subdecls (and no name?)
		return self:node('_fwdDecl', startType)
	end

	-- See if we're using a const type -- that reflects on every subsequent field
	--  while in C if we get a * on the lhs, that marks the start of the 1st subdecl's field.
	if quals.const then
		startType = self:node('_const', startType)
	end
	if quals.volatile then
		startType = self:node('_volatile', startType)
	end

	-- always decl here?
	-- yes?

	-- ok rather than passing in startType
	-- lets make an AST
	-- so lets collect back decls
	-- and save them inside startType
	-- so it can generate them with its serialization

	local subdecls = table()
	repeat
		local subdecl = self:parseSubDecl(isStructDecl, isFuncArg)
--DEBUG:assert(subdecl)
		subdecls:insert(subdecl)

		-- if isFuncArg then don't handle multiple names after the type
		if isFuncArg then break end
	until not self:canbe(',', 'symbol')

	-- in fact, maybe at this point I should be returning a unique node of the two together
	-- just like I do with fwdDecl
	local decl = self:node('_decl',
		-- the starting type on the left of the statement
		startType,

		-- the comma-separated sub-declarations to the right of the starting type
		subdecls,

		-- if it's not a structDecl or a funcArg then this is a stmt subdecl
		--  so save the stmt-qualifiers
		quals)
	return decl
end

function C_H_Parser:parseCVQuals()
	return self:parseQualifiers{'const', 'volatile'}
end

-- This is the start of a declaration, before the *cv-specific comma-separated stuff to the right
function C_H_Parser:parseStartType()
	if self:canbe('struct', 'keyword')
	or self:canbe('union', 'keyword')
	then
		local isUnion = self.lasttoken == 'union'
		local structName = self:canbe(nil, 'name')
		-- now "struct "..structName is our type that we should test for collision.
		-- TODO here's where ctypes won't recognize this
		-- or for no name, don't lookup in ctypes cuz it's anonymous

		if self:canbe('{', 'symbol') then
			local fields = table()
			while not self:canbe('}', 'symbol') do
				local quals = self:parseCVQuals()
				-- 2nd 'true' means struct-decls, means only look for 'const' qualifier and not the rest (volatile extern etc)
				-- 3rd 'false' means not a function-arg.  function-args can only have one name after the startType, not multiple.
				local fielddecl = self:parseDecl(quals, true, false)
				fields:insert(fielddecl)
				self:mustbe(';', 'symbol')
			end
			return self:node('_structType', {
				name = structName,	-- nil for anonymous
				isUnion = isUnion,
				fields = fields,
			})
		end
		-- else ... better have a name
		-- and then it's a forward-declaration of a struct, for the sake of other decl prototypes ...
		-- ... TODO really just use self.tree anyways.
		local ctype = self:node('_structType', {
			name = structName,	-- nil for anonymous
			isUnion = isUnion,
			-- fwd-declare ...
			-- but still legit here cuz it could be used for ptr-types in the same stmt.
		})
		return ctype
	elseif self:canbe('enum', 'keyword') then
		local enumName = self:canbe(nil, 'name')

		local ctype = self:node('_enumType', {
			name = enumName,
			-- TODO does C have different enum types?
			baseType = assert(self.builtinTypes.uint32_t),
		})
		
		if self:canbe('{', 'symbol') then
			-- define a new enum type
			local first = true
			repeat
				local enumField = self:mustbe(nil, 'name')
			
				local enumValue
				if self:canbe('=', 'symbol') then
					-- TODO handle enum expressions
					enumValue = self:mustbe(nil, 'number')
				end

				ctype.enumFields:insert(self:node('_enumdef', 
					enumField,
					enumValue	-- optional
				))

				if self:canbe(',', 'symbol') then 
					if self:canbe('}', 'symbol') then break end
				else
					self:mustbe('}', 'symbol') 
					break 
				end
			until false
		end
	
		return ctype
	else
		local name = ''
		if self:canbe('signed', 'name')
		or self:canbe('unsigned', 'name')
		then
			name = self.lasttoken
			if self:canbe('int', 'name') 
			or self:canbe('char', 'name')
			then
				name = name..' '..self.lasttoken
				-- and done
			elseif self:canbe('short', 'name') then
				name = name..' '..self.lasttoken
				if self:canbe('int', 'name') then
					name = name..' '..self.lasttoken
				end
			elseif self:canbe('long', 'name') then
				name = name..' '..self.lasttoken
				if self:canbe('int', 'name') then
					name = name..' '..self.lasttoken
				elseif self:canbe('long', 'name') then
					name = name..' '..self.lasttoken
					if self:canbe('int', 'name') then
						name = name..' '..self.lasttoken
					end
				end
			end
		elseif self:canbe'short' then
			name = name..' '..self.lasttoken
			if self:canbe('int', 'name') then
				name = name..' '..self.lasttoken
			end	
		elseif self:canbe('long', 'name') then
			name = name..' '..self.lasttoken
			if self:canbe('int', 'name') 
			or self:canbe('double', 'name')
			then
				name = name..' '..self.lasttoken
			elseif self:canbe('long', 'name') then
				name = name..' '..self.lasttoken
				if self:canbe('int', 'name') then
					name = name..' '..self.lasttoken
				end
			end	
		else
			name = self:mustbe(nil, 'name')
		end
		return self:node('_ctype', name)
	end
end

--[[
If this is for a stmt-decl or a struct-decl then it gives a warning without a name
If this is for a function-arg then it doesn't.
--]]
function C_H_Parser:parseSubDecl(isStructDecl, isFuncArg)
--DEBUG:print('C_H_Parser:parseSubDecl', isStructDecl, isFuncArg)
	-- once we get our * then it and all subsequent *'s and const's only applies to this subdecl
	if self:canbe('*', 'symbol') then
		-- parse 'const' or 'volatile', which have to always follow a *, or be on the startType
		local quals = self:parseCVQuals()
		
		-- parse the rest
		-- start with this layer so we can parse multiple *'s 
		local subdecl = self:parseSubDecl(isStructDecl, isFuncArg)
		
		-- wrap the rest in our ptr() and either const() or volateil()
		subdecl = self:node('_ptr', subdecl)
		if quals.const then
			subdecl = self:node('_const', subdecl)
		end
		if quals.volatile then
			subdecl = self:node('_volatile', subdecl)
		end
		return subdecl
	else
		return self:parseSubDecl2(isStructDecl, isFuncArg)
	end
end

function C_H_Parser:parseSubDecl2(isStructDecl, isFuncArg)
--DEBUG:print('C_H_Parser:parseSubDecl2', isStructDecl, isFuncArg)
	local subdecl = self:parseSubDecl3(isStructDecl, isFuncArg)

	-- while-loop for multiple x[1][1][1]... ... can anything go between array decls?
	while self:canbe('[', 'symbol') do
		-- but honestly, if this is supposed to be the array-part of the function, then my parser has a problem.
		--if func then error{msg="functions can't return arrays"} end

		-- TODO also parse compile-time expressions
		local arrayCount = self:mustbe(nil, 'number')
		arrayCount = tonumber(arrayCount) or error{msg="bad array size "..arrayCount}
		self:mustbe(']', 'symbol')
	
		subdecl = self:node('_array', subdecl, arrayCount)
	end

	return subdecl
end

function C_H_Parser:parseSubDecl3(isStructDecl, isFuncArg)
--DEBUG:print('C_H_Parser:parseSubDecl3', isStructDecl, isFuncArg)
	local subdecl = self:parseSubDecl4(isStructDecl, isFuncArg)

	if isStructDecl
	and self:canbe(':', 'symbol')
	then
		local bits = self:mustbe(nil, 'number')
		subdecl = self:node('_bitfield', subdecl, bits)
	end

	return subdecl
end

function C_H_Parser:parseSubDecl4(isStructDecl, isFuncArg)
--DEBUG:print('C_H_Parser:parseSubDecl4', isStructDecl, isFuncArg)

	--[[
	if name is optional (i.e. for func args)
	then how to distinguish th 1st arsg from the 2nd args in `int(x)(int)`
	... when we parse the 1st parenthesis, we dont know if it's going to be a name with qualiifers or a list of funcargs/subdecls

	when is name required, and when is it optional?
	- required for root-level declarations.
	- optional for root-level struct declarations ... in which case we expect no-subdecls, i.e. expect ';'.
	- optional for function-args
	--]]

	local subdecl

	-- try for name
	-- if not isFuncArg then must be name, but here it might be another subexpression so ...
	-- TODO my recursion order is messed up I'm sure
	local name = self:canbe(nil, 'name')
	if name
	--or isFuncArg	-- TODO this is going to be a problem
	then
		-- technically these aren't ctypes, they are symbol/variable/function/field names
		subdecl = self:node('_ctype', name)
	else
		-- try for parenthesis-wrapping
		if self:canbe('(', 'symbol') then
			-- What do ( ) around a decl name do? scare off macros?
			-- TODO maybe I should wrap this in a _partype AST node
			--  since for arrays we're going to complain if it's on any AST node other than the inner-most non-parenthsis AST node.
			subdecl = self:parseSubDecl(isStructDecl, isFuncArg)
--DEBUG:assert(subdecl)
			self:mustbe(')', 'symbol')
			-- partype should go around the name and any internal function-args ...
			subdecl = self:node('_partype', subdecl)
		else
			-- and after name or parenthesis
			--  try for function-arguments
			--error{msg='too much'}
		
			if not isFuncArg 
			and not isStructDecl
			then
				error{msg="only function arguments or struct fields can be anonymous"}
			end
		
			-- technically these aren't ctypes, they are symbol/variable/function/field names
			subdecl = self:node('_ctype', '')
		end
	end

	-- [[ TODO this is also above
	-- there's gotta be a isngle place I can put it
	while self:canbe('[', 'symbol') do
		local arrayCount = self:mustbe(nil, 'number')
		arrayCount = tonumber(arrayCount) or error{msg="bad array size "..arrayCount}
		self:mustbe(']', 'symbol')
		subdecl = self:node('_array', subdecl, arrayCount)
	end
	--]]

	-- Function args can go here for now, until I figure out where they belong.
	-- ... but how to tell function-args from parenthesis?
	if self:canbe('(', 'symbol') then
		--[[ TODO idk how even to tell the name now, it's buried in the AST somewhere
		-- maybe I don't care
		-- if we're not parsing a func-arg then we'll want our subdecl to have a name
		-- becuase functions all need names.
		if not isFuncArg then
			assert(subdecl.subdecl.name, {msg="function needs a name"})
		end
		--]]

		local funcArgs = table()
		local first = true
		while not self:canbe(')', 'symbol') do
			if not first then
				self:mustbe(',', 'symbol')
			end
			first = false

			-- funcArg:
			local quals = self:parseCVQuals()	-- see if there's a leading 'const'
			-- arg 2 true = struct-type = only look for 'const' right of the type, not 'volatile', 'static', 'inline'.
			-- arg 3 false = function-arg = only allow one name (and with an optional name at that), not multiple `int a,b,c`'s
			local argdecl = self:parseDecl(quals, true, true)
			funcArgs:insert(argdecl)
		end

		-- TODO another validity check I stopped caring about
		--assert(not var.type.funcArgs, {msg="can't define a function of a function, right?  It needs to be a function pointer, right?"})
	
		-- TODO should I even od this?  how about a unique new funcType node? same with arrayType, ptrType, constType, structType, enumType, etc ...
		-- but what about unique type name registration and caching ...
		subdecl = self:node('_funcType', subdecl, funcArgs)
	end

	return subdecl
end

return C_H_Parser
