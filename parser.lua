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
	for w in ([[* ( ) { } [ ] ; : ,]]):gmatch('%S+') do
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

-- follow typedef baseType lookups to the origin and return that
function C_H_Parser:getCType(typename)
	local ctype = self.ctypes[typename]
	if not ctype then return end

	-- TODO should typedefs be ctypes?
	if ctype.isTypedef then
		local sofar = {}
		-- if it has a baseType and no arrayCount then it's just a typedef ...
		repeat
			if sofar[ctype] then
				error"found a typedef loop"
			end
			sofar[ctype] = true
			ctype = assert.index(ctype, 'baseType')
		until not ctype.isTypedef
	end
	return ctype
end

function C_H_Parser:getPtrType(baseType)
--DEBUG:print('getPtrType', baseType)
	local typename = baseType.name..'*'
--DEBUG:print('getPtrType typename', typename)
	local ptrType = self:getCType(typename)
	if ptrType then
--DEBUG:print('...getPtrType found old', ptrType, ptrType == self.ctypes.void, ptrType==self.ctypes['void*'])
		return ptrType
	end
	ptrType = self:node('_ctype', {
		baseType = baseType,
		isPointer = true,
		parser = self,
	})
--DEBUG:print('...getPtrType made new', ptrType)
	return ptrType
end

function C_H_Parser:getConstType(baseType)
	if baseType.isConst then return baseType end
	local typename = baseType.name..' const'	-- matches _ctype:init name builder
	local constType = self:getCType(typename)
	if constType then return constType end
	-- within the ctor it'll assign to self:getCType i.e. self.ctypes[]
	return self:node('_ctype', {
		parser = self,
		baseType = baseType,
		isConst = true,
	})
end

function C_H_Parser:getVolatileType(baseType)
	if baseType.isVolatile then return baseType end
	local typename = baseType.name..' volatile'	-- matches _ctype:init name builder
	local volatileType = self:getCType(typename)
	if volatileType then return volatileType end
	-- within the ctor it'll assign to self:getCType i.e. self.ctypes[]
	return self:node('_ctype', {
		parser = self,
		baseType = baseType,
		isVolatile = true,
	})
end


function C_H_Parser:getArrayType(baseType, ar)
	local ctype = self:getCType(baseType.name..'['..ar..']')
--DEBUG:print('looking for ctype name', baseType.name..'['..ar..'], got', ctype)
	-- if not then make the array-type
	if not ctype then
		ctype = self:node('_ctype', {
			parser = self,
			baseType = baseType,
			arrayCount = ar,
		})
	end
	return ctype
end

-- forward to __call
function C_H_Parser:init(args)

	-- put types here ... for name
	self.ctypes = {}

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
	for _,name in ipairs(string.split(string.trim[[
void
bool
char
signed char
unsigned char
short
short int
signed short
signed short int
unsigned short
unsigned short int
int
signed
signed int
unsigned
unsigned int
long
long int
signed long
signed long int
unsigned long
unsigned long int
long long
long long int
signed long long
signed long long int
unsigned long long
unsigned long long int
float
double
long double
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
		self:node('_ctype', {name=name, isPrimitive=true, parser=self})
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
	repeat
		self:parseStmt()
		self:mustbe(';', 'symbol')
	until not self.t.token
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
--DEBUG:print('C_H_Parser:parseStmt nexttoken='..self.t.token)

	-- lhs of the type name ...
	local stmtQuals = self:parseStmtQuals()
	local isTypedef = self:canbe('typedef', 'keyword')
	
	assert(not isTypedef or not stmtQuals.static, {msg="cannot combine static and typedef"})
	assert(not isTypedef or not stmtQuals.extern, {msg="cannot combine extern and typedef"})
	assert(not isTypedef or not stmtQuals.inline, {msg="cannot combine inline and typedef"})
	
	--stmtQuals.static	-- function or variable
	--stmtQuals.extern	-- function only 
	--stmtQuals.inline	-- function only

	-- forward on 'const' and 'volatile' to the type-qualifiers

--DEBUG:print('pre-decl qualifiers:', require'ext.tolua'(stmtQuals))

	local decls = self:parseDecls(stmtQuals, false, false)	-- false == not a struct, false == not a function-arg

	if isTypedef then
		-- insert all as typedefs
		for _,decl in ipairs(decls) do
			-- add typedefs
print('decl type', decl.subdecl.type.name)			
print('decl name', decl.subdecl.name)			
			self.declTypes:insert(self:node('_ctype', {
				parser = self,
				isTypedef = true,
				baseType = decl.subdecl.type,
				name = assert.index(decl.subdecl, 'name'),
			}))
		end
	else
		-- insert all as decls
		-- here ... add to subdecl in-order list
		-- TODO track names: insert 'func' by name? what about anonymous funcs?
		for _,decl in ipairs(decls) do
			if self.ast._fwdDeclStruct:isa(decl) then
				self.declTypes:insert(decl)
			else
				self.symbolsInOrder:insert(decl)
			end
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
function C_H_Parser:parseDecls(quals, isStructDecl, isFuncArg)
--DEBUG:print('C_H_Parser:parseDecls', quals, isStructDecl, isFuncArg)
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
	if startType.isStruct
	--and self:canbe(';', 'symbol') -- don't consume it, leave it for the end of stmt
	and self.t.token == ';'
	then
		local fwdDecl = self:node('_fwdDeclStruct', {
			parser = self,
			name = startType.name,
		})
		return {fwdDecl}
		--self.symbolsInOrder:insert(fwdDecl)
	end

	-- See if we're using a const type -- that reflects on every subsequent field
	--  while in C if we get a * on the lhs, that marks the start of the 1st subdecl's field.
	if quals.const then
		startType = self:getConstType(startType)
	end
	if quals.volatile then
		startType = self:getVolatileType(startType)
	end

	-- always decl here?
	-- yes?

	local decls = table()
	repeat
		local decl = self:parseSubDecl(startType, isStructDecl, isFuncArg)
		decls:insert(decl)

		-- if it's not a structDecl or a funcArg then this is a stmt decl 
		--  so save the stmt-qualifiers
		if not (isStructDecl or isFuncArg) then
			decl.stmtQuals = quals
		end
		
		-- if isFuncArg then don't handle multiple names after the type
		if isFuncArg then break end
	until not self:canbe(',', 'symbol')
	return decls
end

function C_H_Parser:parseCVQuals()
	return self:parseQualifiers{'const', 'volatile'}
end

function C_H_Parser:parseStartType()
	if self:canbe('struct', 'keyword')
	or self:canbe('union', 'keyword')
	then
		local isUnion = self.lasttoken == 'union'
		local structName = self:canbe(nil, 'name')
		-- now "struct "..structName is our type that we should test for collision.
		structName = (isUnion and 'union ' or 'struct ')..structName

		if self:canbe('{', 'symbol') then
			local fields = table()
			while not self:canbe('}', 'symbol') do
				local quals = self:parseCVQuals()
				-- 2nd 'true' means struct-decls, means only look for 'const' qualifier and not the rest (volatile extern etc)
				-- 3rd 'false' means not a function-arg.  function-args can only have one name after the startType, not multiple.
				local decls = self:parseDecls(quals, true, false)	
				fields:append(assert(decls))
				self:mustbe(';', 'symbol')
			end
			return self:node('_ctype', {
				parser = self,
				name = structName,
				fields = fields,
				isStruct = true,
				isUnion = isUnion,
			})
		end
		-- else ... better have a name
		-- and then it's a forward-declaration of a struct, for the sake of other decl prototypes ...
		-- ... TODO really just use self.tree anyways.
		local ctype = self:node('_ctype', {
			parser = self,
			name = structName,
			isStruct = true,
			isUnion = isUnion,
			-- fwd-declare ...
			-- but still legit here cuz it could be used for ptr-types in the same stmt.
		})
		return ctype
	elseif self:canbe('enum', 'keyword') then
		local enumName = self:canbe(nil, 'name')
	
		local ctype, fieldDest
		if enumName then
			-- TODO maybe not define the type here, 
			-- but instead return the enum name and enum values to whoever called this
			-- and then based on 'typedef' or not, define the type versus look the type up.
			ctype = self:node('_ctype', {
				parser = self,
				name = enumName,
				baseType = assert(self.ctypes.uint32_t),
				isEnum = true,
			})
			ctype.enumValues = table()	-- TODO in ctor
			fieldDest = ctype.enumValues
		else
			fieldDest = self.anonEnumValues
		end

		if self:canbe('{', 'symbol') then
			-- define a new enum type
			local enumValue = 0
			local first = true
			repeat
				if not first then
					self:mustbe(',', 'symbol')
				end
				first = false
				
				local enumField = self:canbe(nil, 'name')
				if self:canbe('=', 'symbol') then
					-- TODO handle enum expressions
					enumValue = self:mustbe(nil, 'number')
				end
			
				fieldDest:insert(self:node('_enumdef', {
					parser = self,
					name = enumName,
					value = enumValue,
				}))
				enumValue = enumValue + 1
			until false
			self:canbe(',', 'symbol')
			self:mustbe('}', 'symbol')
		else
			-- expect the type to exist
		end
	else
		local typename = self:mustbe(nil, 'name')
		-- this typename is going to be the type of whatever comes next -- symbol, or typedef.
		-- so it must exist
		return assert(self:getCType(typename), {msg="unknown type "..tostring(typename)})
	end
end

--[[
If this is for a stmt-decl or a struct-decl then it gives a warning without a name
If this is for a function-arg then it doesn't.
--]]
function C_H_Parser:parseSubDecl(startType, isStructDecl, isFuncArg)
	if self:canbe('(', 'symbol') then
		-- What do ( ) around a decl name do? scare off macros?
		-- TODO maybe I should wrap this in a _par AST node
		--  since for arrays we're going to complain if it's on any AST node other than the inner-most non-parenthsis AST node.
		local subdecl = self:parseSubDecl(startType, isStructDecl, isFuncArg)
		self:mustbe(')', 'symbol')
		return subdecl
	end

	-- once we get our * then it and all subsequent *'s and const's only applies to this subdecl
	if self:canbe('*', 'symbol') then
		startType = self:getPtrType(startType)
		local quals = self:parseCVQuals()
		-- const or volatile has to always follow a *, or be on the startType
		if quals.const then
			startType = self:getConstType(startType)
		end
		if quals.volatile then
			startType = self:getVolatileType(startType)
		end

		return self:parseSubDecl(startType, isStructDecl, isFuncArg)
	end

	-- done with const's *'s and ()'s, move on to the name, array, function-args
	local subdecl = self:parseCPSubDecl(startType, isFuncArg, isStructDecl)

	-- Function args can go here for now, until I figure out where they belong.
	-- ... but how to tell function-args from parenthesis?
	local func
	if self:canbe('(', 'symbol') then
		
		-- if we're not parsing a func-arg then we'll want our subdecl to have a name
		-- becuase functions all need names.
		if not isFucnArg then
			assert(subdecl.name, {msg="function needs a name"})
		end
		
		local funcArgs = table()
		local first = true
		while not self:canbe(')', 'symbol') do
			if not first then
				self:mustbe(',', 'symbol')
			end
			first = false

			funcArgs:insert(self:parseFuncArg())
		end

		assert(not baseType.funcArgs, {msg="can't define a function of a function, right?  It needs to be a function pointer, right?"})
		-- TODO should I even od this?  how about a unique new funcType node? same with arrayType, ptrType, constType, structType, enumType, etc ...
		-- but what about unique type name registration and caching ...
		subdecl.type = self:node('_ctype', {
			parser = self,
			baseType = subdecl.type,	-- return type
			funcArgs = funcArgs,		-- arg list
		})

		-- so func and var are the same?
		-- except that func can't return an array or something?
		func  = self:node('_func', {
			parser = self,
			subdecl = subdecl,
		})
	end

	-- TODO where to process arrays ... 
	-- ... they can nest in parenthesis
	-- ... but if we have any function-args in the overall decl
	-- ... ... then we get an error if the array is anywhere except in the innermost parenthesis after the name. 
	if self:canbe('[', 'symbol') then
		if func then
			-- but honestly, if this is supposed to be the array-part of the function, then my parser has a problem.
			error{msg="functions can't return arrays"}
		end

		-- TODO also parse compile-time expressions
		local arrayCount = self:mustbe(nil, 'number')
		-- modifying this in-place won't come back to haunt me, right?
		-- there's only one subdecl per func or var, so ... ?
		subdecl.type = self:getArrayType(subdecl.type, arrayCount)
		self:mustbe(']', 'symbol')
	end

	local var
	if not func then 
		var = self:node('_var', {
			parser = self,
			subdecl = subdecl,
		})
	end

	return func or var
end

function C_H_Parser:parseFuncArg()
	-- funcArg:
	local quals = self:parseCVQuals()	-- see if there's a leading 'const'
	-- arg 2 true = struct-type = only look for 'const' right of the type, not 'volatile', 'static', 'inline'.  
	-- arg 3 false = function-arg = only allow one name (and with an optional name at that), not multiple `int a,b,c`'s
	return self:parseDecls(quals, true, true)	
end

function C_H_Parser:parseCPSubDecl(ctype, isFuncArg, isStructDecl)
--DEBUG:print('C_H_Parser:parseCPSubDecl', ctype, isFuncArg, isStructDecl)
--DEBUG:assert(ctype)	
	-- I could make a separte rule or three for this but nah ... just if's for ( and )
	local name
	if isFuncArg then
		name = self:canbe(nil, 'name')
	else
		name = self:mustbe(nil, 'name')
	end

	if isStructDecl 
	and self:canbe(':', 'symbol')
	then
		local bitfield = self:mustbe(nil, 'number')
		ctype = self:getBitFieldType(ctype, bitfield)
	end
	return self:node('_subdecl', {
		parser = self,
		isFuncArg = isFuncArg,
		isStructDecl = isStructDecl,
		name = name,
		type = ctype,
	})
end

return C_H_Parser
