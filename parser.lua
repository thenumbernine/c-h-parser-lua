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
const enum extern struct typedef union
static
extern
inline 
__inline 
__inline__
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
function C_H_Parser:getctype(typename)
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

function C_H_Parser:getptrtype(baseType)
--DEBUG:print('getptrtype', baseType)
	local ptrtypename = baseType.name..'*'
--DEBUG:print('getptrtype ptrtypename', ptrtypename)
	local ptrType = self:getctype(ptrtypename)
	if ptrType then
--DEBUG:print('...getptrtype found old', ptrType, ptrType == ctypes.void, ptrType==ctypes['void*'])
		return ptrType
	end
	ptrType = self:node('_ctype', {
		baseType = baseType,
		isPointer = true,
		parser = self,
	})
--DEBUG:print('...getptrtype made new', ptrType)
	return ptrType
end

function C_H_Parser:getArrayType(baseType, ar)
	local ctype = self:getctype(baseType.name..'['..ar..']')
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
			local keyword = select(i, ...)
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
	
	-- lhs of the type name ...
	local stmtQuals = self:parseStmtQuals()
	local isTypedef = self:canbe('typedef', 'keyword')
	
	assert(not isTypedef or not stmtQuals.static, {msg="cannot combine static and typedef"})
	assert(not isTypedef or not stmtQuals.extern, {msg="cannot combine extern and typedef"})
	assert(not isTypedef or not stmtQuals.inline, {msg="cannot combine inline and typedef"})
	
	--stmtQuals.static	-- function or variable
	--stmtQuals.extern	-- function only 
	--stmtQuals.inline	-- function only
	--stmtQuals.volatile works on all, but only works once on the 'startType' of declarations.

	-- forward on 'const' and 'volatile' to the type-qualifiers

	self:parseDecls(stmtQuals)
end

--[[
This is a combination of `stmtDecls` and `structDecls`
parse lhs start-type of our decls 
then parse added quals
	- if it's a stmt then parse stmt-quals (and append to the qual list given, if there)
	- if it's a struct then parse struct-decls , which is just 'const'
--]]
function C_H_Parser:parseDecls(quals, isStructDecl)
	
	-- parse the start of the type
	-- notice that stmt quals can still come after the name, so long as they are before the comma
	-- in fact, of all stmt-qualifiers, it seems the subsequent types of the decls only cares about 'const'
	local startType = self:parseStartType(quals)

	if isStructDecl then
		assert(not quals)
		quals = self:parseStructDeclQuals()
	else
		-- More stmt qualifeirs can come after the first type.
		-- Yes you can define a struct then do 'volatile', so long as you haven't done any *'s or names
		-- You can also do `int const volatile static a, b, c` and the const goes to all subsequent a,b,c;
		--  evne though the const is for the fields while the volatile static is for the statement.	
		self:parseStmtQuals(quals)
	end

	-- see if we're using a const type
	if quals.const then
		startType = self:getconsttype(startType)
	end

	if self:parseSubDecl(startType) then
		self:mustbe(',', 'symbol')
		repeat
			self:parseSubDecl(startType)
		until not self:canbe(',', 'symbol')
	end
end

function C_H_Parser:parseStructDeclQuals()
	return self:parseQualifiers{'const'}
end

function C_H_Parser:parseStartType(isConst)
	if self:canbe('struct', 'keyword')
	or self:canbe('union', 'keyword')
	then
		local isUnion = self.lasttoken == 'union'
		local structName = self:canbe(nil, 'name')
		-- now "struct "..structName is our type that we should test for collision.
		if self:canbe('{', 'symbol') then
			self:parseDecls(nil, true)	-- 2nd 'true' means struct-decls, means only look for 'const' qualifier and not the rest (volatile extern etc)
			self:mustbe(';', 'symbol')
		end
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
		return assert(self:getctype(typename), {msg="unknown type "..tostring(typename)})
	end
end

function C_H_Parser:parseSubDecl(startType)
	-- once we get our * then it and all subsequent *'s and const's only applies to this subdecl
	while self:canbe('*', 'symbol') do
		startType = self:getPtrType(startType)
		if self:canbe('const', 'keyword') then
			startType = self:getConstType(startType)
		end
	end

	local isFuncPtrName = self:canbe('(', 'symbol') 
	-- I could make a separte rule or three for this but nah ... just if's for ( and )
	local name = self:c

	if isFuncPtrName then
	end

	return startType
end

function idk()
	repeat
		-- unlike parser/base/parser, I'm not going to save the tree here
		-- typedef ...
		if self:canbe('typedef', 'keyword') then
			local srctype
			-- next is either a previously declared typename or 'struct'/'union' followed by a struct/union def
			if self:canbe(nil, 'name') then
				local prefix = self:parseSignedUnsignedShortLong()

				-- TODO this won't handled `signed`, `unsigned` etc without the `int` at the end ...
				local name = self.lasttoken
				if prefix then name = prefix..' '..name end

				srctype = assert(self:getctype(name), "couldn't find type "..name)

			-- typedef struct ...
			-- typedef union ...
			elseif self:canbe('struct', 'keyword')
			or self:canbe('union', 'keyword')
			then
				-- TODO this still could either be ...
				--  ... a named struct declaration
				--  ... an anonymous struct declaration
				--  ... a 'struct ____' typename
				-- and we can't know until after we read the '{'

				local srctype = self:parseStruct(self.lasttoken == 'union')
				assert(srctype)

			-- typedef enum ...
			elseif self:canbe('enum', 'keyword') then
				local srctype = self:parseEnum()
				assert(srctype)
			end

			while self:canbe('*', 'symbol') do
				srctype = self:getptrtype(srctype)
			end

			local name = self:mustbe(nil, 'name')

			-- make a typedef type
			local ctype = self:node('_ctype', {
				parser = self,
				isTypedef = true,
				name = name,
				baseType = srctype,
			})
			self.declTypes:insert(ctype)

			self:mustbe(';', 'symbol')

		-- struct ...
		-- union ...
		elseif self:canbe('struct', 'keyword')
		or self:canbe('union', 'keyword')
		then
			local ctype = self:parseStruct(self.lasttoken == 'union')
			self:mustbe(';', 'symbol')
		elseif self:canbe('enum', 'keyword') then
			local ctype = self:parseEnum()
			self:mustbe(';', 'symbol')

		-- extern ...
		else
			local extern = self:canbe('extern', 'keyword')

			local ctypename = self:mustbe(nil, 'name')
			local ctype = self:getctype(ctypename)
			assert(ctype, {msg="expected ctype"})

			while self:canbe('*', 'symbol') do
				ctype = self:getptrtype(ctype)
			end

			-- TODO properly parse variable definition, including function and function-pointer definitions
			local name = self:mustbe(nil, 'name')

			while self:canbe('[', 'symbol') do
				local count = self:mustbe(nil, 'number')
				count = tonumber(count) or error("expected number: "..tostring(count))
				assert.gt(count, 0, "can we allow non-positive-sized arrays?")
				self:mustbe(']', 'symbol')
				ctype = self:getArrayType(ctype, count)
			end

			-- TODO function here
			local symbol = self:node('_symbol', {
				name = name,
				type = ctype,
			})

			if self.symbols[name] then error("already defined "..name) end
			self.symbols[name] = symbol
			self.symbolsInOrder:insert(symbol)

			-- TODO handle [ ]
			-- can you declare an array of functions?

			self:mustbe(';', 'symbol')
		end
	until not self.t.token
end

function C_H_Parser:parseSignedUnsignedShortLong()
	local prefix
	if self:canbe('signed', 'name')
	or self:canbe('unsigned', 'name')
	then
		prefix = self.lasttoken
	end

	if self:canbe('short', 'name') then
		prefix = prefix..' '..self.lasttoken
	elseif self:canbe('long', 'name') then
		prefix = prefix..' '..self.lasttoken
		if self:canbe('long', 'name') then
			prefix = prefix..' '..self.lasttoken
		end
	end
	return prefix
end

-- similar to struct but without the loop over multiple named vars with the same base type
function C_H_Parser:parseType(allowVarArray)
--DEBUG:print'parseType'

	-- should these be keywords?
	local signedness = self:parseSignedUnsignedShortLong()

	--const-ness ... meh?
	local const = self:canbe('const', 'keyword')

	local structunion = self:canbe('struct', 'keyword') or self:canbe('union', 'keyword')

	local name = self:mustbe(nil, 'name')
	if structunion then
		name = structunion..' '..name
	end
	if signedness then
		name = signedness..' '..name
	end

--DEBUG:print('parseType name', name)
	-- fields ...
	-- TODO this should be 'parsetype' and work just like variable declarations
	-- and should be interoperable with typedefs
	-- except typedefs can't use comma-separated list (can they?)
	local baseFieldType = assert(self:getctype(name), "couldn't find type "..name)
--DEBUG:assert.is(baseFieldType, self.ast._ctype)
--DEBUG:print('parseType baseFieldType', baseFieldType)
--DEBUG:print('does baseFieldType* exist?', ctypes[baseFieldType.name..'*'])
	local fieldtype = baseFieldType
	while self:canbe('*', 'symbol') do
--DEBUG:print('...looking for ptr of type', fieldtype)
		fieldtype = self:getptrtype(fieldtype)
--DEBUG:print('...making ptr type', fieldtype)
		-- const-ness ... meh?
		local const = self:canbe('const', 'keyword')
	end

	while self:canbe('[', 'symbol') do
		local count = self:canbe(nil, 'number')
		if count then
			assert(count > 0, "can we allow non-positive-sized arrays?")
		end
		self:mustbe(']', 'symbol')
		assert(count, "aren't [] arrays just pointers?  or we have to error about size being missing?")
		fieldtype = self:getArrayType(fieldtype, count)
	end

--DEBUG:print('parseType got', fieldtype)
	return fieldtype
end

-- assumes 'struct' or 'union' has already been parsed
-- doesn't assert closing ';' (since it could be used in typedef)
function C_H_Parser:parseStruct(isunion)

	local name = self:canbe(nil, 'name')
	if name then
		name = (isunion and 'union' or 'struct')..' '..name
	end

	if self:canbe('{', 'symbol') then
		if not name then
			error("struct/union expected name or {")
		end

		local ctype = self:getctype(name)
		assert(ctype, "couldn't find type "..tostring(name))
		-- TODO in the case of typedef calling this
		-- the 'struct XXX' might not yet exist ...
		-- hmm ...
		return ctype
	end

	-- struct [name] { ...
	local ctype = self:node('_ctype', {
		name = name,	-- auto-generate a name for the anonymous struct/union
		fields = newtable(),
		isunion = isunion,
		parser = self,
	})

	while true do
--DEBUG:print('field first token', token, tokentype)
		if self:canbe('}', 'symbol') then
			break
		elseif self:canbe('struct', 'keyword')
		or self:canbe('union', 'keyword')
		then
			-- TODO
			-- if the next token is a { then parse a stuct
			-- if the next token is a name then ...
			-- 	if the next after that is { then parse a struct
			--  if the next after that is not then it's a "struct" typename...

			-- nameless struct/union's within struct/union's ...
			-- or even if they have names, the names should get ignored?
			-- or how long does the scope of the name of an inner struct in C last?

			local nestedtype = self:parseStruct(token == 'union')
--DEBUG:assert.is(nestedtype, self.ast._ctype)
			ctype.fields:insert(self:node('field', {
				name = '',
				type = nestedtype,
			}))
			-- what kind of name should I use for nameless nested structs?
			self:mustbe(';', 'symbol')

		elseif self:canbe(nil, 'name') then
			-- should these be keywords?
			local signedness = self:parseSignedUnsignedShortLong()
			local const = self:canbe('const', 'keyword')
			local name = self:canbe(nil, 'name')
			if signedness then
				name = signedness..' '..name
			end

			-- fields ...
			-- TODO this should be 'parsetype' and work just like variable declarations
			-- and should be interoperable with typedefs
			-- except typedefs can't use comma-separated list (can they?)
			local baseFieldType = assert(self:getctype(name), "couldn't find type "..name)
--DEBUG:assert.is(baseFieldType, self.ast._ctype)

			while true do
				local fieldtype = baseFieldType
				while self:canbe('*', 'symbol') do
					fieldtype = self:getptrtype(fieldtype)
					-- const-ness ... meh?
					self:canbe('const', 'keyword')
				end

				local fieldname = self:mustbe(nil, 'name')
				-- can you have a variable named __attribute__ ?
				if fieldname == '__attribute__' then
					self:mustbe('(', 'symbol')
					self:mustbe('(', 'symbol')
					self:mustbe('packed', 'name')
					self:mustbe(')', 'symbol')
					self:mustbe(')', 'symbol')

					fieldname = self:mustbe(nil, 'name')
				end

				while self:canbe('[', 'symbol') do
					local count = self:nustbe(nil, 'number')
					assert.gt(count, 0, "can we allow non-positive-sized arrays?")
					self:mustbe('[', 'symbol')
					fieldtype = getArrayType(fieldtype, count)
				end

				local field = self:node('_field', {
					name = fieldname,
					type = fieldtype,
				})
				ctype.fields:insert(field)

				if self:canbe(';', 'symbol') then
					break
				elseif self:canbe(',', 'symbol') then
					error("expected , or ;, found "..tostring(tokentype).." "..tostring(token))
				end
			end
		else
			error("got end of string")
		end
	end

	return ctype
end

return C_H_Parser
