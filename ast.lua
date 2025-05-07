local assert = require 'ext.assert'
local table = require 'ext.table'
local BaseAST = require 'parser.base.ast'

local ast = {}

local AST = BaseAST:subclass()

ast.node = AST

function AST:toC()
	local s = ''
	local sep = ''
	self:serialize(function(x)
		s = s .. sep .. tostring(x)
		sep = ' '
	end)
	return s
end

-- ok maybe it's not such a good idea to use tostring and serialization for the same purpose ...
AST.__tostring = string.nametostring

local function nodeclass(type, parent, args)
	parent = parent or AST
	local cl = parent:subclass(args)
	cl.type = type
	cl.__name = type
	ast['_'..type] = cl
	return cl
end
ast.nodeclass = nodeclass


local function funcArgsToC(funcArgs)
	return '('..funcArgs:mapi(function(arg) return arg:toC() end):concat', '..')'
end

local _ctype = nodeclass'ctype'
--[[
TODO get rid of all of this.
use subclasses
don't track and cache ctype names.
do track AST's of parenthesis.

args:
	name = it could be nameless for typedef'd or anonymous nested structs
	anonymous = true for name == nil for nested anonymous structs/unions
	fields = it will only exist for structs
	isStruct = this is a struct/union.  if no fields then it's a fwd-declare.
	isUnion = goes with isStruct for unions vs structs
	baseType = for typedefs or for arrays
	arrayCount = if the type is an array of another type
	isPrimitive = if this is a primitive type
	isPointer = is a pointer of the base type
	isConst
	isVolatile

usage: (TODO subclasses?)
primitives: name isPrimitive size get set
typedef: name baseType
array: [name] baseType arrayCount
pointer: baseType isPointer
struct: [name] fields [isUnion]
--]]
function _ctype:init(args)
	args = args or {}
	self.name = args.name
	if not self.name then
		-- if it's a pointer type ...
		if args.baseType then
			if args.arrayCount then
				self.name = args.baseType.name..'['..args.arrayCount..']'
			elseif args.isPointer then
				self.name = args.baseType.name..'*'
			elseif args.isConst then
				-- TODO don't use isConst and isPointer in the same ctype?
				-- or names might get mixed up or something? idk
				self.name = args.baseType.name..' const'
			elseif args.isVolatile then
				self.name = args.baseType.name..' volatile'
			else
				error("_ctype:init with baseType, isn't arrayCount, isn't isPointer, isn't isConst, isn't isVolatile ...")
			end
		else
			self.anonymous = true
		end
--DEBUG:print('...CType new name', self.name)
	else
--DEBUG:print('...CType already has name', self.name)
	end
	
	self.fields = args.fields
	self.funcArgs = args.funcArgs
	self.isTypedef = args.isTypedef
	self.isEnum = args.isEnum
	self.isConst = args.isConst
	self.isVolatile = args.isVolatile
	self.isStruct = args.isStruct
	self.isUnion = args.isUnion
	self.baseType = args.baseType
	self.arrayCount = args.arrayCount
	self.isPrimitive = args.isPrimitive
	self.isPointer = args.isPointer
end

function _ctype:serialize(out, varname)
	-- TODO should a ctype be a typedef?  nah?  esp with name being dif things
	if self.isTypedef then
		out'typedef'
		self.baseType:serialize(out)
		out(self.name)	-- .name is the typedef's name
	elseif self.isStruct then
		out(self.name or (
			self.isUnion and 'union' or 'struct'
		))
		if self.fields then
			out'{'
			for _,field in ipairs(self.fields) do
				field:serialize(out)
			end
			out'}'
		end
		if varname then
			out(varname)
		end
	else -- if self.name then
--[[ use the stored name		
		out(self.name)
--]]
-- [[ regenerate - same as in _ctype:init ... consoldate
		-- if it's a pointer type ...
		if self.baseType then
			if self.arrayCount then
				self.baseType:serialize(out, varname)
				out'['
				out(self.arrayCount)
				out']'
			elseif self.isPointer then
				out(self.baseType.name)
				out'*'
				if varname then
					out(varname)
				end
			elseif self.isConst then
				out(self.baseType.name)
				out'const'
				if varname then
					out(varname)
				end
			elseif self.isVolatile then
				out(self.baseType.name)
				out'volatile'
				if varname then
					out(varname)
				end
			else
				error("_ctype:serialize this should match the :init's name determination: "..tostring(self.name))
			end
		else
			out(self.name)	-- anonymous here?
			if varname then
				out(varname)
			end
			if self.fields then
				out'{'
				for _,field in ipairs(self.fields) do
					field:serialize(out)
				end
				out'}'
			end
--]]
		end
	end
end

-- this and symbol has a bit of overlap - this is typeless - symbol is valueless
local _enumdef = nodeclass'enumdef'
function _enumdef:init(args)
	self.name = assert.type(assert.index(args, 'name'), 'string')
	self.value = assert.type(assert.index(args, 'value'), 'number')	-- number?  or expression?  or name?
end
function _enumdef:serialize(out)
	out(self.name)
	out'='
	out(self.value)
end

--[[
this is half of a declaration.
the other half is the type
this is created and returned to the rest for creating things like
- symbols declarations (variables, functions)
- struct fields
- function args
--]]
local _subdecl = nodeclass'subdecl'
function _subdecl:init(args)
	self.isFuncArg = args.isFuncArg	-- does it matter?
	self.isStructDecl = args.isStructDecl	-- does it matter?
	self.name = args.name			-- optional for isFuncArg
	if not (self.isFuncArg) then
		-- technically anonymous declarations just give you warnings that nothing is being defined...
		assert(self.name, "expected name")
	end
	self.type = assert.index(args, 'type')
end
function _subdecl:serialize(out)
	self.type:serialize(out, self.name)
end

-- qualifiers unique to statement
-- don't include "const" and "volatile" because they are forwarded on to the first subdecl
local stmtQuals = table{'static', 'extern', 'inline'}
local function outputStmtQuals(qualSet, out)
	if not qualSet then return end
	for _,q in ipairs(stmtQuals) do
		if qualSet[q] then
			out(q)
		end
	end
end

-- subdecl of a variable or a function
-- to be used in either function-arguments, struct-fields, or declarations
local _var = nodeclass'var'
function _var:init(args)
	self.subdecl = assert.index(args, 'subdecl')
	--.stmtQuals can be added later
end
function _var:serialize(out)
	outputStmtQuals(self.stmtQuals, out)
	self.subdecl:serialize(out)
end

-- sub-declaration, when you get ()'s after the subdecl name, that means the subdecl type turns into a function-type
local _funcType = nodeclass'funcType'
function _funcType:init(args)
	self.baseType = args.baseType
	self.funcArgs = args.funcArgs

	-- TODO get rid of this
	-- what should name even be ...
	-- why am I even using name ...
	self.name = args.baseType.name .. funcArgsToC(args.funcArgs)

end
-- all ctype (subclasses? this isn't a subclass, but it is interchangeable with ctype)
--  take 'varname' as the 2nd parameter
function _funcType:serialize(out, varname)
	self.baseType:serialize(out, varname)
	out(funcArgsToC(self.funcArgs))
end



local _fwdDeclStruct = nodeclass'fwdDeclStruct'
function _fwdDeclStruct:init(args)
	self.type = assert.index(args, 'type')
end
function _fwdDeclStruct:serialize(out)
	self.type:serialize(out)
end

return ast
