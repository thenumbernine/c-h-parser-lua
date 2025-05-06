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


local _ctype = nodeclass'ctype'

local _field = nodeclass'field'
function _field:init(args)
	self.name = assert.type(assert.index(args, 'name'), 'string')
	self.type = assert.is(assert.index(args, 'type'), _ctype)
end
function _field:serialize(out)
	self.type:serialize(out)
	assert(self.name, 'some fields are anonymous...')
	out(self.name)
end

--[[
args:
	name = it could be nameless for typedef'd or anonymous nested structs
	anonymous = true for name == nil for nested anonymous structs/unions
	fields = it will only exist for structs
	isunion = goes with fields for unions vs structs
	baseType = for typedefs or for arrays
	arrayCount = if the type is an array of another type
	isPrimitive = if this is a primitive type
	isPointer = is a pointer of the base type

TODO parser needs to be in here so just use this one and not nodeclass?
or move this one's parser-specific code into CParser.ast:node() ?

usage: (TODO subclasses?)
primitives: name isPrimitive size get set
typedef: name baseType
array: [name] baseType arrayCount
pointer: baseType isPointer
struct: [name] fields [isunion]
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
			else
				error("_ctype:init with baseType, isn't arrayCount, isn't isPointer, isn't isConst ...")
			end
		else
			self.name = nextuniquename()
			self.anonymous = true
		end
--DEBUG:print('...CType new name', self.name)
	else
--DEBUG:print('...CType already has name', self.name)
	end
	self.parser = assert.index(args, 'parser')	-- there for prims, not needed otherwise
	assert(not self.parser.ctypes[self.name], "tried to redefine "..tostring(self.name))
	self.parser.ctypes[self.name] = self

	self.fields = args.fields
	self.isTypedef = args.isTypedef
	self.isEnum = args.isEnum
	self.isConst = args.isConst
	self.isunion = args.isunion
	self.baseType = args.baseType
	self.arrayCount = args.arrayCount
	self.isPrimitive = args.isPrimitive
	self.isPointer = args.isPointer

	assert(not (self.arrayCount and self.fields), "can't have an array of a struct - split these into two CTypes")

	if self.isPointer then
		assert(self.baseType)
	elseif self.isPrimitive then
	elseif self.arrayCount then
	elseif not self.fields then
	else
		-- struct?
		assert(self.fields)
		-- expect :finalize to be called later
	end

--DEBUG:print('setting ctype['..self.name..'] = '..tostring(self))
end

function _ctype:serialize(out, varname)
	-- TODO should a ctype be a typedef?  nah?  esp with name being dif things
	if self.isTypedef then
		out'typedef'
		self.baseType:serialize(out)
		out(self.name)
	elseif self.name then
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
			else
				error("_ctype:serialize this should match the :init's name determination ...")
			end
		else
			out(self.name)	-- anonymous here?
			if varname then
				out(varname)
			end
		end
--]]
	else
		out'#ERROR'
	end
end

local _symbol = nodeclass'symbol'
function _symbol:init(args)
	self.type = assert.is(assert.index(args, 'type'), _ctype)
	self.name = assert.type(assert.index(args, 'name'), 'string')
end
function _symbol:serialize(out)
	self.type:serialize(out, self.name)
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
	self.parser = assert.index(args, 'parser')
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
	self.type:serialize(out)
	if self.name then out(self.name) end
end

-- qualifiers unique to statement
-- don't include "const" because that is forwarded on to the first subdecl
local stmtQuals = table{'static', 'extern', 'inline', 'volatile'}
local function outputStmtQuals(qualSet, out)
	if not qualSet then return end
	for _,q in ipairs(stmtQuals) do
		if qualSet[q] then
			out(q)
		end
	end
end

local _func = nodeclass'func'
function _func:init(args)
	self.parser = assert.index(args, 'parser')
	self.subdecl = assert.index(args, 'subdecl')
	self.args = assert.index(args, 'args')
	--.stmtQuals can be added later
end
function _func:serialize(out)
	outputStmtQuals(self.stmtQuals, out)
	self.subdecl:serialize(out)
	out'('
	for _,arg in ipairs(self.args) do
		arg:serialize(out)
	end
	out')'
end

local _var = nodeclass'var'
function _var:init(args)
	self.parser = assert.index(args, 'parser')
	self.subdecl = assert.index(args, 'subdecl')
	--.stmtQuals can be added later
end
function _var:serialize(out)
	outputStmtQuals(self.stmtQuals, out)
	self.subdecl:serialize(out)
end

return ast
