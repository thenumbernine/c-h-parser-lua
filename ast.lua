local assert = require 'ext.assert'
local table = require 'ext.table'
local BaseAST = require 'parser.base.ast'

local ast = {}

local AST = BaseAST:subclass()

ast.node = AST

function AST:toC(args)
	return self:serializeRecursiveMember('toC_recursive', args)
end

-- why distinguish toC() and serialize(consume)?
-- The need for this design pops up more in subclasses.
-- serialize(consume) is used by all language-serializations
-- toC_recursive(consume) is for Lua-specific serialization (to-be-subclassed)
-- I'm not sure if this is better than just using a fully separate table of serialization functions per node ...
-- toC() is the external API
function AST:toC_recursive(consume)
	return self:serialize(consume)
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
			else
				error("???")
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
	self.parser.ctypesInOrder:insert(self)

	self.fields = args.fields
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


return ast
