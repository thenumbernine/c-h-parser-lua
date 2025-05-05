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
function _field:serialize(consume)
	self.type:serialize(consume)
	assert(self.name, 'some fields are anonymous...')
	consume(self.name)
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

	self.fields = args.fields
	self.isTypedef = args.isTypedef
	self.isEnum = args.isEnum
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

function _ctype:serialize(consume, varname)
	-- TODO should a ctype be a typedef?  nah?  esp with name being dif things
	if self.isTypedef then
		consume'typedef'
		self.baseType:serialize(consume)
		consume(self.name)
	elseif self.name then
--[[ use the stored name		
		consume(self.name)
--]]
-- [[ regenerate - same as in _ctype:init ... consoldate
		-- if it's a pointer type ...
		if self.baseType then
			if self.arrayCount then
				self.baseType:serialize(consume, varname)
				consume'['
				consume(self.arrayCount)
				consume']'
			elseif self.isPointer then
				consume(self.baseType.name)
				consume'*'
				if varname then
					consume(varname)
				end
			else
				error("???")
			end
		else
			consume(self.name)	-- anonymous here?
			if varname then
				consume(varname)
			end
		end
--]]
	else
		consume'#ERROR'
	end
end

local _symbol = nodeclass'symbol'
function _symbol:init(args)
	self.type = assert.is(assert.index(args, 'type'), _ctype)
	self.name = assert.type(assert.index(args, 'name'), 'string')
end
function _symbol:serialize(consume)
	self.type:serialize(consume, self.name)
end

-- this and symbol has a bit of overlap - this is typeless - symbol is valueless
local _enumdef = nodeclass'enumdef'
function _enumdef:init(args)
	self.name = assert.type(assert.index(args, 'name'), 'string')
	self.value = assert.type(assert.index(args, 'value'), 'number')	-- number?  or expression?  or name?
end
function _enumdef:serialize(consume)
	consume(self.name)
	consume'='
	consume(self.value)
end


return ast
