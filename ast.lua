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
function _ctype:init(name)
	self.name = name
end
function _ctype:serialize(out)
	out(self.name)
end

local _ptr = nodeclass'ptr'
function _ptr:init(ch)
	self[1] = ch
end
function _ptr:serialize(out)
	out'*'
	self[1]:serialize(out)
end

local _const = nodeclass'const'
function _const:init(ch)
	self[1] = ch
end
function _const:serialize(out)
	out'const'
	self[1]:serialize(out)
end

local _volatile = nodeclass'volatile'
function _volatile:init(ch)
	self[1] = ch
end
function _volatile:serialize(out)
	out'volatile'
	self[1]:serialize(out)
end

local _array = nodeclass'array'
function _array:init(ch, count)
	self[1] = ch
	self.count = count
end
function _array:serialize(out)
	self[1]:serialize(out)
	out'['
	out(self.count)
	out']'
end

local _bitfield = nodeclass'bitfield'
function _bitfield:init(ch, bits)
	self[1] = ch
	self.bits = bits
end
function _bitfield:serialize(out)
	self[1]:serialize(out)
	out':'
	out(self.bits)
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


--[[
A 'decl' is going to be one statement / struct-field / func-arg which has:
- a base-type , the lhs-most type of a C declaration, which comes with stmt-qualifiers like 'static', 'inline', 'extern', as well as type-qualifiers like 'const', 'volatile'
- a table of .subdecls, which will be comma-separate sub-declarations, that each have their own unique sub-type and name (or no name for function-args or anonymous-struct-fields).
--]]
local _decl = nodeclass'decl'
function _decl:init(baseType, subdecls, stmtQuals)
	self.baseType = baseType
	self.subdecls = subdecls
	self.stmtQuals = stmtQuals
end
function _decl:serialize(out)
	outputStmtQuals(self.stmtQuals, out)
	self.baseType:serialize(out)
	local sep
	for _,subdecl in ipairs(self.subdecls) do
		if sep then out(sep) end
		subdecl:serialize(out)
		sep = ','
	end
end

local _typedef = nodeclass'typedef'
function _typedef:init(decl)
	self[1] = decl
end
function _typedef:serialize(out)
	out'typedef'
	self[1]:serialize(out)
end

local _structType = nodeclass'structType'
function _structType:init(args)
	-- name is empty for anonymous struct/unions
	self.name = args.name
	self.isUnion = args.isUnion
	-- fields is optional, empty means it is a fwd-declare struct (which can still be used in declarations of ptrs)
	self.fields = args.fields
end
function _structType:serialize(out)
	out(self.isUnion and 'union' or 'struct')
	if self.name then out(self.name) end
	if self.fields then
		out'{'
		if #self.fields > 0 then
			out'\n'
			for _,field in ipairs(self.fields) do
				field:serialize(out)
				out';'
			out'\n'
			end
		end
		out'}'
	end
end

-- pars should be a form of subdecl that just wraps subdecls ...
-- this is going to make subdecl a part of the ast serialization ...
local _partype = nodeclass'partype'
function _partype:init(arg)
	self[1] = arg
end
function _partype:serialize(out)
	out'('
	self[1]:serialize(out)
	out')'
end

-- sub-declaration, when you get ()'s after the subdecl name, that means the subdecl type turns into a function-type
local _funcType = nodeclass'funcType'
function _funcType:init(ch, funcArgs)
	self[1] = ch
	self.funcArgs = funcArgs
end
function _funcType:serialize(out)
	self[1]:serialize(out)
	out'('
	local sep
	for _,arg in ipairs(self.funcArgs) do
		if sep then out(sep) end
		arg:serialize(out)
		sep = ','
	end
	out')'
end

-- fwd decl for struct { ... }; and enum { ... };
-- wrapper to let the parser know that a stmt was a single struct -- no typedef, no vars, no nothing.
-- then it goes on the 'declTypes'
local _fwdDecl = nodeclass'fwdDecl'
function _fwdDecl:init(startType)
	self[1] = startType
end
function _fwdDecl:serialize(out)
	self[1]:serialize(out)
end

local _enumType = nodeclass'enumType'
function _enumType:init(args)
	self.name = args.name
	self.baseType = args.baseType	-- always int32?
	self.enumFields = table()	-- filled out after ctor
end
function _enumType:serialize(out)
	out'enum'
	if self.name then out(self.name) end
	if #self.enumFields > 0 then
		out'{'
		for _,field in ipairs(self.enumFields) do
			field:serialize(out)
			out','
		end
		out'}'
	end
end

-- this and symbol has a bit of overlap - this is typeless - symbol is valueless
local _enumdef = nodeclass'enumdef'
function _enumdef:init(name, value)
	self.name = name
	self.value = value
end
function _enumdef:serialize(out)
	out(self.name)
	if self.value then
		out'='
		out(self.value)
	end
end

return ast
