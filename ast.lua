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
args:
	name
--]]
function _ctype:init(args)
	args = args or {}
	self.name = args.name
--DEBUG:assert.index(self, 'name')
--DEBUG:assert(not self.baseType)
end

function _ctype:serialize(out, varname)
	out(self.name)
	if varname then out(varname) end
end

local _ptrtype = nodeclass'ptrtype'
function _ptrtype:init(args)
	self.baseType = args.baseType
	-- TODO get rid of .name
	self.name = args.baseType.name..'*'
end
function _ptrtype:serialize(out, varname)
	self.baseType:serialize(out)
	out'*'
	if varname then out(varname) end
end

local _consttype = nodeclass'consttype'
function _consttype:init(args)
	self.baseType = args.baseType
	-- TODO get rid of .name
	self.name = args.baseType.name..' const'
end
function _consttype:serialize(out, varname)
	self.baseType:serialize(out)
	out'const'
	if varname then out(varname) end
end

local _volatiletype = nodeclass'volatiletype'
function _volatiletype:init(args)
	self.baseType = args.baseType
	-- TODO get rid of .name
	self.name = args.baseType.name..' volatile'
end
function _volatiletype:serialize(out, varname)
	self.baseType:serialize(out)
	out'volatile'
	if varname then out(varname) end
end

local _arraytype = nodeclass'arraytype'
function _arraytype:init(args)
	self.baseType = args.baseType
	self.arrayCount = args.arrayCount
	-- TODO get rid of .name
	self.name = args.baseType.name..'['..args.arrayCount..']'
end
function _arraytype:serialize(out, varname)
	self.baseType:serialize(out, varname)
	out'['
	out(self.arrayCount)
	out']'
end

local _typedef = nodeclass'typedef'
function _typedef:init(args)
	self.name = args.name
	self.baseType = args.baseType
end
function _typedef:serialize(out)
	out'typedef'
	self.baseType:serialize(out, self.name)
end

local _structType = nodeclass'structType'
function _structType:init(args)
	-- name is empty for anonymous struct/unions
	self.name = args.name
	self.isUnion = args.isUnion
	-- fields is optional, empty means it is a fwd-declare struct (which can still be used in declarations of ptrs) 
	self.fields = args.fields
end
function _structType:serialize(out, varname)
	out(self.isUnion and 'union' or 'struct')
	if self.name then out(self.name) end
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
end

local _enumType = nodeclass'enumType'
function _enumType:init(args)
	self.name = args.name
	self.baseType = args.baseType	-- always int32?
	self.enumValues = table()	-- filled out after ctor
end
function _enumType:serialize(out)
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
	self.isFuncArg = args.isFuncArg	-- do I need to store?
	self.isStructDecl = args.isStructDecl	-- do I need to store?
	self.name = args.name			-- optional for function args (isFuncArg) or anonymous struct/union fields
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
