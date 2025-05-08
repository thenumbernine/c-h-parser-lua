local assert = require 'ext.assert'
local table = require 'ext.table'
local tolua = require 'ext.tolua'
local BaseAST = require 'parser.base.ast'

local ast = {}

local AST = BaseAST:subclass()

ast.node = AST

function AST:toC()
	local s = ''
	local sep = ''

	local dontSpace = {
		['\t'] = true,
		['\n'] = true,
		[';'] = true,
		['('] = true,
		[')'] = true,
		['['] = true,
		[']'] = true,
		['{'] = true,
		['}'] = true,
		[','] = true,
		['*'] = true,
	}

	local out = setmetatable({}, {
		__call = function(out, x)
--DEBUG:assert.type(x, 'string')
--DEBUG:print('AST:toC out', require 'ext.tolua'(x))
			local lastChar = s:sub(-1)
			if x == '\n' then
				s = s .. x
			elseif lastChar == '-' and x:match'^%d' then -- "-" + digit = "-"+digit
				s = s .. x
			elseif lastChar == ' ' and (x == ')' or x == '[') then	-- " "+")" => ")", " "+"[" = "["
				s = s:sub(1,-2) .. x
			elseif lastChar == ')' and x:match'^[_%w]' then	-- ') '+name
				s = s .. sep .. x
			elseif lastChar == '(' and x == '*' then -- no space between (*
				-- also, if the char before (* is a %w then put a space between %w and (
				if s:sub(-2,-2):match'%w' then
					s = s:sub(1,-2)..' (*'
				else
					s = s .. x
				end
			elseif lastChar == '*' and x == '*' then	-- no space between **
				s = s .. x
			elseif lastChar == '}' and x == ';' then	-- no space for };
				s = s .. x
			elseif x == '*' or x == '{' then					-- put only space before these
				s = s .. sep .. x
			elseif lastChar == '}' or lastChar == ',' then		-- space after only
				s = s .. sep .. x
			elseif lastChar == ' ' and x == ',' then	-- TODO when do we output spaces before commas?.  This is showing up in function args.
				s = s:sub(1,-2) .. x
			elseif dontSpace[x] or dontSpace[lastChar] then
				s = s .. x	-- no sep
			else
				s = s .. sep .. x
			end
			sep = ' '
		end,
	})
	out.tabs = 0
	self:serialize(out)
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


-- used in attributes, should also be used in _var's name and in _ctype's name maybe but idk
local _name = nodeclass'name'
function _name:init(arg)
	self[1] = assert.type(arg, 'string')
end
function _name:serialize(out)
	out(self[1])
end

local _string = nodeclass'string'
function _string:init(arg)
	self[1] = assert.type(arg, 'string')
end
function _string:serialize(out)
	out(tolua(self[1]))
end

local _number = nodeclass'number'
function _number:init(value)
	self[1] = value
end
function _number:serialize(out)
	out(tostring(self[1]))
end

local _ctype = nodeclass'ctype'
function _ctype:init(name)
	self.name = name
end
function _ctype:serialize(out)
	if self.name and self.name ~= '' then out(self.name) end
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

local _restrict = nodeclass'restrict'
function _restrict:init(ch)
	self[1] = ch
end
function _restrict:serialize(out)
	out'restrict'
	self[1]:serialize(out)
end

local _array = nodeclass'array'
function _array:init(ch, count)
	self[1] = ch
	self[2] = count
end
function _array:serialize(out)
	self[1]:serialize(out)
	out'['
	if self[2] then self[2]:serialize(out) end	-- can be empty
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
	-- attributes always
	if qualSet.__attribute__ then
		for _,attr in ipairs(qualSet.__attribute__) do
			attr:serialize(out)
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

local _subdeclAttrQual = nodeclass'subdeclAttrQual'
function _subdeclAttrQual:init(subdecl, attrs)
	self[1] = subdecl
	self.attrs = attrs
end
function _subdeclAttrQual:serialize(out)
	self[1]:serialize(out)
	for _,attr in ipairs(self.attrs) do
		attr:serialize(out)
	end
end

local _var = nodeclass'var'
function _var:init(name)
	self[1] = name
end
function _var:serialize(out)
	out(self[1])
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
	if self.name and self.name ~= '' then out(self.name) end
	if self.fields then
		out'{'
		if #self.fields > 0 then
			out'\n'
			out.tabs = out.tabs + 1
			for _,field in ipairs(self.fields) do
				out(('\t'):rep(out.tabs))
				field:serialize(out)
				out';'
				out'\n'
			end
			out.tabs = out.tabs - 1
			out(('\t'):rep(out.tabs))
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
	if #self.funcArgs == 1
	and _ctype:isa(self.funcArgs[1])
	and self.funcArgs[1].name == 'void'
	then
		-- special-case to convert '(void)' to '()'
	else
		local sep
		for _,arg in ipairs(self.funcArgs) do
			if sep then out(sep) end
			arg:serialize(out)
			sep = ','
		end
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
function _enumType:init(name, fields)
	self.name = name
	self.fields = fields
end
function _enumType:serialize(out)
	out'enum'
	if self.name and self.name ~= '' then out(self.name) end
	if self.fields then
		out'{'
		out'\n'
		out.tabs = out.tabs + 1
		for _,field in ipairs(self.fields) do
			out(('\t'):rep(out.tabs))
			field:serialize(out)
			out','
			out'\n'
		end
		out.tabs = out.tabs - 1
		out(('\t'):rep(out.tabs))
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
	if self.name and self.name ~= '' then out(self.name) end
	if self.value then
		out'='
		self.value:serialize(out)
	end
end

local _vararg = nodeclass'vararg'
function _vararg:serialize(out)
	out'...'
end

local _attr = nodeclass'attr'	-- __attribute(( ... ))
function _attr:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
	self.n = select('#', ...)
end
function _attr:serialize(out)
	out'__attribute__'
	out'('
	out'('
	for i=1,self.n do
		self[i]:serialize(out)
	end
	out')'
	out')'
end

local _call = nodeclass'call'
function _call:init(name, args)
	self[1] = name
	self.args = args
end
function _call:serialize(out)
	self[1]:serialize(out)
	out'('
	for i,arg in ipairs(self.args) do
		if i > 1 then out',' end
		arg:serialize(out)
	end
	out')'
end

local _op = nodeclass'op'
function _op:init(...)
	for i=1,select('#', ...) do
		self[i] = select(i, ...)
	end
end
function _op:serialize(out)
--DEBUG:print('_op:serialize', self.op)
	for i,x in ipairs(self) do
		x:serialize(out)
		if i < #self then out(self.op) end
	end
end

for _,info in ipairs{
	{'add','+'},
	{'sub','-'},
	{'mul','*'},
	{'div','/'},
	{'mod','%'},
	{'lt','<'},
	{'le','<='},
	{'gt','>'},
	{'ge','>='},
	{'eq','=='},
	{'ne','!='},
	{'and','&&'},
	{'or','||'},
	{'band', '&'},
	{'bxor','^'},
	{'bor', '|'},
	{'shl', '<<'},
	{'shr', '>>'},
} do
	local cl = nodeclass(info[1], _op)
	cl.op = info[2]
end

for _,info in ipairs{
	{'unm','-'},
	{'not','!'},
	{'bnot','~'},
} do
	local cl = nodeclass(info[1], _op)
	cl.op = info[2]
	function cl:init(...)
		for i=1,select('#', ...) do
			self[i] = select(i, ...)
		end
	end
	function cl:serialize(out)
		out(self.op)
		self[1]:serialize(out)	-- spaces required for 'not'
	end
end

return ast
