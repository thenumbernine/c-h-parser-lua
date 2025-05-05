#!/usr/bin/env luajit

local CParser = require 'c-header.parser'
local headers = CParser()

assert(headers[[

typedef int foo;

//typedef void F;

int x;
]])

-- maybe an AST is good, for re-serialization ...
print'typedefs:'
for i,ctype in ipairs(headers.ctypesInOrder:filter(function(ctype)
	return not ctype.isPrimitive
end)) do
	print('#'..i, ctype:toC()..';')
end
print()

print'symbols:'
for i,symbol in ipairs(headers.symbolsInOrder) do
	print('#'..i, symbol:toC()..';')
end
