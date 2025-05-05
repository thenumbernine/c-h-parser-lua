#!/usr/bin/env luajit

local CParser = require 'c-header.parser'
local headers = CParser()

assert(headers[[
typedef void T_v;
typedef int T_i;
typedef char * T_pc;

// TODO typedef and symbol / function decl should overlap
// typedef int y[20];

int i;
char * pc; // pointers don't work?
float f_20[20];
float * pf_20[20];
char c_20_30[20][30];
]])

-- maybe an AST is good, for re-serialization ...
print'typedefs:'
for i,ctype in ipairs(headers.declTypes) do
	print('#'..i, ctype:toC()..';')
end
print()

print'symbols:'
for i,symbol in ipairs(headers.symbolsInOrder) do
	print('#'..i, symbol:toC()..';')
end
