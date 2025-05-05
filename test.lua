#!/usr/bin/env luajit

local CParser = require 'c-header.parser'
local headers = CParser()

headers[[
int x;
]]

-- maybe an AST is good, for re-serialization ...
for _,ctype in ipairs(headers.ctypesInOrder) do
	print(ctype)
end
