local string = require 'ext.string'
local assert = require 'ext.assert'

local Tokenizer = require 'parser.base.tokenizer'
local C_H_Tokenizer = Tokenizer:subclass()

function C_H_Tokenizer:initSymbolsAndKeywords()
	for w in ([[... * ( ) { } [ ] ; : , = -]]):gmatch('%S+') do
		self.symbols:insert(w)
	end

	-- self.keywords lets the tokenizer flag if this is a reserved word or not, that's all
	for w in ([[
const enum extern
struct union
typedef
static
extern
inline
__inline
__inline__
volatile
	]]):gmatch'%w+' do
		self.keywords[w] = true
	end
end

C_H_Tokenizer.singleLineComment = string.patescape'//'

function C_H_Tokenizer:parseBlockComment()
	local r = self.r
	if not r:canbe'/%*' then return end
	local start = r.index
	if not r:seekpast'%*/' then
		error{msg="expected closing block comment"}
	end
	r.lasttoken = r.data:sub(start, r.index - #r.lasttoken - 1)
	return true --r.lasttoken
end

function C_H_Tokenizer:parseHexNumber()
	local r = self.r
	local token = r:mustbe('[%da-fA-F]+', 'malformed number')
	if r:canbe'LL' then
		token = token .. 'LL'
	elseif r:canbe'ULL' then
		token = token .. 'ULL'
	end
	coroutine.yield('0x'..token, 'number')
end

function C_H_Tokenizer:parseDecNumber()
	local r = self.r
	local token = r:canbe'[%.%d]+'
	assert.le(#token:gsub('[^%.]',''), 1, 'malformed number')
	if r:canbe'e' then
		token = token .. r.lasttoken
		token = token .. r:mustbe('[%+%-]%d+', 'malformed number')
	elseif r:canbe'LL' then
		token = token .. 'LL'
	elseif r:canbe'ULL' then
		token = token .. 'ULL'
	end
	coroutine.yield(token, 'number')
end

return C_H_Tokenizer
