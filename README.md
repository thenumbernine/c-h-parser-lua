# C Header Parser in Lua

## Use

This is a C header parser.  No C function bodies allowed.

Before this I using my [C-preprocessor in Lua](https://github.com/thenumbernine/preproc-lua) to generate my [LuaJIT binding code from C headers](https://github.com/thenumbernine/lua-ffi-bindings) of various libraries / include files.
This was fine, but sometimes I would need ot organize the results by hand.
Especially when using my [proxying the LuaJIT table](https://github.com/thenumbernine/lua-ffi-bindings/blob/master/libwrapper.lua) that tends to overflow.
In that case I'd have to separate by hand the requires, the types, the enums, the symbols, and the uncompiled macros.

Act 2, I made a [Lua+FFI emulation in JS](https://github.com/thenumbernine/glapp-js/).
The oldest proof-of-concept of this implemented Lua's FFI module in [pure lua](https://github.com/thenumbernine/glapp-js/blob/64b297b21041f1d3e3d675058b8d2adfc39c0d1d/ffi.lua).
From this I had a `ffi.cdef` parser in Lua just lying around.

This target use of this project is to wedge in between the [preprocessor](https://github.com/thenumbernine/preproc-lua)
and the [LuaJIT bindings](https://github.com/thenumbernine/lua-ffi-bindings).
Especially for the purpose of writing those [libwrappers](https://github.com/thenumbernine/lua-ffi-bindings/blob/master/libwrapper.lua).

Final stage is probably going to be moving the `make.lua`, `generate.lua`, `include-list.lua` out of `preproc` and into a half-started repo of the same purpose: [include-lua](https://github.com/thenumbernine/include-lua).
Then it would be `include-lua` -> `c-header-lua` -> `preproc-lua` (rename that one to c-preproc maybe?).

Extra points if I convert preproc to use [parser-lua](https://github.com/thenumbernine/lua-parser), as I'm writing this C header parser to use `parser-lua`.

Extra double points if I use `include-lua` for its original purpose.  That is make it so it either includes a binding header or generates one live.
I could couple this with the current `lua-ffi-bindings` folder that is already searching for the binding headers per-os/arch/etc, to make it so if the file isn't found the it just goes to the `include-lua` for automatic generation if the header is accessible.
