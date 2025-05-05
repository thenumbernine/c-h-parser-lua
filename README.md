This is going to be a C header parser.  No C function bodies allowed.

I'm going to combine it with my [preproc](https://github.com/thenumbernine/preproc-lua) repo for C parsing on preprocessed headers.

Then I'll move the make / generate / include-list into its own repo ,maybe the old [include-lua](https://github.com/thenumbernine/include-lua) repo

Then from both preproc & C parser, collect the following:
    - comments / unused lines
    - typedef section
        - require's that were replacing #include files
        - structs, unions, typedefs - preserving order for interdependency's sake.  I guess I could make a DAG out of them all and output them that way.
    - libwrapper section
        - lib, if specified
        - enums
        - function & variable prototypes
        - macros that honestly need to be hand-tuned to Lua content

