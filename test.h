int i;
int const ic;
const int ic2;	//ooh it rearranges qualifiers to organize them and detect duplicates
volatile int iv;
int volatile iv2;
const int volatile icv;
const int volatile * volatile icvv;
int * const ica, icb;	// => int * const a; int icb;
int const * ica2, icb2;	// => int const * a; int const b;

int arr[20];
int arr2[20][30];

typedef int INT;
typedef int * PINT;
typedef int INT2, * PINT2;

struct namedStruct2 {};	//TODO parses but missing its empty fields...
struct FwdStruct;
typedef struct FwdStruct2 FwdStruct2Name;
struct FwdStructWithVar *fsv2;
struct {} anonStructVar;
struct namedStructWith {} namedStructVar;

int ifi(int i);
int (ipar);
int ((ipar2));

int if1();
int (*ifp1)();

//int if1ar[20]();			// error
//int if1ari[20](int d);	// error
int (*if1arip[20])(int d);

struct memberFuncPtrStruct {
	int (*c)();	
	int (*d[20])(int d);
};

typedef struct {
	//int (*c)(int d)[20];	//function cannot return an array
	int (*c[20])(int d);	// does that make this an array of fptrs?
	int d;
} (*fp)(int x, float y[20]);

int *ip;
int *(ip2);
int *(*ip3);
int *(*(ip4));
int **ipp;
int * (* (* * * parsanywhere));

struct structFieldInt { int i; };
struct EmptyBitifled { int i : 1; };	//error

int funcOfAnonArg(int);
void funcOfAnonArray(int[20]);

struct {
	//int (*c)(int d)[20];	//function cannot return an array
	int (*c[20])(int d);	// does that make this an array of fptrs?
	int d;
} s;

//int();	//nope, needs a name.
int (x)(int);

//I guess 'const volatile' qualifiers only apply to pointers or to types, and can't have parenthesis separating them.
// (even though they can be separated in their application via typedefs, which act like parenthesis ...)
//int (*(const nestedparsquals));		// can't have parenthesis separating const on pointers
//int (*(volatile nestedparsquals));	// can't have parenthesis separating const on pointers
//int (const (nestedparsquals));		// can't have parenthesis separating const on pointers

//struct structEmptyInt { int; };			//warns: "does not declare anything"
struct structEmptyBitifled { int : 1; };	//no warnings...

// can i declare a symbol with no name? like i do types? yes.

struct A {
//	int;	// warning nothign declared
};			// but empty is fine

struct FwdStruct;
//struct FwdStructConst const;	// warning: 'const' ignored on this declaration [-Wmissing-declarations]
typedef struct FwdStruct2 FwdStruct2Name;
//struct FwdStructWithVar fsv1;	// fails cuz the struct wasn't defined.
struct FwdStructWithVar *fsv2;


// no warnings
struct B {
	struct {
		struct {
		};
	};
};

struct Q1 { } const Q1a, *Q1b;
const struct Q2 { } Q2a, *Q2b;

//typedef int;	// warning typedef requires a name
//double;	// warning nothing declared... but not an error?

//int;		// warning
//int[10];	// error

//int (*name)();			// works
//int (*)();				// error - need a name
//void vfpf( int(*)() );	// works - dont need a name

// oh yeah that's a thing too ...
int (paren), (* const paren2);
// as many ( ) as you want in fact
int ((paren));
// but ofc you need a name with ()'s
//int ();
// also an error:
//int (*);
// ... but you still can't lead subdecls with 'const'
//int (paren), (const * paren2);
// arrays?  yup.
int (parenarray)[1];
// on either side too ... smh
int (parenarray2[1]);
int ((parenarray2b[1]));
// and nested
int ((parenarray3))[1];
// and in between
int ((parenarray4)[1]);
// one of those is for typedef'ing array types ...

int * (* (* * * parsanywhere));

//int fa()[1];		// error: cannot return array type ... to the rhs-most array goes to the return-type ...
//int fa[1]();		// error: 'fa' declared as array of functions of type 'int ()' ... so the inner-most array is a variable array definition.
//int (*fa)[1]();	// same ... so lhs of func args but still out-of-parenthesis is still an array-of-functions...
int (*fa[1])();		// actually works.

//int (const a);	// not even with parentehsis you still can't lead subdecls with const.

// multiple ()'s wrapping the function name and the entire name+arglist is fine
// but no more than 1 () wrapping the arg list.
int funcpar();
int (funcpar2)();
int ((funcpar3))();
int (funcpar3());
int ((funcpar4)());
int (((funcpar5)()));
int ((((funcpar6))()));
//int ((((funcpar7))(())));
//int ((((funcpar7))())());	// goes without saying, we only get one function-arg list

// bitfields don't go outside struct.
//int bitfield : 1;

// bitfields go inside struct
struct { int bitfield : 1; } structbitfield;

int a,
	*b,
	* const b2,
	(*b3),		// is this any dif? there's par in here?
	c[2],
	d,
	//const d2,	// can't lead with const for comma-sep
	(*e)(),
	e2(),
	//e3[20](),	// can't do this tho
	* const ** const * q,
	(*f[20])(int);

//typedef int A, B, C;

typedef struct {
	int a;
} const A2, *B2, (*C2[20])();

typedef const struct {
	int a;
} A, *B, (*C[20])();

enum Enum1;
//enum Enum2 {};	// error
//const enum Enum2;	// warning: const ignored
enum Enum2 { Enum2_Foo };

enum EnumName3 { EnumName3_Foo = 4 };
const enum EnumDeclName4 { EnumName4_Foo, } enumdeclname4_foo1, *enumdeclname4_foo2;

const enum Enum3 {	// const goes to the subdecls
	Enum3_Foo,
} enum3_foo1, *enum3_foo2;

extern int externfunctype1(int);
int extern externfunctype2(int);

int f1(), f2(int);

//int() f3, f4, f5; 	//can't have parenthesis after the type on the left side
//(void*)() f3, f4, f5;	//nope, can't.

volatile int voli;
int volatile voli2;
volatile int voli3;
//volatile int volatile voli;	//warning duplicate 'voliatile' declaration specifier 
//int volatile volatile voli;	//warning duplicate 'voliatile' declaration specifier 

// if i can do `int volatile` then can i do `struct {} volatile` as well?  yes.
struct { int foo; } volatile const * fvps;

//int ia, volatile ivb;	// volatile has to be on the lhs
//volatile int ia, * const ib;

// same for static etc?
//int static is;		// warning unused static ... ofc.  static goes on rhs like volatile

//void inline * const * vif();	// 'inline' can go right of the typename ... but not of the *'s or their const attrs 

typedef volatile int volatileinttype;	// works
//typedef static int staticinttype;		// doesn't work
//typedef extern int externinttype;		// doesn't work
//typedef inline int inlineinttype;		// doesn't work

typedef int const * volatile * const volatile * volatile const vvvp;

// more lhs type-qualifiers:
const volatile extern int constvolatilestaticint;

// can you put a struct in a funciton arg? 
// yes, tho vim doesn't think so.
int funcofstruct(struct { int a; } ((a))[20]);

// "warning: declaration of 'struct FA' will not be visible outside of this function [-Wvisibility]"
// Wow, sure enough, you can use the struct inside the function only.
// So names on structs on function-args is valid, but they don't go to the global scope.
//void funcofstruct2(struct FA { int a; } ((a))[20]) {
//	printf("%lu\n", sizeof(struct FA));
//}

// can you typedef a function type?
// yup.
typedef void FuncType(int, char, double);

// can you do 'int a,b' but as func args?
// nope.
//int funcofsubdecls(int a, b);
