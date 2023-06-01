#include "ol/ol.h"
#include <stdio.h>

struct olvm_t* vm;
unsigned char program[] = {
	#include "main.inc"
};
int main(int argc, char** argv) {
	return 0;
}

// compiled ol functions
int load_xpm3;
int load_layout;
int xmp3_to_board;
int board_texture;
int board_energize;
int board_simulate;

// high-level adapters (todo: simplify)
__attribute__((used))
void new_ol()
{
	vm = OLVM_new(program);
	OLVM_userdata(vm, &vm);
	uintptr_t
	r = OLVM_run(vm, 0, 0);

	// well, we have our "smart" script prepared,
	//  now save functions for feature use
	assert( is_vector(r) );

	load_xpm3 = ol2int(ref(r, 1));
	load_layout = ol2int(ref(r, 2));

	xmp3_to_board = ol2int(ref(r, 3));
	board_texture = ol2int(ref(r, 4));

	board_energize = ol2int(ref(r, 5));
	board_simulate = ol2int(ref(r, 6));
}

#define INT_CHARP(name, handler) \
__attribute__((used))\
int name(char* source)\
{\
	uintptr_t args[] = {\
		new_string(vm, source)\
	};\
	uintptr_t m = OLVM_evaluate(vm,\
		OLVM_deref(vm, handler),\
		sizeof(args) / sizeof(args[0]), args);\
	if (is_reference(m))\
		return OLVM_pin(vm, m);\
	return 0;\
}

INT_CHARP(ol_load_xpm3, load_xpm3)
INT_CHARP(ol_load_layout, load_layout)

// #define INT_REF(name, handler) \
// __attribute__((used)) \
// int name(int arg1)\
// {\
// 	uintptr_t args[] = {\
// 		OLVM_deref(vm, arg1)\
// 	};\
// 	uintptr_t m = OLVM_evaluate(vm,\
// 		OLVM_deref(vm, handler),\
// 		sizeof(args) / sizeof(args[0]), args);\
// 	assert (is_number(m));\
// 	return ol2int(m);\
// }

// INT_REF(ol_xpm3_get_width, xpm3_width)
// INT_REF(ol_xpm3_get_height, xpm3_height)

#define REF_REF(name, handler) \
__attribute__((used)) \
int name(int arg1)\
{\
	uintptr_t args[] = {\
		OLVM_deref(vm, arg1)\
	};\
	uintptr_t m = OLVM_evaluate(vm,\
		OLVM_deref(vm, handler),\
		sizeof(args) / sizeof(args[0]), args);\
	if (is_reference(m))\
		return OLVM_pin(vm, m);\
	return 0;\
}

REF_REF(ol_xpm3_to_board, xmp3_to_board)

__attribute__((used))
const char* ol_board_texture(int arg1)
{
	uintptr_t args[] = {
		OLVM_deref(vm, arg1)
	};
	uintptr_t m = OLVM_evaluate(vm,
		OLVM_deref(vm, board_texture),
		sizeof(args) / sizeof(args[0]), args);
	if (is_bytevector(m))
		return bytevector_value(m);
	return 0;
}

__attribute__((used))
void ol_board_energize(int board, int x, int y, int ttl)
{
	uintptr_t args[] = {
		OLVM_deref(vm, board),
		make_integer(x),
		make_integer(y),
		make_integer(ttl)
	};
	uintptr_t m = OLVM_evaluate(vm,
		OLVM_deref(vm, board_energize),
		sizeof(args) / sizeof(args[0]), args);
}

__attribute__((used))
void ol_board_simulate(int board)
{
	uintptr_t args[] = {
		OLVM_deref(vm, board)
	};
	uintptr_t m = OLVM_evaluate(vm,
		OLVM_deref(vm, board_simulate),
		sizeof(args) / sizeof(args[0]), args);
}
