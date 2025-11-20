#ifndef CLOSURE_RUNTIME_H
#define CLOSURE_RUNTIME_H

#include <stdint.h>

// Closure structure: function pointer + environment pointer
typedef struct {
    void* func_ptr;
    void* env;
} closure;

// Create a closure with a function pointer and environment
closure* create_closure(void* func_ptr, void* env);

// Apply a closure to an argument (for int -> int functions)
int32_t apply_closure_i32_i32(closure* c, int32_t arg);

// Apply a closure to an argument (for int -> bool functions)
int32_t apply_closure_i32_i1(closure* c, int32_t arg);

// Apply a closure to an argument (for bool -> int functions)
int32_t apply_closure_i1_i32(closure* c, int32_t arg);

// Apply a closure to an argument (for bool -> bool functions)
int32_t apply_closure_i1_i1(closure* c, int32_t arg);

// Apply a closure to a closure argument (for higher-order functions ptr -> ptr)
closure* apply_closure_closure_closure(closure* c, closure* arg);

// Apply a closure to an int, returning a pointer (int -> ptr)
closure* apply_closure_i32_ptr(closure* c, int32_t arg);

// Apply a closure to a bool, returning a pointer (bool -> ptr)
closure* apply_closure_i1_ptr(closure* c, int32_t arg);

// Apply a closure to a pointer, returning an int (ptr -> int)
int32_t apply_closure_ptr_i32(closure* c, closure* arg);

// Apply a closure to a pointer, returning a bool (ptr -> bool)
int32_t apply_closure_ptr_i1(closure* c, closure* arg);

// Free a closure
void free_closure(closure* c);

#endif