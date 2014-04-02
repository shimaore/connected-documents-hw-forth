#ifndef FAS_H
#define FAS_H

/* boehmgc */
#include <gc.h>

void* gc_malloc( size_t sz );

/* uthash: http://uthash.sourceforge.net/ */
#include <uthash.h>

/* undefine the defaults */
#undef uthash_bkt_malloc
#undef uthash_bkt_free
#undef uthash_tbl_malloc
#undef uthash_tbl_free

/* re-define, specifying alternate functions */
/* for UT_hash_bucket */
#define uthash_bkt_malloc(sz) gc_malloc(sz)
#define uthash_bkt_free(ptr)  /* GC_FREE(ptr) */
/* for UT_hash_table  */
#define uthash_tbl_malloc(sz) gc_malloc(sz)
#define uthash_tbl_free(ptr)  /* GC_FREE(ptr) */

#include "../params.h"

int yylex(void);
void yyerror(char const*);

void create_name(char const* name, g_cell value);
void create_name_here(char const* name);
void compile_header(char const* name, short immediate);
void compile_word(char const* name);
g_cell get_value(char const* name);

void FAIL_MALLOC_FAILED();
int yyparse();

#endif
