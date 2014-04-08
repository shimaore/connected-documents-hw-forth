/*
 * fas.c - the Forth assembler
 * (c) 2009 Stephane Alnet - GPL3+
 */

#include "fas.h"
#include "../params.h"

#include <stdlib.h> /* fprintf, exit */

#define debug 1

uint8_t flash[0x80000];

g_cell here = ADDR_FLASH;

#include <stdio.h>

void dump()
{
  fwrite(flash,1,here-ADDR_FLASH,stdout);
}

void show_missing_symbols();

/* Called by yyparse on error.  */
void yyerror (char const *s)
{
 fprintf (stderr, "yyerror: '%s'\n", s);
}

/* main */

int main(void)
{
  GC_INIT();
  if(yyparse()) {
    fprintf (stderr, "yyparse failed\n");
    return 1;
  }
  show_missing_symbols();
  dump();
  return 0;
}

/* Output block management */

void set_cell(g_cell addr, g_cell value)
{
  if(debug) fprintf(stderr, "      set_cell(" DISPLAY_CELL_HEX ") to " DISPLAY_CELL_HEX "\n",addr,value);
  addr -= ADDR_FLASH;
  flash[addr+0] = (value >> 16) & 0xff;
  flash[addr+1] = (value >>  8) & 0xff;
  flash[addr+2] = (value >>  0) & 0xff;
}

void set_char(g_cell addr, g_char value)
{
  if(debug) fprintf(stderr, "      set_char(" DISPLAY_CELL_HEX ") to %02x (%c)\n",addr,value,value);
  addr -= ADDR_FLASH;
  flash[addr] = value;
}


typedef struct Cell Cell;
struct Cell {
  g_cell    value;
  Cell*     next;
};

Cell* new_cell(g_cell car, Cell* cdr)
{
  Cell* node = gc_malloc(sizeof(Cell));
  node->value = car;
  node->next = cdr;
  return node;
}

typedef struct Atom Atom;
struct Atom {
  UT_hash_handle  hh;
  short           defined;
  g_cell          value;
  Cell*           backref_list;
  char            name[];
};

Atom* atoms = NULL;

Atom* new_atom(char const* name)
{
  unsigned len = strlen(name)+1;
  Atom* atom = gc_malloc(sizeof(Atom)+len);
  atom->defined = 0;
  atom->value = -1;
  atom->backref_list = NULL;
  memcpy(atom->name,name,len);
  HASH_ADD_STR(atoms,name,atom);
  return atom;
}

void show_missing_symbols()
{
  Atom* this;
  for( this = atoms; this != NULL; this = this->hh.next )
  {
    if(!this->defined && this->backref_list)
    {
      fprintf(stderr,"Undefined symbol %s\n",this->name);
    }
  }
}

void compile_value(g_cell value)
{
  if(debug) fprintf(stderr, "    compile_value(" DISPLAY_CELL_HEX ") at " DISPLAY_CELL_HEX "\n",value,here);
  set_cell(here,value);
  here += 3;
}

void compile_word(char const* name)
{
  if(debug) fprintf(stderr, "  compile_word(%s)\n",name);
  Atom* atom = NULL;
  HASH_FIND_STR(atoms,name,atom);
  if(!atom)
  {
    atom = new_atom(name);
  }

  if(atom->defined)
  {
    compile_value(atom->value);
  }
  else
  {
    atom->backref_list = new_cell(here,atom->backref_list);
    here += 3;
  }
}

g_cell get_value(char const* name)
{
  Atom* atom = NULL;
  HASH_FIND_STR(atoms,name,atom);
  if(atom && atom->defined)
  {
    return atom->value;
  }
  fprintf(stderr,"Undefined value: %s\n",name);
  exit(1);
}

void resolve_backrefs(Cell* backref, g_cell value)
{
  if(backref)
  {
    set_cell(backref->value,value);
    resolve_backrefs(backref->next,value);
    backref->next = NULL;
  }
}

void create_name_here(char const* name) {
  create_name(name,here);
}

void create_name(char const* name, g_cell value)
{
  if(debug) fprintf(stderr, "create_name(%s," DISPLAY_CELL_HEX ")\n",name,value);
  Atom* atom = NULL;
  HASH_FIND_STR(atoms,name,atom);
  if(atom)
  {
    if(atom->defined)
    {
      fprintf(stderr,"Duplicate name: %s\n",name);
      exit(1);
    }
    else
    {
      resolve_backrefs(atom->backref_list,value);
      atom->backref_list = NULL;
    }
  }
  else
  {
    atom = new_atom(name);
  }
  atom->defined = 1;
  atom->value = value;
}

g_cell latest = 0;

void compile_header(char const* name, short immediate)
{
  if(debug) fprintf(stderr, "compile_header(%s)\n",name);
  g_cell new_latest = here;

  unsigned len = strlen(name);
  g_cell name_pos = here;

  // Save the name
  unsigned i;
  for( i = 0; i < len; i++ )
  {
    set_char(here+i,name[i]);
  }
  here += len;

  // NFA
  compile_value(len);       // len
  compile_value(name_pos); // addr(c)
  compile_value(immediate); // flags

  // LFA
  compile_value(latest);
  latest = new_latest;

  // PFA
  create_name(name,here);
}


void FAIL_MALLOC_FAILED()
{
  fprintf(stderr,"gc_malloc failed\n");
  exit(2);
}

void* gc_malloc( size_t sz )
{
  void* new_ptr = GC_MALLOC(sz);
  if(!new_ptr) FAIL_MALLOC_FAILED();
  return new_ptr;
}

