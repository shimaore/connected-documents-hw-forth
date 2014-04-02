#ifndef __PARAMS_H
#define __PARAMS_H

typedef __uint32_t g_cell;
typedef __uint8_t g_char;
#define DISPLAY_CELL_HEX "%06x"
#define DISPLAY_CELL "%ud"
#define imm_set 1

enum {
  ADDR_FLASH = 0x800000,
  ADDR_RAM   = 0x400000
};

#endif /* __PARAMS_H */
