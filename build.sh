#!/bin/bash
egrep 'case.*opcode' ../connected-documents-mcu-code/main.c | sed 's/^.*case \([0-9]\+\):.*"\([^"]\+\)".*$/.inline \2 \1/' > opcodes.t
egrep 'case.*opcode' ../connected-documents-mcu-code/main.c | sed 's/^.*case \([0-9]\+\):.*"\([^"]\+\)".*$/\1 :opcode \2/' > opcodes.f
(cd fas && make clean && make) || exit 1
cat types.t opcodes.t bootstrap.t | fas/fas > forth.bin 2>forth.bin.log || exit 1

hexdump -v -e '/1 "0x%02x, "' -e '/1 " /* %06_ax %_u */\n"' forth.bin > forth.bin.h
cc -o emulate -D__EMULATE -I. ../connected-documents-mcu-code/main.c || exit 1

FORTH_LIBS="
           opcodes.f boot/rstack.f \
           boot/char.f boot/comment.f \
           boot/branch.f boot/does.f \
           boot/misc.f boot/noname.f boot/value.f \
           boot/umul.f boot/udiv.f boot/base.f \
           boot/string.f      \
           boot/numprint.f \
           boot/case.f \
           "

echo "Compiling $FORTH_LIBS"
cat $FORTH_LIBS | ./emulate > flash.bin 2> emulate.log
