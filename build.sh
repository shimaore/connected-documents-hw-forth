egrep 'case.*opcode' ../frifri-mcu-code/main.c | sed 's/^.*case \([0-9]\+\):.*"\([^"]\+\)".*$/.inline \2 \1/' > opcodes.t
egrep 'case.*opcode' ../frifri-mcu-code/main.c | sed 's/^.*case \([0-9]\+\):.*"\([^"]\+\)".*$/\1 :opcode \2/' > opcodes.f
(cd fas && make)
cat types.t opcodes.t bootstrap.t | fas/fas > forth.bin 2>forth.bin.log

hexdump -v -e '/1 "0x%02x,\n"' forth.bin > forth.bin.h
cc -o emulate -D__EMULATE -I. ../frifri-mcu-code/main.c

FORTH_LIBS="
           opcodes.f \
           boot/char.f boot/comment.f \
           boot/branch.f boot/does.f \
           boot/misc.f boot/noname.f boot/value.f \
           boot/base.f \
           boot/string.f      \
           boot/numprint.f \
           boot/case.f \
           "

echo "Compiling $FORTH_LIBS"
cat $FORTH_LIBS | ./emulate
