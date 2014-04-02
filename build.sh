egrep 'case.*opcode' ../frifri-mcu-code/main.c | sed 's/^.*case \([0-9]\+\):.*"\([^"]\+\)".*$/.inline \2 \1/' > opcodes.t
(cd fas && make)
cat types.t opcodes.t bootstrap.t | fas/fas > forth.bin

