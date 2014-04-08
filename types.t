.inline true        16777215  \ 0xffffff
.inline neg_mask     8388608  \ 0x800000
.inline imm_set            1  \ flag
.inline addr_flash   8388608  \ 0x800000 base address for Flash access
.inline addr_ram     4194304  \ 0x400000 base address for RAM access
.inline addr_rp0     4198400  \ 0x401000 base address for RP (grows downwards)
.inline addr_sp0     4202496  \ 0x402000 base address for SP (grows downwards)
.inline addr_tib1    4202496  \ 0x402000 base address for first input buffer
.inline addr_tib2    4204544  \ 0x402800 base address for second input buffer
.inline addr_pad     4208640  \ 0x403800 base address for pad (may grow up or down)
