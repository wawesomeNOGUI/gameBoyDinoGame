;Push DMA transfer routine into HRAM
;https://gbdev.io/pandocs/OAM_DMA_Transfer.html
run_dma:
LOAD "OAM DMA", HRAM
hRunDma::
    ld a, $D0
    ldh [$FF46], a  ; start DMA transfer (starts right after instruction)
    ld a, 40        ; delay for a total of 4×40 = 160 cycles
.wait
    dec a           ; 1 cycle
    jr nz, .wait    ; 3 cycles
    ret
ENDL
.end:

    ld bc, run_dma.end - run_dma ; length
    ld hl, run_dma ; source in rom
    ld de, hRunDma ; destination in ram
    call MemcpySmall

MemcpySmall:
  ld a, [de]
  ld [hli], a
  inc de
  dec c
  jr nz, MemcpySmall
  ret
