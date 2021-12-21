INCLUDE "hardware.inc"

SECTION "Header", ROM0[$100]

EntryPoint: ; This is where execution begins
  di ; Disable interrupts. That way we can avoid dealing with them, especially since we didn't talk about them yet :p
  jp Start ; Leave this tiny space

REPT $150 - $104
  db 0
ENDR


SECTION "Game code", ROM0

Start:
    ; Turn off the LCD
.waitVBlank1
    ld a, [rLY]
    cp 144 ; Check if the LCD is past VBlank
    jr c, .waitVBlank1

    xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
    ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.

    ;copy game tiles to game tile ram
    ld hl, $8800
    ld de, GameTiles
    ld bc, GameTilesEnd - GameTiles
.copyGameStuff
    ld a, [de] ; Grab 1 byte from the source
    ld [hli], a ; Place it at the destination, incrementing hl
    inc de ; Move to next byte
    dec bc ; Decrement count
    ld a, b ; Check if count is 0, since `dec bc` doesn't update flags
    or c
    jr nz, .copyGameStuff

    jr dmaEnd

;Push DMA transfer routine into HRAM
;https://gbdev.io/pandocs/OAM_DMA_Transfer.html
MemcpySmall:
  ld a, [de]
  ld [hli], a
  inc de
  dec c
  jr nz, MemcpySmall
  ret

run_dma:
LOAD "OAM DMA", HRAM
;ld a, $D0 to start DMA transfer from certain high byte of RAM
hRunDma::
    ldh [$FF46], a  ; start DMA transfer (starts right after instruction)
    ld a, 40        ; delay for a total of 4×40 = 160 cycles
.wait
    dec a           ; 1 cycle
    jr nz, .wait    ; 3 cycles
    ret
ENDL

dmaEnd:
  ld c, dmaEnd - run_dma ; length
  ld hl, hRunDma ; source in rom
  ld de, run_dma ; destination in ram
  call MemcpySmall


;========================================
;Variables Setup
;========================================
    ;C000 = WRAM0 is gonna be used for movement Fixed-Point data
    ;Fixed point representation will be: 12 bits integer, lower four fractional
    ;gives you 16 fractional values
    ;shift these values right four times to get interger

    ;dino x
    ld a, 20
    ld hl, $C000
    call Load8BitIntToFixedPointInMem

;just a test to make sure shifts working
;=======================================
    ld a, [$C000]
    ld b, a
    ld a, [$C001]
    ld c, a

    call ShiftFixedPointToInt

    ld a, b
    ld [$C010], a
;=======================================
    ;dino y
    ld a, 104
    ld hl, $C002
    call Load8BitIntToFixedPointInMem

    ;dino x velocity
    ld a, 0
    ld hl, $C004
    call Load8BitIntToFixedPointInMem

    ;dino y velocity
    ld a, 0
    ld hl, $C006
    call Load8BitIntToFixedPointInMem

    ;dino gravity acceration
    ld a, 0
    ld hl, $C008
    call Load8BitIntToFixedPointInMem

    ;acceleration delay (loop through 100 times without accel)
    ld a, 100
    ld [$C010], a
    ;ld hl, $C008
    ;call Load8BitIntToFixedPointInMem

    ;dino anime xor starting variable
    ld a, $8F   ;flip flop between $8F and $98 with xor %00010111
    ld [$C011], a

    ;D000 is gonna be for storing sprite data for DMA trnsfer to OAM
    ;WRA1 = Work RAm 1 starts at $D000
    ;I'm just clearing the WRAM so I can see changes easier in the debugger
    ld hl, $D000 ;start of WRA1
  .clearWRA1
    xor a
    ld [hli], a
    ld a, l
    cp a, $9f
    jr nz, .clearWRA1

   ;Init display registers
   ld a, %11100100
   ld [rBGP], a

   xor a ; ld a, 0
   ld [rSCY], a
   ld [rSCX], a

;Clear OAM so no phantom random sprites
   ld hl, $FE00 ;start of OAM
.clearOAM
    xor a
    ld [hli], a
    ld a, l
    cp a, $9f
    jr nz, .clearOAM

;=======================================================
;Cactus setup
;=======================================================
;reg cactus
;mid
ld a, $8A
ld [$9921], a
ld a, $8D
ld [$9941], a
ld a, $93
ld [$9961], a
ld a, $8D
ld [$9981], a
ld a, $99
ld [$99A1], a

;stems
;left
ld a, $8C
ld [$9940], a
ld a, $92
ld [$9960], a
;right
ld a, $8E
ld [$9942], a
ld a, $94
ld [$9962], a

;ground
;keep changing offscreen ones to add new random ground when scrolling at .update
ld a, $98
ld [$99A0], a
ld a, $9A
ld [$99A2], a
ld a, $98
ld [$99A3], a
ld a, $9A
ld [$99A4], a
ld a, $9A
ld [$99A5], a
ld a, $9A
ld [$99A6], a
ld a, $98
ld [$99A7], a
ld a, $98
ld [$99A8], a
ld a, $98
ld [$99A9], a
ld a, $9A
ld [$99AA], a
ld a, $98
ld [$99AB], a
ld a, $98
ld [$99AC], a
ld a, $9A
ld [$99AD], a
ld a, $9A
ld [$99AE], a
ld a, $98
ld [$99AF], a
ld a, $98
ld [$99B0], a

;=======================================================
;sprite data
;=======================================================
  ;dino sprite setup
  ;dino has 24 tiles split into groups of six on the y axis

  ld b, 0     ;loop counter
  ld c, 20    ;top left x of dino
  ld d, 100    ;top left y of dino
  ld e, $80   ;starting sprite number from Sprite Table
  ld h, $D0
  ld l, $00   ;WRAM1 memory pointer (we'll use dma copies for writing to OAM)

.dinoSetup
  ld a, d
  ld [hli], a  ;y pos

  ld a, c
  ld [hli], a  ;x pos
  add a, 8
  ld c, a

  ld a, e
  ld [hli], a  ;pattern number
  inc e

  ld a, 0      ;special sprite settings (bit 7 = 0, puts sprite over background and window)
  ld [hli], a

  ;check if finished loading sprite
  ld a, e
  cp a, $88
  jr z, .lcdOn

  inc b

  ;check if b hit 6
  ld a, b
  cp a, 3
  jr nz, .dinoSetup

  ;if b == 6 change lines
  ld b, 0

  ld a, d   ;change y
  add a, 8
  ld d, a

  ld c, 20

  jr .dinoSetup

.lcdOn
  ; Shut sound down
  ld [rNR52], a

  ; Turn screen on, display background
  ;ld a, (LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON )
  ld a, (LCDCF_ON | LCDCF_BG8800 | LCDCF_BGON | LCDCF_OBJON) ;display sprites too
  ld [rLCDC], a

jr GameLoop


;================================================
;Math Routines
;================================================

; @param bc: Q12.4 fixed-point number.
ShiftFixedPointToInt::
  ; Adjust number and store in b.
  ld a, c
  rrc b
  rra
  rrc b
  rra
  rrc b
  rra
  rrc b
  rra
  ld b, a
  ret

;takes a as a parameter
;set hl to high byte e.g fixed point will be stored: $C000 and $C001
Load8BitIntToFixedPointInMem:
  ld b, 0
  rla
  rl b
  rla
  rl b
  rla
  rl b
  rla
  rl b

  ld c, a
  ld a, b

  ld [hli], a
  ld a, c
  ld [hl], a
  ret

;takes bc and de as parameters, returns bc
AddTwo16BitNumbers:
  ld a, c    ;16bit addition
  add a, e
  ld c, a
  ld a, b
  adc a, d
  ld b, a
  ret

;================================================
;Sprite Update Routines
;================================================
DinoMove:
  ld e, 0      ;row counter
  ;ld c, 20    ;top left x of dino set before call
  ;ld b, 100   ;top left y of dino set before call
  ld h, $D0
  ld l, $00   ;WRAM1 memory pointer
.updateDinoParts
;*don't use 16-bit inc or dec (ld [hli]) while accessing OAM causes glitch
;https://gbdev.gg8.se/wiki/articles/Sprite_RAM_Bug
;use 8bit inc instead
;maybe just write to WRAM1 $D000 instead and do DMA transfer though so ld [hli] is okeydokey
  ld a, b
  ld [hl], a  ;y pos
  inc l

  ld a, c
  ld [hl], a  ;x pos
  inc l

  inc l
  inc l

  add a, 8
  ld c, a      ;move x + 8

  inc e        ;inc row counter

  ;check if finished moving sprite
  ld a, l
  cp a, $20
  ret z       ;return if zero

  ;check if need to move down row
  ld a, e
  cp a, 3
  jr nz, .updateDinoParts

  ld e, 0

  ld a, c
  sub a, 24    ;row made up of 3x 8 wide sprites
  ld c, a

  ld a, b
  add a, 8     ;move down y-pos for updating next row
  ld b, a


  jr .updateDinoParts



GameLoop:

;================================================
;Dropper Script
;================================================

  ;ld b, 0    ;counter
  ld d, 0
  ld e, 1
Dropper:
.waitVBlankDropper
    ld a, [rLY]
    cp 144 ; Check if the LCD is in VBlank
    jr c, .waitVBlankDropper

    ld a, $D0
    call hRunDma

.buttonDpadTesting
    ld a, %00010000   ;looking for action input
    ;ld a, %00100000   ;looking for d-pad button input
    ld [$FF00], a

    ld a, [$FF00]  ;good to check joypad register multiple times
    ld a, [$FF00]
    ld a, [$FF00]

    ;cp a, %11101101  ;looking for left press
    ;cp a, %11101110   ;looking for right press
    ;cp a, %11101011   ;looking for up press
    cp a, %11011110   ;looking for  a
    jr nz, .update    ;if not pressing up, go straight to update
    ;Change Dino Velocity based on how long button held down (TODO)
    ;if dino is on ground
    ld a, [$D000]
    cp a, 104
    jr nz, .update

    ;C006 & 7 store y-velocity
    ;ld a, [$C006]
    ;ld b, a
    ;ld a, [$C007]
    ;ld c, a
    ;dec bc   ;16bit dec  (negative velocity moves dino up)

    ld a, $FF
    ld [$C006], a
    ld a, $E2
    ld [$C007], a

    ;jr .buttonDpadTesting  ;keep decrementing velocity if up still held down
    ;call ShiftFixedPointToInt  ;shifts bc to int, returns b int

    ;cp a, %11100111   ;looking for down press

    ;cp a, %11011101  ;looking for  b
    ;cp a, %11011110   ;looking for  a
    ;cp a, %11011011   ;looking for  select
    ;cp a, %11010111   ;looking for  start
    ;jr z, .ballDropped ;if a pressed drop ball

.update
  ;scroll background to scroll cacti and ground
  ld a, [$FF43] ;Scroll X
  inc a
  inc a
  ld [$FF43], a

.dinoLegAnimation
  ;if dino is on ground
  ld a, [$D000]
  cp a, 104
  jr c, .regDinolegs  ;dino y < 104

    ld a, [$C011]  ;anime variable (starts as $8F)
    xor a, %00010111   ;flip flop between $8F and $98
    ld [$C011], a

    ;$D01A & 1E are legs tile index
    ;tiles $8F and $90 are run1, $98 and $99 are run2, $86 and $87 are no run
    ld [$D01A], a
    inc a
    ld [$D01E], a
    jr .dinoUpdate

.regDinolegs
    ld a, $86
    ld [$D01A], a
    inc a
    ld [$D01E], a

.dinoUpdate
    ;load dino y FP into de
    ld a, [$C002]  ;dino y Fixed Point
    ld d, a
    ld a, [$C003]  ;dino y Fixed Point
    ld e, a

    ;set bc to dino y-velocity
    ld a, [$C006]  ;dino y velocity
    ld b, a
    ld a, [$C007]
    ld c, a

    ;add velocity y to dino y (store new y in bc for use in ShiftFixedPointToInt)
    call AddTwo16BitNumbers  ;returns bc

    ;store new Dino y in WRAM0
    ld a, b
    ld [$C002], a  ;dino y Fixed Point
    ld a, c
    ld [$C003], a  ;dino y Fixed Point

    call ShiftFixedPointToInt  ;returns b, which is used for dino y in DinoMove

    ld a, [$D001]
    ld c, a  ;dino x
    call DinoMove

    ;GRAVITTTTY
    ;first check for accel loop delay
    ld a, [$C010]
    dec a
    ld [$C010], a  ;store delay count
    cp a, 0
    jr nz, .checkDinoGround

    ;reset accel delay
    ld a, 5
    ld [$C010], a

    ;store grav accceceelelleration in bc
    ld a, [$C008]
    ld b, a
    ld a, [$C009]
    ld c, a

    ;gravitational accceceelelleration
    inc bc
    ;and store new grav
    ld a, b
    ld [$C008], a
    ld a, c
    ld [$C009], a

    ;set de to dino y-velocity
    ld a, [$C006]
    ld d, a
    ld a, [$C007]
    ld e, a

    ;add grav to y-velocty (de still set to dino y)
    call AddTwo16BitNumbers  ;returns bc

    ;store new y-velocity
    ld a, b
    ld [$C006], a
    ld a, c
    ld [$C007], a

;next check if dino below ground, if so move him up and rest vel, and grav accel
.checkDinoGround
    ld a, [$D000]  ;dino y
    cp a, 104
    jr nc, .setDinoTo100  ;if dino less than or equal to 104, no need to setDinoTo100
    jp Dropper

.setDinoTo100
    ;reset velocity
    xor a
    ld [$C006], a  ;dino y velocity Fixed Point
    ld [$C007], a
    ;reset grav accel
    ld [$C008], a
    ld [$C009], a

    ;reset dino y fixed point
    ld a, 104
    ld hl, $C002
    call Load8BitIntToFixedPointInMem

    ld b, 104
    ld c, 20
    call DinoMove
    jp Dropper

.ballDropped

SECTION "GameTiles", ROM0

GameTiles:
INCBIN "dino.2bpp"
INCBIN "dino24x24Running/run1.2bpp"
INCBIN "dino24x24Running/run2.2bpp"
INCBIN "cactus.2bpp"
GameTilesEnd:
