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

    ;copy font tiles to game tile ram
    ld hl, $8000
    ld de, FontTiles
    ld bc, FontTilesEnd - FontTiles
.copyFontTiles
    ld a, [de] ; Grab 1 byte from the source
    ld [hli], a ; Place it at the destination, incrementing hl
    inc de ; Move to next byte
    dec bc ; Decrement count
    ld a, b ; Check if count is 0, since `dec bc` doesn't update flags
    or c
    jr nz, .copyFontTiles


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

    ;RGBDS assembler defines so it will fill out stuff with those values for us
    ;I'll use capital letters for RGBDS defines
    ;DEF DINO_X = $C000
    ;DEF DINO_Y = $C002
    ;DEF DINO_X_VEL = $C004
    ;DEF DINO_Y_VEL = $C006
    ;DEF DINO_GRAV_ACCEL = $C008

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

    ;dino gravity acceration (change starting accel at .setDinoTo100)
    ld a, 0
    ld hl, $C008
    call Load8BitIntToFixedPointInMem

    ;dino anime xor starting variable
    ld a, $8F   ;flip flop between $8F and $98 with xor %00010111
    ld [$C020], a

    ;dino animation delay var
    ld a, 15
    ld [$C021], a

    ;cacti x locations (using just as a bool for cactus out or naw)
    xor a
    ld [$C030], a

    ;place or remove cactus boolean
    ld [$C031], a  ; 2 = check for valid place cactus, 0 = check for valid remove cactus, 1 = delay counter before place cactus
    ld [$C040], a  ;which one (0 or 2) to flip flop to

    ;cactus place counter delay
    ld [$C032], a

    ;cactus random mem look location to get random counter delay amount
    ;clamped to $C000 - $CFFF
    ld a, $C0
    ld [$C033], a
    xor a
    ld [$C034], a

    ;scroll x postiion fixed point
    ld a, 0
    ld hl, $C050
    call Load8BitIntToFixedPointInMem

    ;scroll speed
    xor a
    ld [$C052], a
    ld a, %00010101
    ld [$C053], a
    ;ld hl, $C052
    ;call Load8BitIntToFixedPointInMem

    ;store Binary Coded Decimal Score
    ;4 bytes
    xor a
    ld [$C0A0], a
    ld [$C0A1], a
    ld [$C0A2], a
    ld [$C0A3], a

    ;store check number for night and day transition
    ld a, %00010000 ;BCD for 1 checked in thousandths place in IncScore
    ld [$C0A4], a


  ;D000 is gonna be for storing sprite data for DMA trnsfer to OAM
  ;WRA1 = Work RAm 1 starts at $D000
  ;clear WRAM1 so DMA transfer doesn't copy random data
  ld hl, $D000 ;start of WRA1
.clearWRA1
  xor a
  ld [hli], a
  ld a, l
  cp a, $9f
  jr nz, .clearWRA1

;Clear OAM so no phantom random sprites
   ld hl, $FE00 ;start of OAM
.clearOAM
    xor a
    ld [hli], a
    ld a, l
    cp a, $9f
    jr nz, .clearOAM

  ;Init background and sprite color palletes
  ;ld a, %00111111 ;for night mode invert pallete
  ld a, %11000000
  ld [rBGP], a

  ;ld a, %00000000 ;for night mode invert pallete
  ld a, %11111100
  ld [rOBP0], a

  xor a ; ld a, 0
  ld [rSCY], a
  ld [rSCX], a

;ground setup
;keep changing offscreen ones to add new random ground when scrolling at .update
;load in value from random RAM spot (ram randomized on startup)
;query bits 0-4 for tiles $AC - $B2
ld hl, $99A3  ;starting location for ground
ld de, $C200  ;random RAM start
.groundPlace
ld a, [de]
inc de

and a, %111  ;get num between 0-7

add a, $AC
ld [hli], a  ;place ground in tile map mem

ld a , l
cp a, $C0
jr nz, .groundPlace

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
  jr z, .dinoSetupEnd

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
.dinoSetupEnd

;score setup
ld c, 8   ;uses 8 sprites
ld b, 24   ;y-location
ld d, 144 ;starting x-location
ld hl, $D020
.scoreSetup
  ld a, b
  ld [hli], a
  ld a, d
  ld [hli], a
  sub a, 8
  ld d, a
  inc hl
  inc hl

  dec c
  xor a
  cp a, c
  jr nz, .scoreSetup



.lcdOn
  ; Shut sound down
  ld [rNR52], a

  ; Turn screen on, display background
  ;ld a, (LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON )
  ld a, (LCDCF_ON | LCDCF_BG9800 | LCDCF_BGON | LCDCF_OBJON | LCDCF_BG8000) ;display sprites too
  ld [rLCDC], a

jp GameLoop


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

;increments the player's score stored at $C0A0-$C0A3
IncScore:
  ld b, 0 ;bool for if to inc speed or naw
  ld e, 0 ;for if to flip flop to night or day

  ld hl, $C0A0
  ld a, [hli]
  add a, 1  ;can't use increment because inc doesn't update carry flag
  daa ;turn to BCD representation
  jr nz, .continue
   ;make scroll faster every 100 steps
   ;set b to make scroll faster after finised incrementing score
   ;ld doesn't affect flags so no worry about messing up carries
   ld b, 1

  .continue
  ld [$C0A0], a
  ld a, 0
  adc a, [hl]
  inc hl
  daa ;turn to BCD representation
  ld e, a  ;for night day check every 1000
  ld [$C0A1], a
  ld a, 0
  adc a, [hl]
  inc hl
  daa ;turn to BCD representation
  ld [$C0A2], a
  ld a, 0
  adc a, [hl]
  daa ;turn to BCD representation
  ld [$C0A3], a

  dec b  ;if b == 1 then decrement will set b to 0
  jr nz, .noSpeedUp
  ld hl, $C053
  inc [hl]
  .noSpeedUp

  ld a, e
  and a, %11110000  ;BCD 9 in thousandths place
  ld hl, $C0A4
  cp a, [hl]
  jr nz, .noDayChange
  add a, %00010000
  ld [hl], a
  ld a, [rBGP]
  xor a, %11111111  ;flip between %11000000 (day) and %00111111 (night)
  ld [rBGP], a

  ld a, [rOBP0]
  xor a, %11111100  ;flip between %11111100 (day) and %00000000 (night)
  ld [rOBP0], a
  .noDayChange
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

;=======================================================
;Cactus place routine takes register a as tile x-value
;=======================================================
PlaceRegularCactus:
  ;save copy of x-value
  ld b, a
  ;since catuses only placed in $99-- tile map space, we'll only update l
  ld h, $99

  ;regular cactus
  ;mid
  add a, $21
  ld l, a
  ld a, $9C  ;top
  ld [hl], a

  ld a, b
  add a, $41
  ld l, a
  ld a, $9F
  ld [hl], a

  ld a, b
  add a, $61
  ld l, a
  ld a, $A5  ;connect to stems
  ld [hl], a

  ld a, b
  add a, $81
  ld l, a
  ld a, $9F
  ld [hl], a

  ld a, b
  add a, $A1
  ld l, a
  ld a, $9F
  ld [hl], a

  ;stems
  ;left
  ld a, b
  add a, $40
  ld l, a
  ld a, $9E
  ld [hl], a

  ld a, b
  add a, $60
  ld l, a
  ld a, $A4
  ld [hl], a
  ;right
  ld a, b
  add a, $42
  ld l, a
  ld a, $A0
  ld [hl], a

  ld a, b
  add a, $62
  ld l, a
  ld a, $A6
  ld [hl], a

  ;cactus ground
  ld a, b
  add a, $A0
  ld l, a
  ld a, $AA
  ld [hl], a

  ld a, b
  add a, $A2
  ld l, a
  ld a, $AC
  ld [hl], a

  ret

;==========================================
;RemoveRegularCactus removes cactus's tiles from tile x value in register a
;==========================================
RemoveRegularCactus:
  ;save copy of x-value
  ld b, a
  ;since catuses only placed in $99-- tile map space, we'll only update l
  ld h, $99

  ;regular cactus
  ;mid
  add a, $21
  ld l, a
  xor a  ;top
  ld [hl], a

  ld a, b
  add a, $41
  ld l, a
  xor a
  ld [hl], a

  ld a, b
  add a, $61
  ld l, a
  xor a  ;connect to stems
  ld [hl], a

  ld a, b
  add a, $81
  ld l, a
  xor a
  ld [hl], a

  ld a, b
  add a, $A1
  ld l, a
  xor a
  ld [hl], a

  ;stems
  ;left
  ld a, b
  add a, $40
  ld l, a
  xor a
  ld [hl], a

  ld a, b
  add a, $60
  ld l, a
  xor a
  ld [hl], a
  ;right
  ld a, b
  add a, $42
  ld l, a
  xor a
  ld [hl], a

  ld a, b
  add a, $62
  ld l, a
  xor a
  ld [hl], a

  ;cactus ground
  ld a, b
  add a, $A0
  ld l, a
  ld a, $AD
  ld [hl], a

  ld a, b
  add a, $A1
  ld l, a
  ld a, $B2
  ld [hl], a

  ld a, b
  add a, $A2
  ld l, a
  ld a, $B2
  ld [hl], a

  ret

;=================Draw Score====================
DrawScore:
  ld c, 4  ;how many bytes to read
  ld hl, $D022 ;location in shadow OAM
  ld de, $C0A0 ;score in $C0A0 - $C0A3

  .repeatThroughBytes
  ld a, [de]
  inc e
  ld b, a
  and a, %00001111  ;get lower 4 bits (one digit of 0-9 in BCD)
  add a, $30  ;$30 start of number tiles in tile memory
  ld [hli], a
  inc hl
  inc hl
  inc hl
  ld a, b
  swap a  ;place upper 4 bits in lower 4 bits
  and a, %00001111
  add a, $30
  ld [hli], a
  inc hl
  inc hl
  inc hl
  ld a, b

  dec c
  xor a
  cp a, c
  jr nz, .repeatThroughBytes

  ret

GameLoop:
;================================================
;Dropper Script
;================================================
  ;ld b, 0    ;counter
  ld d, 0
  ld e, 1
Dropper:
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
    ld a, $E2     ;originally $E2
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
.waitVBlankDropper
    ld a, [rLY]
    cp 144 ; Check if the LCD is in VBlank
    jr c, .waitVBlankDropper

.testForPlaceOrRemove
    ld a, [$C031]
    cp a, 5
    jr nz, .testForRemove

    xor a
    call PlaceRegularCactus
    inc a
    ld [$C031], a
    ld [$C030], a  ;cactus out (for hit detection)
    jr .scrollBackground

    .testForRemove
    cp a, 10
    jr nz, .scrollBackground

    xor a
    ld [$C030], a   ;removed cactus, no cactus out (for hit detection)
    call RemoveRegularCactus
    inc a
    ld [$C031], a

.scrollBackground
  ld a, $D0
  call hRunDma
  ;scroll background to scroll cacti and ground
  ;by adding scroll speed to Scroll X ?
  ;for now just inc Scroll X twice
  ;ld a, [$FF43]

  ;load scroll x FP into de
  ld a, [$C050]
  ld d, a
  ld a, [$C051]
  ld e, a

  ;set bc to scroll speed
  ld a, [$C052]
  ld b, a
  ld a, [$C053]
  ld c, a

  ;add scroll velocity to scroll x
  call AddTwo16BitNumbers  ;returns bc

  ;store new  scroll x
  ld a, b
  ld [$C050], a
  ld a, c
  ld [$C051], a

  call ShiftFixedPointToInt  ;returns b

  ;scroll screen
  ld a, b
  ld [$FF43], a

  ;if scx greater than cactus width, delete cactus

  ;place new cactus on random interval
  ;divide Scroll X by 8 (shifts a right without carry)
  ;srl a
  ;srl a
  ;srl a
  ;actually just using the shift right and bit mask
  ;gives 16 different cactus positions which I think is good enough
  ;srl a
  ;and a, %00011101
  ;remove previous cactus
  ;ld a, [$C030]
  ;cp a, 0
  ;call z, RemoveRegularCactus

  ld a, [$C031]  ;check if should place catcus or should remove cactus
  cp a, 0
  jr z, .removeCactus
  cp a, 2
  jr z, .placeCactus  ;delay already done

  .placeCactusDelay
  ld a, [$C032]
  dec a
  ld [$C032], a
  jr nz, .dinoLegAnimation

  ;flip flop between 0 and 2
  ld a, [$C040]
  xor a, %00000010
  ld [$C040], a
  ld [$C031], a
  jr .dinoLegAnimation

  .placeCactus
  ;only place cactus when scrolled out of view
  ld a, [$FF43]
  cp a, 24  ;cactus width = 24 pixels (three tiles)
  jr c, .dinoLegAnimation  ;a < 24
  cp a, 100
  jr nc, .dinoLegAnimation  ;a >= 100

  ;ld a, 1
  ;ld [$C031], a  ;set to remove cactus next, but first do cactus delay so doesn't instantly remove catcus

  ;ld a, [$FF43]
  ;srl a
  ;and a, %00011101
  ;ld [$C030], a  ;load catus x postion for hit detection/deletion
  ld a, 5  ;draw cactus next update loop
  ld [$C031], a
  ;xor a
  ;call PlaceRegularCactus
  jr .dinoLegAnimation

  .removeCactus
  ld a, [$FF43]     ;wait until cactus out of view, then delete
  cp a, 24  ;cactus width = 24 pixels (three tiles)
  jr c, .dinoLegAnimation  ;a < 24
  cp a, 100
  jr nc, .dinoLegAnimation  ;a > 100

  ld a, 10  ;remove cactus from tilemap next update loop
  ld [$C031], a
  ;ld a, [$C030]
  ;xor a
  ;call RemoveRegularCactus

  ;ld a, 1
  ;ld [$C031], a  ;set cactus place boolean to 1, placeCatusDelay

  .resetCactusCounter
  ld a, [$C033]
  ld h, a  ;for loadRandNumIntoDelayCounter
  ld d, a
  ld a, [$C034]
  ld l, a  ;for loadRandNumIntoDelayCounter
  ld e, a
  inc de
  ld a, d
  cp a, $D0  ;clamp random num range between $C000 - $CFFF
  jr z, .clampRandomPick

  ;else store incremented location
  ld [$C033], a
  ld a, e
  ld [$C034], a
  jr .loadRandNumIntoDelayCounter

  .clampRandomPick
  ld a, $C0
  ld [$C033], a
  xor a
  ld [$C034], a

  .loadRandNumIntoDelayCounter
  ld a, [hl]
  ld [$C032], a

.dinoLegAnimation
  ;if dino is on ground
  ld a, [$D000]
  cp a, 104
  jr c, .regDinolegs  ;dino y < 104

    ld a, [$C021]  ;dino animation delay
    dec a
    ld [$C021], a
    cp a, 0
    jr nz, .dinoUpdate  ;dino y < 104

    ld a, 13  ;originally delay = 15
    ld [$C021], a

    ;increment score every dino step
    call IncScore
    call DrawScore

    ;animate legs
    ld a, [$C020]  ;anime variable (starts as $8F)
    xor a, %00010111   ;flip flop between $8F and $98
    ld [$C020], a

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

    ;check if dino smacked into cactus
.checkDinoCactus
  ld a, [$D000]  ;dino y
  cp a, $46
  jr c, .checkDinoGround  ;only check for hitting cactus if dino low enough to hit one

  ld a, [$FF43]  ;scroll x - less than $E2, greater than $0C
  cp a, $EF
  jr c, .checkDinoGround

  ;cp a, $0C
  ;jr nc, .checkDinoGround

  ld a, [$C030]
  cp a, 0  ;0 = no cactus out, 1 = cactus out
  jr z, .checkDinoGround

  ;dino hit cactus!
  jr .hitCactus

    ;check if dino below ground, if so move him up and reset vel, and grav accel
    ;else if dino above do grav
.checkDinoGround
    ld a, [$D000]  ;dino y
    cp a, 104
    jr nc, .setDinoTo100  ;if dino y greater than or equal to 104 setDinoTo100, else do grav

    ;GRAVITTTTY
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

    ;takes bc, returns b int (used to slow down grav accel)
    call ShiftFixedPointToInt
    ;store b in c so correct endianess (b most significant, c least significant)
    ld c, b
    ld b, 0

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

    jp Dropper

.setDinoTo100
    ;reset velocity
    xor a
    ld [$C006], a  ;dino y velocity Fixed Point
    ld [$C007], a
    ;reset grav accel
    ld [$C008], a
    ld a, 4  ;can change starting accel to make dino jump (shorter / longer) shorter / higher
    ld [$C009], a

    ;reset dino y fixed point
    ld a, 104
    ld hl, $C002
    call Load8BitIntToFixedPointInMem

    ld b, 104
    ld c, 20
    call DinoMove
    jp Dropper

;pause game and wait for player to press a to restart
.hitCactus
  .waitVBlankHitCactus
      ld a, [rLY]
      cp 144 ; Check if the LCD is in VBlank
      jr c, .waitVBlankHitCactus

  .dinoSurprised
  ld a, $B5
  ld [$FE06], a   ;change dino head to surprised
  inc a
  ld [$FE0A], a

  ld hl, $9883  ;prints kinda upper middle
  ld de, RestartString
  .copyString
  ld a, [de]
  ld [hli], a
  inc de
  and a ; Check if the byte we just copied is zero
  jr nz, .copyString ; Continue if it's not

  ld hl, $98C2  ;prints middle, underneath above string
  ld de, RestOfString
  .copyString2
  ld a, [de]
  ld [hli], a
  inc de
  and a ; Check if the byte we just copied is zero
  jr nz, .copyString2 ; Continue if it's not

.waitForRestart
  ld a, %00010000   ;looking for action input
  ;ld a, %00100000   ;looking for d-pad button input
  ld [$FF00], a

  ld a, [$FF00]  ;good to check joypad register multiple times
  ld a, [$FF00]
  ld a, [$FF00]

  cp a, %11010111   ;looking for start or down
  ;cp a, %11101101  ;looking for left press
  ;cp a, %11101110   ;looking for right press
  ;cp a, %11101011   ;looking for up press
  ;cp a, %11011110    ;looking for  a
  jr z, .ballDropped  ;if pressing a, restart game
jr .waitForRestart

.ballDropped
  .waitVBlankCLRSCREEN
      ld a, [rLY]
      cp 144 ; Check if the LCD is in VBlank
      jr nz, .waitVBlankCLRSCREEN
  ld hl, $9883
  .clearBG
  xor a
  ld [hli], a
  ld a, l
  cp a, $CF
  jr nz, .clearBG

SECTION "GameTiles", ROM0

GameTiles:
INCBIN "dino.2bpp"
INCBIN "dino24x24Running/run1.2bpp"
INCBIN "dino24x24Running/run2.2bpp"
INCBIN "cactus.2bpp"
INCBIN "groundSprites/ground0.2bpp"
INCBIN "groundSprites/ground1.2bpp"
INCBIN "groundSprites/ground2.2bpp"
INCBIN "groundSprites/ground3.2bpp"
INCBIN "groundSprites/ground4.2bpp"
INCBIN "groundSprites/ground5.2bpp"
INCBIN "groundSprites/ground6.2bpp"
INCBIN "dino24x24Surprised/dinoSurprised.2bpp"
GameTilesEnd:

SECTION "Font", ROM0

FontTiles:
INCBIN "font.chr"
FontTilesEnd:

SECTION "Restart Game String", ROM0
RestartString:
    DB "Press START", 0

RestOfString:
    DB "To Play Again", 0
