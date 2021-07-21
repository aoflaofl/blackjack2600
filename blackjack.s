; Blackjack for the Atari 2600
;
; Written by Bob Whitehead in 1977 as one of the original Atari 2600 launch
; titles.

; The code is unaltered.  I am adding comments to explain the ins and outs of
; programming the Atari 2600.

; Disassembly Information
;------------------------
; Disassembly of Blackjack.bin
; Disassembled Thu Jul 15 18:45:51 2021
; Using DiStella v3.01a
;
; Command Line: distella-301a\binaries\windows\DiStella.EXE -pabf -cblackjack.cfg Blackjack.bin
;
; blackjack.cfg contents:
;
;      GFX f680 f768
;      GFX f780 f7b1
;      GFX f7b7 f7de

; Assembly Information
; --------------------
; To assemble using DASM: dasm blackjack.s -f3 -oBlackjack.bin

; sha256sum.exe "Blackjack.bin"
; 8333eb40ab694f123f4d3335df8fdde4ea0aa1cf2b35ad89a1aa526ec1ae5163 *Blackjack.bin

;
; RAM Map:
; $86 = P1's 1st 2 digits (in BCD) of chip count
; $89 = P1's 2nd 2 digits (in BCD) of chip count
; $8f = P1's chips bet (in BCD)
; $AF = Number of players?  Or current player being processed.
; $B2,$B3 = Pointer to center display graphics
; $BA,$BB = Pointer to tens part of center display
; $BC,$BD = ones digit of center player.
; $BE,$BF = Pointer to left player's chip display
; $C2 = Holds the next value of the bet
; $C4,$C5 = left part of center column
; $C6,$C7 = Pointer to left player's chips hundreds and question mark

; Tell DASM which processor is used.  The Atari 2600 uses a less functional (and
; less expensive) variant of the 6502 named the 6507, but the instruction set
; is the same.

      processor 6502

; Include a set of constants of important memory locations in the Atari 2600
; hardware.

       include vcs.h

; Tell the assembler to build the instructions starting at memory location
; $F000.  This is the preferred convention, though the upper three bits of the
; address are not used.  Having only 13 address lines was a cost cutting measure
; for the 6507 and limits memory size to 2^13 = 8192 bytes.  Cartridge size
; (without using bank switching) is further limited to 4k.  This cartridge only
; uses 2k.

       ORG $F000

START:

; Initialize the hardware.  A best practice is to assume nothing about the
; state of the machine when powered up. The following code will make sure
; everything is set to standard values.

; Set Interrupts Disabled.  TODO: I'm not sure this is strictly necessary since
; removing the interrupt function is another cost cutting feature of the 6507.

       SEI

; Clear Decimal Arithmetic flag. This means the math commands will assume
; binary arithmetic and not decimal arithmetic.  The program switches to
; Binary Coded Decimal mode at various times.

       CLD

; Simple Atari 2600 memory map:
;   TIA - Telivision Interface Adapter Chip
;     0000 - 002C -- TIA (write)
;     0030 - 003D -- TIA (read)
;   RIOT - RAM, IO, Timers Chip
;     0080 - 00FF -- RIOT (128 bytes of RAM)
;     0280 - 0297 -- RIOT (I/O, Timer)
;   Cartridge
;     F000 - FFFF -- 4K ROM

; Initialize stack pointer.  The Atari 2600 has only 128 bytes of ram, from $80
; to $FF.  The stack pointer is set to $FF.  This means the programmer needs to
; be very careful about using stack space so it doesn't overwrite stored values.

       LDX    #$FF
       TXS

; Write Zeros into the first 256 bytes of memory.  This clears the TIA
; (Television Interface Adapter) chip and the RAM part of the RIOT (RAM, I/O,
; Timer) chip.

       INX
       TXA
CLEARMEM:
       STA    VSYNC,X
       INX
       BNE    CLEARMEM

; Initialize Audio Volume and Frequency of tone generator 1.  $0F (=15) is the
; maximum volume, since the volume registers are 4 bits.
; That value would be in the middle of the frequency range, which is 5 bits.

       LDX    #$0F
       STX    AUDV1
       STX    AUDF1

; Initialize RAM.  I don't know what these memory addresses are for yet
; so I'll update them when I find out.

       INX
RAMINIT:
       LDA    LF7F3,X
       STA    $D0,X
       LDY    #$F6
       STY    $86,X
       STY    $B1,X
       INY
       STY    $B7,X
       DEX
       BPL    RAMINIT

; At this point X = $FF, Y= $F7

; Set volume of voice 0 to 7.

       STY    AUDV0

; Init RAM and do RNG calculations?

       JSR    LF3DD
       JSR    LF2E9

; Initialize the screen graphics.

; Init Playfield graphics
;
;  PF0   PF1       PF1
; 4567 76543210 01234567
; 0000 00000001 10000001

       LDA    #$81
       STA    PF2
       LDA    #$01
       LDX    #$06 ; BUG: Why is this here?
       STA    PF1

; Putting a $01 in CTRLPF sets it to reflect the
; playfield.

       STA    CTRLPF

; The sprites don't move during the game so they will be put in position once
; and stay there.

; Set Player graphics to show 3 copies

       STX    NUSIZ0
       STX    NUSIZ1

; Pause CPU until the electron beam reaches the start of the next scan line on
; the TV screen.  WSYNC is a "strobe" register which means writing any value to
; it (including 0) will trigger it.

       STA    WSYNC

; Coarse position the Player graphics

; The following seemingly useless instructions run for a total of 31 CPU
; cycles.  This is being used for the coarse positioning of the P0 and P1
; sprites.

       DEX           ; Takes 2 CPU cycles
LF03F: DEX           ; Takes 2 CPU cycles
       BPL    LF03F  ; Takes 3 CPU cycles if branch is taken, 2 if not

; As the above instructions run the cathode ray beam traces from left to right
; across the screen, three pixel positions per CPU cycle.

; A STore Accumulator instruction takes 3 cycles so at the end of the following
; instruction the CPU cycle count for this screen line will be 31 + 3 = 34.

; Storing any value ("strobing") to RESP0 and RESP1 causes Player 0 and Player 1
; respectively to reset their position to the current pixel position of the
; beam.  If this happens during HBlank (which the beam is in immediately
; following a WSYNC and for the first 22 CPU cycles) then the position is set
; to pixel 3.

; Once the beam is out of HBlank the pixel position is given by the formula:
; (CPU Cycle - 21) * 3.

; Since the CPU spends 34 cycles between the STA WSYNC and the end of the
; STA RESP1 instruction the beam is currently at the (34 - 21) * 3 = 39th
; pixel and that is where the P1 sprite will be set.

       STA    RESP1

; Same calculation as above, but starting from CPU Cycle 34 and adding 3 sets
; the P0 sprite at (37 - 21) * 3 = 48 pixels from left border.

       STA    RESP0

; Fine tune Player sprite positions

; Because of the coarsness explained above it's hard (or impossible) to get the
; position just right, so use the Horizontal Move TIA registers to fine tune the
; positions.

; The Horizontal Move registers use the 4 high bits of the value put in
; them and read it in twos complement form.  Positive values move the position
; left, negative values move right.  These moves will not take effect until
; the HMOVE register is strobed.

; Move P1 three pixels left.

       LDA    #$30
       STA    HMP1

; Move P0 four pixels left.

       LDA    #$40
       STA    HMP0

; Block CPU until next scan line.

       STA    WSYNC

; Cause the HMoves to happen. Now P1 is positioned at 39-3=36 pixels and P0 is at 48-4=44
; pixels.  Strobing HMOVE should normally take place immediately after strobing
; WSYNC.

       STA    HMOVE

; Begin the screen drawing kernel

; First tell the TV to do a Vertical Sync (VSYNC) by writing a 1 into bit D1
; of the VSYNC register and then doing 3 WSYNCs before turning that bit off.
; VSYNC  is not a strobe register, it's under the program's control.

; Start Frame

LF052: LDA    #$02
       LDY    #$82
; WSYNC is a strobe register
       STA    WSYNC
       STY    VSYNC

; Now the 2600 is in VBLANK which needs to last for 37 scan lines.  The usual
; way this is done is to set a timer to
; count down the time.

       STY    VBLANK
       STA    WSYNC  ; First WSYNC
       STA    WSYNC  ; Second WSYNC
       LDY    #$00
       STA    WSYNC  ; Third WSYNC

; After 3 WSYNCs turn off VSYNC by writing 0 into VSYNC

       STY    VSYNC

; Store $28 (decimal 40) into the TIM64T register, which counts down the INTIM register once each
; 64 CPU cycles.  TODO: The math to show how many cycles/scan lines are used up.  The program will
; continue running.  Later the INTIM register will be checked until it is 0 which means the timer is
; done.

; Start Frame

       LDX    #$28
       STX    TIM64T

       JSR    LF27D
       INC    $A7
       LDA    $A7
       BNE    LF07C
       DEC    $F2
       BNE    LF07C
       LDX    #$FF
       STX    $F3
LF07C: AND    #$03
       STA    $AF
       BNE    LF08E
; Read the Paddle buttons
       LDA    SWCHA
       TAY
       EOR    $E5
       AND    $E5
       STY    $E5
       STA    $E6
LF08E: DEC    $A3
       BPL    LF09A
       LDA    $E4
       ORA    #$02
       STA    $E4
       INC    $A3
LF09A: LDA    $E8
       ASL
       EOR    $E8
       ASL
       ASL
       ROL    $E9
       ROL    $E8
       LDX    $9B
       BEQ    WaitForTimer
       LDA    $A3
       BNE    WaitForTimer
       CLC
       ADC    #$03
       STA    $A3
       LDA    $E9
       AND    #$07
       TAY
       STA    $AB
       LDA    $E8
       EOR    #$07
       AND    #$07
       TAX
       ASL
       ASL
       ASL
       ORA    $AB
       AND    #$FD
       STA    $AB
       CMP    #$34
       BCS    WaitForTimer
       LDA    $EA,X
       AND    LF7E4,Y
       BEQ    WaitForTimer
       EOR    $EA,X
       STA    $EA,X
       DEC    $9B

; Wait for timer to count down to 0

WaitForTimer:
       LDA    INTIM
       BNE    WaitForTimer

; Then store 0 in VBLANK (because A will have 0 in it when the loop completes) to tell the TV to start displaying again.

       STA    VBLANK

; Now at scan line 33.

       LDY    #$80
       STY    $C8
       LDA    #$01
       JSR    LF1E4
       LDA    $CB
       TAX
       JSR    LF13B
       LDX    $CC
       JSR    LF13F
       LDA    $CC
       TAX
       LDY    #$37
       JSR    LF13B
       LDA    #$02
       STA    $C2
       LDA    #$05
       JSR    LF1E4
       STA    $C2
       LDA    $E6
       BEQ    LF110
       STY    $F2
       STY    $F3
LF110: DEY
       LDA    SWCHB
       AND    #$08
       LSR
       BNE    LF11B
       LDY    #$0F
LF11B: STY    $B0
       ORA    #$03
       TAY
       LDX    #$03
LF122: LDA    $F2
       AND    $F3
       AND    $B0
       EOR    LF7EC,Y
       STA    $CA,X
       DEY
       DEX
       BPL    LF122
       LDX    #$16
LF133: STA    WSYNC
       DEX
       BPL    LF133
       JMP    LF052
LF13B: STY    $B0
       STA    COLUP1
LF13F: STX    COLUP0
       LDX    $C8
       LDY    #$10
LF145: LDA    VSYNC,X
       STA    $B2
; multiply by 5
       ASL
       ASL
       ADC    $B2
; Add $B0
       ADC    $B0
; multiply by 2
       ASL
       CMP    #$60
       ROR
       EOR    #$80
       STA.wy $00B6,Y
       INX
       DEY
       DEY
       CPY    #$0C
       BCS    LF145
LF15F: LDA    VSYNC,X
       AND    #$0F
       STA    $B2
       ASL
       ASL
       ADC    $B2
       ADC    $B0
       ORA    #$80
       STA.wy $00B6,Y
       DEY
       DEY
       LDA    VSYNC,X
       AND    #$F0
       LSR
       LSR
       STA    $B2
       LSR
       LSR
       ADC    $B2
       ADC    $B0
       STA.wy $00B6,Y
       INX
       DEY
       DEY
       BPL    LF15F
       STX    $C8

; Start displaying chips

       LDY    #$04
       LDA    ($B8),Y
       ORA    ($B6),Y
       PHA
       LDA    ($C6),Y
       STA    WSYNC
       BCC    LF19E
LF197: LDA    ($B8),Y
       ORA    ($B6),Y
       PHA
       LDA    ($C6),Y
LF19E: STA    GRP1

; TODO: Has something to do with the count up/down for wins/losses.
; Load the digit data.
       LDA    ($C0),Y
       ORA    ($BE),Y
       STA    GRP0
       LDA    ($C4),Y
       TAX
       LDA    ($BC),Y
       ORA    ($BA),Y
       STX    GRP1
       STA    GRP0
       LDA    ($C2),Y
       STA    GRP1
       PLA
       STA    GRP0
       LDA    $B6
       LDA    ($B8),Y
       ORA    ($B6),Y
       PHA
       LDA    ($C6),Y
       STA    GRP1
       LDA    ($C0),Y
       ORA    ($BE),Y
       STA    GRP0
       LDA    ($C4),Y
       TAX
       LDA    ($BC),Y
       ORA    ($BA),Y
       STX    GRP1
       STA    GRP0
       LDA    ($C2),Y
       STA    GRP1
       PLA
       STA    GRP0
       DEY
       BPL    LF197
       INY
       STY    GRP0
       STY    GRP1
       RTS

LF1E4: STA    $C6
LF1E6: STA    WSYNC
; Scan line 34
       LDA    $CD
       STA    COLUP0
       LDA    #$FF
       STA    GRP0
       LDA    $CB
       STA    COLUPF
       LDX    #$04
LF1F6: LDY    #$00
       LDA    ($C8),Y
       AND    #$FC
       STA    $B0,X
       EOR    ($C8),Y
       TAY
       LDA.wy $00CA,Y
       STA    $B6,X
       DEX
       INC    $C8
       DEX
       BPL    LF1F6
       LDY    #$88
       CPY    $C8
       BCS    LF214
       INC    $C8

;********************************
;* Set Binary Coded Decimal mode.
;********************************

LF214: SED

LF215: STA    WSYNC
; Scan line 37
       LDA    ($B4),Y
       STA    GRP0
       LDA    $BA
       STA    COLUP0
       LDX    $AF

; Read the Paddle
; The way the 2600 handles paddles is to count (the number of scanlines) it takes for the
; paddle to discharge.  When it does the high bit of the INPT register becomes 1.
; The next 3 instructions sets the carry flag if the paddle has not discharged yet.

; X holds the player number.

       LDA    INPT0,X
       EOR    #$FF
       ASL
       LDA    ($B0),Y
       TAX
       LDA    ($B2),Y
       STA    GRP0
       LDA    $B8
       STA    COLUP0
       LDA    $B6
       STA    COLUP0
       STX    GRP0
       DEY
       STA    WSYNC
       LDA    ($B4),Y
       STA    GRP0
       LDA    $BA
       STA    COLUP0

; Use the Carry Bit to add $C9 to $C2 in BCD mode.

       LDA    $C2
       ADC    $C9
       STA    $C2
       LDA    ($B0),Y
       CPY    #$81
       TAX
       LDA    ($B2),Y
       STA    GRP0
       LDA    $B8
       STA    COLUP0
       LDA    $B6
       STA    COLUP0
       STX    GRP0
       BCS    LF215

;*********************
;* Clear Decimal Flag.
;*********************

       CLD
       DEC    $C6
       BMI    LF265
       JMP    LF1E6
LF265: LDY    #$00
       STY    GRP0
       LDA    $CD
       STA    COLUBK
       STA    COLUPF
       LDA    $C2
LF271: LSR
       PHA
       AND    #$08
       CMP    #$02
       PLA
       BCC    LF27C
       SBC    #$03
LF27C: RTS

LF27D: STY    AUDC1
       LDX    $F1
       TXA
       BEQ    LF291
       LDA    $E7
       AND    LF6E8,X
       BNE    LF28E
       INX
       INX
       INX
LF28E: LDA    LF7B1,X
LF291: STA    AUDC0
       LSR
       LSR
       LSR
       CPX    #$03
       BEQ    LF2A0
       CPX    #$06
       BNE    LF2A6
       STX    AUDC0
LF2A0: LDA    #$1F
       AND    $E7
       ADC    #$0C
LF2A6: LSR
       STA    AUDF0
       LDX    $E7
       BEQ    LF2CB
       DEX
       STX    $E7
       BNE    LF2C4
       LDA    $F1
       CMP    #$03
       BNE    LF2C2
       LDA    $E9
       ORA    $E8
       BEQ    LF2CB
       LDA    $D2
       STA    $81
LF2C2: STX    $F1
LF2C4: TXA
       LSR
       BCC    LF27C
       JMP    LF4BD
LF2CB: LDA    $D4
       CMP    #$1E
       BCS    LF2D4
       JMP    LF379
LF2D4: LDA    SWCHB
       TAX
       EOR    $E4
       AND    $E4
       STX    $E4
       CLC
       AND    #$43
       ROR
       ROR
       BCC    LF328
       LDA    #$3F
       STA    $A3
LF2E9: CLC
       LDA    $D5
       ADC    #$20
       CMP    #$E0
       BCC    LF2F4
       LDA    #$10
LF2F4: STA    $D5
       STA    $D1
       LDX    #$02
LF2FA: LDA    $86,X
       CMP    #$0A
       BCS    LF306
       STA    $D6,X
       LDA    $89,X
       STA    $E1,X
LF306: LDA    #$0B
       STA    $86,X
       TAY
       LDA    #$BB
       STA    $89,X
       STA    $8F,X
       LDA    $D5
       AND    LF7E4,X
       BNE    LF322
       LDA    $D6,X
       STA    $86,X
       LDA    $E1,X
       STA    $89,X
       LDY    #$14
LF322: STY    $8C,X
       DEX
       BPL    LF2FA
       RTS

LF328: ASL
       BNE    LF345
       BCC    LF348
       STY    $F2
       STY    $F3
       TAX
       LDA    $D5
       STA    $C4
       LDA    #$02
LF338: ASL    $C4
       BCS    LF340
       STA    $86,X
       STY    $89,X
LF340: INX
       CPX    #$03
       BCC    LF338
LF345: DEY
       STY    $D3
LF348: LDA    $D3
       BPL    LF379
       LDA    #$3C
       STA    $E7
       LDA    #$03
       STA    $F1
       LDA    #$6F
       STA    $81
       LDX    #$01
       LDA    $E9
       ORA    $E8
       BNE    LF367
       BCS    LF365
       STX    $E7
       RTS

LF365: INC    $E8
LF367: STX    $9B
       LDX    #$06
       STX    AUDC1
       LDA    #$FF
LF36F: STA    $EA,X
       DEX
       BPL    LF36F
       LDX    #$21
       STX    $D3
       RTS

LF379: CLC
       LDA    $D1
       CMP    #$F0
       BCS    LF3D5
       LDX    $AF
       LDA    LF7E4,X
       BIT    $D1
       BNE    LF3D5
       TAY
       LDA    LF6FD,X
       AND    $E6
       BEQ    LF3A0
       LDA    #$0B
       STA    $8C,X
       LDA    #$00
       STA    $92,X
       STA    $95,X
       LDA    #$04
       STA    AUDC1
       TYA
LF3A0: ORA    $D1
       STA    $D1
       LSR
       LDA    $C2
       BCS    LF3C1
; Load hundreds digits of chip count
       LDY    $86,X
; If it's 0 then skip to LF3B3
       BNE    LF3B3
       CMP    $89,X
       BCC    LF3B3
       LDA    $89,X
LF3B3: STA    $8F,X
       LDA    $D1
       CMP    #$F0
       BCC    LF3D5
       LDA    #$00
       STA    $D4
       BEQ    LF3D5
LF3C1: LDY    #$01
       CMP    #$01
       BEQ    LF3D1
       INY
       CMP    #$25
       BCS    LF3D1
       LDA    $CE,X
       BNE    LF3D1
       INY
LF3D1: STY    $92,X
       STY    $95,X
LF3D5: LDA    $A3
       BNE    LF426
       LDY    $D4
       BNE    LF40E

;  Put value $77 in various memory locations.  Not sure why yet.

LF3DD: LDY    #$19
       LDA    #$77
LF3E1: LDX    LF662,Y
       STA    $80,X
       DEY
       BNE    LF3E1


       STY    $9F
       LDX    #$02
LF3ED: LDA    $86,X
       BNE    LF3FB
       LDA    $89,X
       JSR    LF271
       SEC
       SBC    $8F,X
       BMI    LF3FC
LF3FB: TYA
LF3FC: STA    $CE,X
       STY    $92,X
       STY    $95,X
       LDA    LF7E4,X
       AND    $D5
       BNE    LF40B
       INC    $9F
LF40B: DEX
       BPL    LF3ED
LF40E: LDY    $D4
       CPY    #$08
       BCS    LF417
       JMP    LF546
LF417: LDA    $DC
       CPY    #$1D
       BCS    LF424
       CPY    #$14
       BCS    LF427
       JMP    LF5C8
LF424: BEQ    LF46F
LF426: RTS



LF427: BNE    LF435
       LDX    $D2
       STX    $81
       DEC    $9F
       BMI    LF43F
       INC    $D4
       BNE    LF451
LF435: CMP    #$16
       BCC    LF445
       LDX    #$00
       STX    $DC
       STX    $E0
LF43F: LDA    #$1D
       STA    $D4
       BNE    LF46C
LF445: JSR    LF641
       LDX    LF663,Y
       STA    $80,X
       LDX    #$03
       BRK
       BRK
LF451: CMP    #$11
       BCS    LF464
       LDX    #$30
       CLC
       ADC    $E0
       CMP    #$11
       BCC    LF46C
       BNE    LF464
       LDY    $E4
       BMI    LF46C
LF464: CMP    #$16
       BCS    LF46C
       STA    $DC
       BNE    LF43F
LF46C: STX    $A3
       RTS

LF46F: LDX    #$02
LF471: LDA    LF7E4,X
       AND    $D5
       BNE    LF4A0
       LDA    $92,X
       CMP    #$04
       BCS    LF4A0
       LDA    $D9,X
       CMP    #$0C
       BCS    LF486
       ADC    $DD,X
LF486: STA    $C2
       LDA    $DC
       LDY    #$08
       CMP    $C2
       BNE    LF498
       STY    $92,X
       STY    $95,X
       LDA    $E4
       BMI    LF4A0
LF498: DEY
       BCS    LF49C
       DEY
LF49C: TYA
       JSR    LF651
LF4A0: DEX
       BPL    LF471
       LDA    $D5
       STA    $D1
       INX
       LDY    #$14
LF4AA: ASL
       BCS    LF4AF
       STY    $8C,X
LF4AF: INX
       CPX    #$03
       BCC    LF4AA
       LDA    $E4
       ORA    #$40
       STA    $E4
       INC    $D4
       RTS

LF4BD: LDX    #$02

;********************************
;* Set Binary Coded Decimal mode.
;********************************

LF4BF: SED
       LDA    $F4,X
       ASL
       LDA    $F7,X
       BEQ    LF51B
       BCS    LF4E3
       SBC    #$00
       STA    $F7,X
       LDA    $89,X
       ADC    #$00
       STA    $89,X

;*********************
;* Clear Decimal Flag.
;*********************

       CLD
       LDA    $86,X
       ADC    #$00
       STA    $86,X
       EOR    #$0A
       BNE    LF519
       LDA    $F7,X
       JMP    LF4F8
LF4E3: SBC    #$01
       STA    $F7,X
       LDA    $89,X
       SBC    #$01
       STA    $89,X

;*********************
;* Clear Decimal Flag.
;*********************

       CLD
       LDA    $86,X
       SBC    #$00
       STA    $86,X
       BNE    LF519
       LDA    $89,X
LF4F8: BNE    LF519
       STA    $E1,X
       LDA    #$02
       STA    $D6,X
       LDA    #$0B
       LDY    #$BB
       BCC    LF50A
       STA    $86,X
       STY    $89,X
LF50A: STY    $8F,X
       STA    $8C,X
       LDA    LF7E4,X
       ORA    $D5
       STA    $D5
       ORA    $D1
       STA    $D1
LF519: LDY    #$02
LF51B: DEX
       BPL    LF4BF
       TYA
       BNE    LF528
       LDX    $F1
       BNE    LF528
       INY
       STY    $E7
LF528: AND    $E7
       ASL
       STA    AUDC1

;*********************
;* Clear Decimal Flag.
;*********************

       CLD
       RTS

LF52F: LDA    LF663,Y
       CMP    #$06
       AND    #$03
       BCC    LF543
       TAX
       LDA    LF7E4,X
       AND    $D5
       BEQ    LF543
       INY
       BNE    LF52F
LF543: STY    $D4
       RTS

LF546: JSR    LF52F
       JSR    LF641
       LDX    #$30
       STX    $A3
       LDX    LF663,Y
       STA    $80,X
       DEX
       BEQ    LF559
       RTS

LF559: STA    $D2
       LDA    #$75
       STA    $81
       TXA
       LDX    #$07
LF562: STA    $D9,X
       DEX
       BPL    LF562
       LDX    #$03
       LDA    $80

; Issue a NMI, which jumps to F769 (The address stored at the FFFA vector)

       BRK
       BRK

; The return point of the NMI

       LDA    $D2
       BRK
       BRK
       CLC
       ADC    $E0
       STA    $B2
       LDX    #$02
LF578: LDA    $98,X
       BRK
       BRK
       LDA    $9C,X
       BRK
       BRK
       LDY    $E4
       BPL    LF58E
       CMP    #$0A
       BCC    LF58C
       CMP    #$0C
       BCC    LF58E
LF58C: STA    $CE,X
LF58E: CLC
       ADC    $DD,X
       CMP    #$15
       BNE    LF5AC
       CMP    $B2
       BEQ    LF5AC
       LDA    $8F,X
       JSR    LF271
       CLC

;********************************
;* Set Binary Coded Decimal mode.
;********************************

       SED
       ADC    $8F,X

;*********************
;* Clear Decimal Flag.
;*********************

       CLD
       STA    $8F,X
       LDA    #$04
       LDY    #$02
       JSR    LF64F
LF5AC: DEX
       BPL    LF578
       LDA    $B2
       CMP    #$15
       BNE    LF5C5
       STA    $DC
       LDA    $D2
       STA    $81
       LDA    #$1D
       STA    $D4
       LDA    #$01
       STA    $F1
       STA    $A3
LF5C5: JMP    LF624
LF5C8: LDA    LF663,Y
       AND    #$03
       TAX
       LDA    $D1
       CMP    #$F0
       BCC    LF640
       STY    $CE,X
       LDA    $92,X
       LDX    LF663,Y
       CMP    #$02
       BEQ    LF61C
       JSR    LF641
       STA    $80,X
       PHA
       TXA
       AND    #$03
       TAX
       PLA
       BRK
       BRK
       CMP    #$16
       LDA    $92,X
       EOR    #$03
       PHP
       BNE    LF5FE

;
; Double the Bet for player X
;

;********************************
;* Set Binary Coded Decimal mode.
;********************************

       SED
       CLC
       LDA    $8F,X
       ADC    $8F,X
       STA    $8F,X

;*********************
;* Clear Decimal Flag.
;*********************

       CLD
LF5FE: LDA    #$05
       LDY    #$01
       PLP
       BCS    LF616
       BEQ    LF619
       LDA    $E4
       BMI    LF624
       LDA    $AC,X
       CMP    #$77
       BEQ    LF624
       LDA    #$06
       LDY    #$00
       CLC
LF616: JSR    LF64F
LF619: LDY    $D4
       DEY
LF61C: TYA
       CLC
       ADC    #$04
       AND    #$FC
       STA    $D4
LF624: LDY    $D4
       CPY    #$14
       BCS    LF637
       JSR    LF52F
       BCC    LF637
       LDA    $92,X
       CMP    #$04
       BEQ    LF61C
       BNE    LF639
LF637: LDX    #$04
LF639: LDA    LF7E4,X
       EOR    #$F9
       STA    $D1
LF640: RTS

LF641: LDA    #$06
       STA    AUDC1
       DEC    $D3
       LDA    $AB
       ASL
       INC    $9B
       INC    $D4
       RTS

LF64F: STY    $F1
LF651: STA    $92,X
       STA    $95,X
       ROR
       STA    $F4,X
       LDA    $8F,X
       STA    $F7,X
       LDA    #$6C
       STA    $E7
       DEC    $9F
LF662: RTS

LF663: .byte $1A,$19,$18,$00,$1E,$1D,$1C,$01,$22,$26,$2A,$2E,$21,$25,$29,$2D
       .byte $20,$24,$28,$2C,$01,$02,$03,$04,$05,$00,$01,$02,$03
       .byte $00 ; |        | $F680
       .byte $22 ; |  X   X | $F681
       .byte $22 ; |  X   X | $F682
       .byte $22 ; |  X   X | $F683
       .byte $3E ; |  XXXXX | $F684
       .byte $22 ; |  X   X | $F685
       .byte $22 ; |  X   X | $F686
       .byte $1C ; |   XXX  | $F687
       .byte $00 ; |        | $F688
       .byte $3E ; |  XXXXX | $F689
       .byte $20 ; |  X     | $F68A
       .byte $20 ; |  X     | $F68B
       .byte $3E ; |  XXXXX | $F68C
       .byte $02 ; |      X | $F68D
       .byte $02 ; |      X | $F68E
       .byte $3E ; |  XXXXX | $F68F
       .byte $00 ; |        | $F690
       .byte $3E ; |  XXXXX | $F691
       .byte $02 ; |      X | $F692
       .byte $02 ; |      X | $F693
       .byte $0E ; |    XXX | $F694
       .byte $02 ; |      X | $F695
       .byte $02 ; |      X | $F696
       .byte $3E ; |  XXXXX | $F697
       .byte $00 ; |        | $F698
       .byte $04 ; |     X  | $F699
       .byte $04 ; |     X  | $F69A
       .byte $04 ; |     X  | $F69B
       .byte $3E ; |  XXXXX | $F69C
       .byte $24 ; |  X  X  | $F69D
       .byte $24 ; |  X  X  | $F69E
       .byte $20 ; |  X     | $F69F
       .byte $00 ; |        | $F6A0
       .byte $3E ; |  XXXXX | $F6A1
       .byte $02 ; |      X | $F6A2
       .byte $02 ; |      X | $F6A3
       .byte $3E ; |  XXXXX | $F6A4
       .byte $20 ; |  X     | $F6A5
       .byte $20 ; |  X     | $F6A6
       .byte $3E ; |  XXXXX | $F6A7
       .byte $00 ; |        | $F6A8
       .byte $3E ; |  XXXXX | $F6A9
       .byte $22 ; |  X   X | $F6AA
       .byte $22 ; |  X   X | $F6AB
       .byte $3E ; |  XXXXX | $F6AC
       .byte $20 ; |  X     | $F6AD
       .byte $20 ; |  X     | $F6AE
       .byte $3E ; |  XXXXX | $F6AF
       .byte $00 ; |        | $F6B0
       .byte $02 ; |      X | $F6B1
       .byte $02 ; |      X | $F6B2
       .byte $02 ; |      X | $F6B3
       .byte $02 ; |      X | $F6B4
       .byte $02 ; |      X | $F6B5
       .byte $02 ; |      X | $F6B6
       .byte $3E ; |  XXXXX | $F6B7
       .byte $00 ; |        | $F6B8
       .byte $3E ; |  XXXXX | $F6B9
       .byte $22 ; |  X   X | $F6BA
       .byte $22 ; |  X   X | $F6BB
       .byte $3E ; |  XXXXX | $F6BC
       .byte $22 ; |  X   X | $F6BD
       .byte $22 ; |  X   X | $F6BE
       .byte $3E ; |  XXXXX | $F6BF
       .byte $00 ; |        | $F6C0
       .byte $3E ; |  XXXXX | $F6C1
       .byte $02 ; |      X | $F6C2
       .byte $02 ; |      X | $F6C3
       .byte $3E ; |  XXXXX | $F6C4
       .byte $22 ; |  X   X | $F6C5
       .byte $22 ; |  X   X | $F6C6
       .byte $3E ; |  XXXXX | $F6C7
       .byte $00 ; |        | $F6C8
       .byte $2E ; |  X XXX | $F6C9
       .byte $2A ; |  X X X | $F6CA
       .byte $2A ; |  X X X | $F6CB
       .byte $2A ; |  X X X | $F6CC
       .byte $2A ; |  X X X | $F6CD
       .byte $2A ; |  X X X | $F6CE
       .byte $2E ; |  X XXX | $F6CF
       .byte $00 ; |        | $F6D0
       .byte $3C ; |  XXXX  | $F6D1
       .byte $24 ; |  X  X  | $F6D2
       .byte $04 ; |     X  | $F6D3
       .byte $04 ; |     X  | $F6D4
       .byte $04 ; |     X  | $F6D5
       .byte $04 ; |     X  | $F6D6
       .byte $0E ; |    XXX | $F6D7
       .byte $00 ; |        | $F6D8
       .byte $02 ; |      X | $F6D9
       .byte $3C ; |  XXXX  | $F6DA
       .byte $2C ; |  X XX  | $F6DB
       .byte $24 ; |  X  X  | $F6DC
       .byte $24 ; |  X  X  | $F6DD
       .byte $24 ; |  X  X  | $F6DE
       .byte $3C ; |  XXXX  | $F6DF
       .byte $00 ; |        | $F6E0
       .byte $22 ; |  X   X | $F6E1
       .byte $24 ; |  X  X  | $F6E2
       .byte $28 ; |  X X   | $F6E3
       .byte $30 ; |  XX    | $F6E4
       .byte $28 ; |  X X   | $F6E5
       .byte $24 ; |  X  X  | $F6E6
       .byte $22 ; |  X   X | $F6E7
LF6E8: .byte $00 ; |        | $F6E8
       .byte $60 ; | XX     | $F6E9
       .byte $10 ; |   X    | $F6EA
       .byte $21 ; |  X    X| $F6EB
       .byte $FF ; |XXXXXXXX| $F6EC
       .byte $3F ; |  XXXXXX| $F6ED
       .byte $7F ; | XXXXXXX| $F6EE
       .byte $3F ; |  XXXXXX| $F6EF
       .byte $C7 ; |XX   XXX| $F6F0
       .byte $D5 ; |XX X X X| $F6F1
       .byte $D5 ; |XX X X X| $F6F2
       .byte $F8 ; |XXXXX   | $F6F3
       .byte $FF ; |XXXXXXXX| $F6F4
       .byte $FF ; |XXXXXXXX| $F6F5
       .byte $FF ; |XXXXXXXX| $F6F6
       .byte $FF ; |XXXXXXXX| $F6F7
       .byte $FF ; |XXXXXXXX| $F6F8
       .byte $FF ; |XXXXXXXX| $F6F9
       .byte $FF ; |XXXXXXXX| $F6FA
       .byte $FF ; |XXXXXXXX| $F6FB
       .byte $FF ; |XXXXXXXX| $F6FC
LF6FD: .byte $80 ; |X       | $F6FD
       .byte $40 ; | X      | $F6FE
       .byte $08 ; |    X   | $F6FF
       .byte $70 ; | XXX    | $F700
       .byte $50 ; | X X    | $F701
       .byte $50 ; | X X    | $F702
       .byte $50 ; | X X    | $F703
       .byte $70 ; | XXX    | $F704
       .byte $20 ; |  X     | $F705
       .byte $20 ; |  X     | $F706
       .byte $20 ; |  X     | $F707
       .byte $20 ; |  X     | $F708
       .byte $20 ; |  X     | $F709
       .byte $70 ; | XXX    | $F70A
       .byte $40 ; | X      | $F70B
       .byte $70 ; | XXX    | $F70C
       .byte $10 ; |   X    | $F70D
       .byte $70 ; | XXX    | $F70E
       .byte $70 ; | XXX    | $F70F
       .byte $10 ; |   X    | $F710
       .byte $30 ; |  XX    | $F711
       .byte $10 ; |   X    | $F712
       .byte $70 ; | XXX    | $F713
       .byte $10 ; |   X    | $F714
       .byte $10 ; |   X    | $F715
       .byte $70 ; | XXX    | $F716
       .byte $50 ; | X X    | $F717
       .byte $40 ; | X      | $F718
       .byte $70 ; | XXX    | $F719
       .byte $10 ; |   X    | $F71A
       .byte $70 ; | XXX    | $F71B
       .byte $40 ; | X      | $F71C
       .byte $70 ; | XXX    | $F71D
       .byte $70 ; | XXX    | $F71E
       .byte $50 ; | X X    | $F71F
       .byte $70 ; | XXX    | $F720
       .byte $40 ; | X      | $F721
       .byte $70 ; | XXX    | $F722
       .byte $10 ; |   X    | $F723
       .byte $10 ; |   X    | $F724
       .byte $10 ; |   X    | $F725
       .byte $10 ; |   X    | $F726
       .byte $70 ; | XXX    | $F727
       .byte $70 ; | XXX    | $F728
       .byte $50 ; | X X    | $F729
       .byte $70 ; | XXX    | $F72A
       .byte $50 ; | X X    | $F72B
       .byte $70 ; | XXX    | $F72C
       .byte $70 ; | XXX    | $F72D
       .byte $10 ; |   X    | $F72E
       .byte $70 ; | XXX    | $F72F
       .byte $50 ; | X X    | $F730
       .byte $70 ; | XXX    | $F731
       .byte $17 ; |   X XXX| $F732
       .byte $15 ; |   X X X| $F733
       .byte $15 ; |   X X X| $F734
       .byte $15 ; |   X X X| $F735
       .byte $17 ; |   X XXX| $F736
       .byte $00 ; |        | $F737
       .byte $00 ; |        | $F738
       .byte $00 ; |        | $F739
       .byte $00 ; |        | $F73A
       .byte $00 ; |        | $F73B
HI:    .byte $94 ; |X  X X  | $F73C
       .byte $94 ; |X  X X  | $F73D
       .byte $F4 ; |XXXX X  | $F73E
       .byte $94 ; |X  X X  | $F73F
       .byte $95 ; |X  X X X| $F740
ST:    .byte $E4 ; |XXX  X  | $F741
       .byte $24 ; |  X  X  | $F742
       .byte $E4 ; |XXX  X  | $F743
       .byte $84 ; |X    X  | $F744
       .byte $EE ; |XXX XXX | $F745
DB:    .byte $EE ; |XXX XXX | $F746
       .byte $AA ; |X X X X | $F747
       .byte $AE ; |X X XXX | $F748
       .byte $AA ; |X X X X | $F749
       .byte $EE ; |XXX XXX | $F74A
B:     .byte $07 ; |     XXX| $F74B
       .byte $05 ; |     X X| $F74C
       .byte $07 ; |     XXX| $F74D
       .byte $05 ; |     X X| $F74E
       .byte $07 ; |     XXX| $F74F
BU:    .byte $77 ; | XXX XXX| $F750
       .byte $55 ; | X X X X| $F751
       .byte $75 ; | XXX X X| $F752
       .byte $55 ; | X X X X| $F753
       .byte $75 ; | XXX X X| $F754
WI:    .byte $6D ; | XX XX X| $F755
       .byte $55 ; | X X X X| $F756
       .byte $45 ; | X   X X| $F757
       .byte $45 ; | X   X X| $F758
       .byte $45 ; | X   X X| $F759
LO:    .byte $77 ; | XXX XXX| $F75A
       .byte $45 ; | X   X X| $F75B
       .byte $45 ; | X   X X| $F75C
       .byte $45 ; | X   X X| $F75D
       .byte $47 ; | X   XXX| $F75E
TI:    .byte $25 ; |  X  X X| $F75F
       .byte $25 ; |  X  X X| $F760
       .byte $25 ; |  X  X X| $F761
       .byte $25 ; |  X  X X| $F762
       .byte $75 ; | XXX X X| $F763
       .byte $20 ; |  X     | $F764
       .byte $00 ; |        | $F765
       .byte $30 ; |  XX    | $F766
       .byte $10 ; |   X    | $F767
       .byte $70 ; | XXX    | $F768

BRK_ROUTINE:
LF769: LSR
       LSR
       LSR
       CMP    #$0A
       BCC    LF772
       LDA    #$09
LF772: CMP    #$00
       BNE    LF77A
       LDY    #$0A
       STY    $DD,X
LF77A: SEC
       ADC    $D9,X
       STA    $D9,X
       RTI

       .byte $07 ; |     XXX| $F780
       .byte $05 ; |     X X| $F781
       .byte $05 ; |     X X| $F782
       .byte $05 ; |     X X| $F783
       .byte $07 ; |     XXX| $F784
       .byte $02 ; |      X | $F785
       .byte $02 ; |      X | $F786
       .byte $02 ; |      X | $F787
       .byte $02 ; |      X | $F788
       .byte $02 ; |      X | $F789
       .byte $07 ; |     XXX| $F78A
       .byte $04 ; |     X  | $F78B
       .byte $07 ; |     XXX| $F78C
       .byte $01 ; |       X| $F78D
       .byte $07 ; |     XXX| $F78E
       .byte $07 ; |     XXX| $F78F
       .byte $01 ; |       X| $F790
       .byte $03 ; |      XX| $F791
       .byte $01 ; |       X| $F792
       .byte $07 ; |     XXX| $F793
       .byte $01 ; |       X| $F794
       .byte $01 ; |       X| $F795
       .byte $07 ; |     XXX| $F796
       .byte $05 ; |     X X| $F797
       .byte $04 ; |     X  | $F798
       .byte $07 ; |     XXX| $F799
       .byte $01 ; |       X| $F79A
       .byte $07 ; |     XXX| $F79B
       .byte $04 ; |     X  | $F79C
       .byte $07 ; |     XXX| $F79D
       .byte $07 ; |     XXX| $F79E
       .byte $05 ; |     X X| $F79F
       .byte $07 ; |     XXX| $F7A0
       .byte $04 ; |     X  | $F7A1
       .byte $07 ; |     XXX| $F7A2
       .byte $01 ; |       X| $F7A3
       .byte $01 ; |       X| $F7A4
       .byte $01 ; |       X| $F7A5
       .byte $01 ; |       X| $F7A6
       .byte $07 ; |     XXX| $F7A7
       .byte $07 ; |     XXX| $F7A8
       .byte $05 ; |     X X| $F7A9
       .byte $07 ; |     XXX| $F7AA
       .byte $05 ; |     X X| $F7AB
       .byte $07 ; |     XXX| $F7AC
       .byte $07 ; |     XXX| $F7AD
       .byte $01 ; |       X| $F7AE
       .byte $07 ; |     XXX| $F7AF
       .byte $05 ; |     X X| $F7B0
LF7B1: .byte $07 ; |     XXX| $F7B1

; Audio Data

 LF7B2: .byte $3E,$31,$F0,$FE,$51
       .byte $00 ; |        | $F7B7
       .byte $00 ; |        | $F7B8
       .byte $00 ; |        | $F7B9
       .byte $00 ; |        | $F7BA
       .byte $00 ; |        | $F7BB
T:     .byte $80 ; |X       | $F7BC
       .byte $80 ; |X       | $F7BD
       .byte $80 ; |X       | $F7BE
       .byte $80 ; |X       | $F7BF
       .byte $C0 ; |XX      | $F7C0
AY:    .byte $A4 ; |X X  X  | $F7C1
       .byte $A4 ; |X X  X  | $F7C2
       .byte $E4 ; |XXX  X  | $F7C3
       .byte $AE ; |X X XXX | $F7C4
       .byte $EA ; |XXX X X | $F7C5
LE:    .byte $EE ; |XXX XXX | $F7C6
       .byte $88 ; |X   X   | $F7C7
       .byte $8C ; |X   XX  | $F7C8
       .byte $88 ; |X   X   | $F7C9
       .byte $8E ; |X   XXX | $F7CA
J:     .byte $70 ; | XXX    | $F7CB
       .byte $50 ; | X X    | $F7CC
       .byte $10 ; |   X    | $F7CD
       .byte $10 ; |   X    | $F7CE
       .byte $38 ; |  XXX   | $F7CF
ST:    .byte $72 ; | XXX  X | $F7D0
       .byte $12 ; |   X  X | $F7D1
       .byte $72 ; | XXX  X | $F7D2
       .byte $42 ; | X    X | $F7D3
       .byte $77 ; | XXX XXX| $F7D4
N:     .byte $44 ; | X   X  | $F7D5
       .byte $4C ; | X  XX  | $F7D6
       .byte $54 ; | X X X  | $F7D7
       .byte $64 ; | XX  X  | $F7D8
       .byte $44 ; | X   X  | $F7D9
SE:    .byte $77 ; | XXX XXX| $F7DA
       .byte $14 ; |   X X  | $F7DB
       .byte $76 ; | XXX XX | $F7DC
       .byte $44 ; | X   X  | $F7DD
       .byte $77 ; | XXX XXX| $F7DE
E:     .byte $C0 ; |XX      | $F7DF
       .byte $00 ; |        | $F7E0
       .byte $80 ; |X       | $F7E1
       .byte $00 ; |        | $F7E2
       .byte $C0 ; |xx      | $F7E3
LF7E4: .byte $80,$40,$20,$10,$08,$04,$02,$01
LF7EC: .byte $08,$0E,$00,$06,$36,$0C,$D0
LF7F3: .byte $D6,$50,$77,$FF,$1E,$50,$02,$02,$02,$00,$F0,$69,$F7
