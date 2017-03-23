; Blackjack for the Atari 2600
;
; Written by Bob Whitehead in 1977 as one of the original launch titles.


; Disassembly of Blackjack.bin
; Disassembled Sun Mar 12 17:28:02 2017
; Using DiStella v3.01a
;
; Command Line: distella-301a\binaries\windows\DiStella.EXE -paf Blackjack.bin


; Tell DASM which processor is used. Technically the Atari 2600 uses a variant
; of the 6502 named the 6507 but the instruction set is the same.

       processor 6502

; Include a set of constants of important memory locations in the Atari 2600
; hardware.

       include vcs.h

; Tell the assembler to build the instructions starting at memory location
; $F000.  This is the preferred convention, though the upper three bits of the
; address are not used.  Having only 13 address lines was a cost cutting measure
; for the 6507, and limits memory size to 2^13 = 8192 bytes.  Cartridge size
; (without using bank switching) is limited to 4k.

       ORG $F000

START:

; Initialize the hardware.  Don't assume anything about the state of the machine
; when powered up. The following code will make sure everything is set to
; standard values.

; Set Interrupts Disabled.  I'm not sure this is strictly necessary since
; removing the interrupt function is another cost cutting feature of the 6507.

       SEI

; Clear Decimal Arithmetic flag. This means the math functions will assume
; binary arithmentic and not decimal arithmetic.

       CLD

; Initialize stack pointer.

       LDX    #$FF
       TXS

; Clear first 256 bytes of memory.
; Simple Atari 2600 memory map:
;   0000 - 002C -- TIA (write)
;   0030 - 003D -- TIA (read)
;   0080 - 00FF -- RIOT (128 bytes of RAM)
;   0280 - 0297 -- RIOT (I/O, Timer)
;   F000 - FFFF -- Cartridge (4K ROM)

       INX
       TXA
CLEARMEM:
       STA    VSYNC,X
       INX
       BNE    CLEARMEM

; Initialize Audio Volume and Frequency

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


       JSR    LF3DD
       JSR    LF2E9

;
       LDA    #$81
       STA    PF2
       LDA    #$01
       LDX    #$06
       STA    PF1
       STA    CTRLPF
       STX    NUSIZ0
       STX    NUSIZ1

; Pause CPU until start of next scan line on TV screen is reached.  WSYNC is
; a "strobe" register which means writing any value to it will trigger it.

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
; following a WSYNC and for the first 22 CPU cycles) then the position is; set
; to pixel 3.

; Once the beam is out of HBlank the pixel position is given by the formula:
; (CPU Cycle - 21) * 3.

; Since the CPU spends 34 cycles between the STA WSYNC and the end of the
; STA RESP1 instructions, the beam is currently at the (34 - 21) * 3 = 39th
; pixel.

       STA    RESP1

; Same calculation as above, but starting from CPU Cycle 34 and adding 3 sets
; the P0 sprite at 48 pixels.

       STA    RESP0

; Now fine tune player sprite positions.  Because of the complexity explained
; above, it's hard (or impossible) to get the timing just right, so use the
; Horizontal Move TIA registers to fine tune the positions.

; Move P1 three pixels left.

       LDA    #$30
       STA    HMP1

; Move P0 four pixels left.

       LDA    #$40
       STA    HMP0

; Block CPU until next CRT line

       STA    WSYNC

; Cause the HMoves to happen. Now P1 is positioned at 36 pixels and P0 is at 44
; pixels.

       STA    HMOVE

LF052: LDA    #$02
       LDY    #$82
       STA    WSYNC
       STY    VSYNC
       STY    VBLANK
       STA    WSYNC
       STA    WSYNC
       LDY    #$00
       STA    WSYNC
       STY    VSYNC
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
       BEQ    LF0DA
       LDA    $A3
       BNE    LF0DA
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
       BCS    LF0DA
       LDA    $EA,X
       AND    LF7E4,Y
       BEQ    LF0DA
       EOR    $EA,X
       STA    $EA,X
       DEC    $9B
LF0DA: LDA    INTIM
       BNE    LF0DA
       STA    VBLANK
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
       ASL
       ASL
       ADC    $B2
       ADC    $B0
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
LF214: SED
LF215: STA    WSYNC
       LDA    ($B4),Y
       STA    GRP0
       LDA    $BA
       STA    COLUP0
       LDX    $AF
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
       LDY    $86,X
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
       BRK
       BRK
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
       SED
       ADC    $8F,X
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
       SED
       CLC
       LDA    $8F,X
       ADC    $8F,X
       STA    $8F,X
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
       .byte $20,$24,$28,$2C,$01,$02,$03,$04,$05,$00,$01,$02,$03,$00,$22,$22
       .byte $22,$3E,$22,$22,$1C,$00,$3E,$20,$20,$3E,$02,$02,$3E,$00,$3E,$02
       .byte $02,$0E,$02,$02,$3E,$00,$04,$04,$04,$3E,$24,$24,$20,$00,$3E,$02
       .byte $02,$3E,$20,$20,$3E,$00,$3E,$22,$22,$3E,$20,$20,$3E,$00,$02,$02
       .byte $02,$02,$02,$02,$3E,$00,$3E,$22,$22,$3E,$22,$22,$3E,$00,$3E,$02
       .byte $02,$3E,$22,$22,$3E,$00,$2E,$2A,$2A,$2A,$2A,$2A,$2E,$00,$3C,$24
       .byte $04,$04,$04,$04,$0E,$00,$02,$3C,$2C,$24,$24,$24,$3C,$00,$22,$24
       .byte $28,$30,$28,$24,$22
LF6E8: .byte $00,$60,$10,$21,$FF,$3F,$7F,$3F,$C7,$D5,$D5,$F8,$FF,$FF,$FF,$FF
       .byte $FF,$FF,$FF,$FF,$FF
LF6FD: .byte $80,$40,$08,$70,$50,$50,$50,$70,$20,$20,$20,$20,$20,$70,$40,$70
       .byte $10,$70,$70,$10,$30,$10,$70,$10,$10,$70,$50,$40,$70,$10,$70,$40
       .byte $70,$70,$50,$70,$40,$70,$10,$10,$10,$10,$70,$70,$50,$70,$50,$70
       .byte $70,$10,$70,$50,$70,$17,$15,$15,$15,$17,$00,$00,$00,$00,$00,$94
       .byte $94,$F4,$94,$95,$E4,$24,$E4,$84,$EE,$EE,$AA,$AE,$AA,$EE,$07,$05
       .byte $07,$05,$07,$77,$55,$75,$55,$75,$6D,$55,$45,$45,$45,$77,$45,$45
       .byte $45,$47,$25,$25,$25,$25,$75,$20,$00,$30,$10,$70,$4A,$4A,$4A,$C9
       .byte $0A,$90,$02,$A9,$09,$C9,$00,$D0,$04,$A0,$0A,$94,$DD,$38,$75,$D9
       .byte $95,$D9,$40,$07,$05,$05,$05,$07,$02,$02,$02,$02,$02,$07,$04,$07
       .byte $01,$07,$07,$01,$03,$01,$07,$01,$01,$07,$05,$04,$07,$01,$07,$04
       .byte $07,$07,$05,$07,$04,$07,$01,$01,$01,$01,$07,$07,$05,$07,$05,$07
       .byte $07,$01,$07,$05
LF7B1: .byte $07,$3E,$31,$F0,$FE,$51,$00,$00,$00,$00,$00,$80,$80,$80,$80,$C0
       .byte $A4,$A4,$E4,$AE,$EA,$EE,$88,$8C,$88,$8E,$70,$50,$10,$10,$38,$72
       .byte $12,$72,$42,$77,$44,$4C,$54,$64,$44,$77,$14,$76,$44,$77,$C0,$00
       .byte $80,$00,$C0
LF7E4: .byte $80,$40,$20,$10,$08,$04,$02,$01
LF7EC: .byte $08,$0E,$00,$06,$36,$0C,$D0
LF7F3: .byte $D6,$50,$77,$FF,$1E,$50,$02,$02,$02,$00,$F0,$69,$F7
