; Disassembly of ~\Documents\Atari2600\Work\blackjack2600\Blackjack.bin
; Disassembled 07/15/21 08:10:03
; Using Stella 6.5.3
;
; ROM properties name : Blackjack (1977) (Atari)
; ROM properties MD5  : 0a981c03204ac2b278ba392674682560
; Bankswitch type     : 2K* (2K) 
;
; Legend: *  = CODE not yet run (tentative code)
;         D  = DATA directive (referenced in some way)
;         G  = GFX directive, shown as '#' (stored in player, missile, ball)
;         P  = PGFX directive, shown as '*' (stored in playfield)
;         C  = COL directive, shown as color constants (stored in player color)
;         CP = PCOL directive, shown as color constants (stored in playfield color)
;         CB = BCOL directive, shown as color constants (stored in background color)
;         A  = AUD directive (stored in audio registers)
;         i  = indexed accessed only
;         c  = used by code executed in RAM
;         s  = used by stack
;         !  = page crossed, 1 cycle penalty

    processor 6502


;-----------------------------------------------------------
;      Color constants
;-----------------------------------------------------------

BLACK            = $00
YELLOW           = $10
BROWN            = $20
ORANGE           = $30
RED              = $40
MAUVE            = $50
VIOLET           = $60
PURPLE           = $70
BLUE             = $80
BLUE_CYAN        = $90
CYAN             = $A0
CYAN_GREEN       = $B0
GREEN            = $C0
GREEN_YELLOW     = $D0
GREEN_BEIGE      = $E0
BEIGE            = $F0


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

CXM0P           = $00  ; (R)
INPT0           = $08  ; (R)
;INPT1          = $09  ; (Ri)
;INPT2          = $0A  ; (Ri)
;INPT3          = $0B  ; (Ri)

VSYNC           = $00  ; (W)
VBLANK          = $01  ; (W)
WSYNC           = $02  ; (W)
NUSIZ0          = $04  ; (W)
NUSIZ1          = $05  ; (W)
COLUP0          = $06  ; (W)
COLUP1          = $07  ; (W)
COLUPF          = $08  ; (W)
COLUBK          = $09  ; (W)
CTRLPF          = $0A  ; (W)
PF1             = $0E  ; (W)
PF2             = $0F  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)
AUDC0           = $15  ; (W)
AUDC1           = $16  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)
AUDV0           = $19  ; (W)
AUDV1           = $1A  ; (W)
GRP0            = $1B  ; (W)
GRP1            = $1C  ; (W)
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
HMOVE           = $2A  ; (W)

SWCHA           = $0280
SWCHB           = $0282
INTIM           = $0284
TIM64T          = $0296


;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------

ram_80          = $80
ram_81          = $81
;                 $82  (i)
;                 $83  (i)
;                 $84  (i)
;                 $85  (i)
ram_86          = $86
;                 $87  (i)
;                 $88  (i)
ram_89          = $89
;                 $8A  (i)
;                 $8B  (i)
ram_8C          = $8C
;                 $8D  (i)
;                 $8E  (i)
ram_8F          = $8F
;                 $90  (i)
;                 $91  (i)
ram_92          = $92
;                 $93  (i)
;                 $94  (i)
ram_95          = $95
;                 $96  (i)
;                 $97  (i)
ram_98          = $98
;                 $99  (i)
;                 $9A  (i)
ram_9B          = $9B
ram_9C          = $9C
;                 $9D  (i)
;                 $9E  (i)
ram_9F          = $9F
;                 $A0  (i)
;                 $A1  (i)
;                 $A2  (i)
ram_A3          = $A3
;                 $A4  (i)
;                 $A5  (i)
;                 $A6  (i)
ram_A7          = $A7
;                 $A8  (i)
;                 $A9  (i)
;                 $AA  (i)
ram_AB          = $AB
ram_AC          = $AC
;                 $AD  (i)
;                 $AE  (i)
ram_AF          = $AF
ram_B0          = $B0
ram_B1          = $B1
ram_B2          = $B2
;                 $B3  (i)
ram_B4          = $B4
;                 $B5  (i)
ram_B6          = $B6
ram_B7          = $B7
ram_B8          = $B8
;                 $B9  (i)
ram_BA          = $BA
;                 $BB  (i)
ram_BC          = $BC
;                 $BD  (i)
ram_BE          = $BE
;                 $BF  (i)
ram_C0          = $C0
;                 $C1  (i)
ram_C2          = $C2
;                 $C3  (i)
ram_C4          = $C4
;                 $C5  (i)
ram_C6          = $C6
;                 $C7  (i)
ram_C8          = $C8
ram_C9          = $C9
ram_CA          = $CA
ram_CB          = $CB
ram_CC          = $CC
ram_CD          = $CD
ram_CE          = $CE
;                 $CF  (i)
ram_D0          = $D0
ram_D1          = $D1
ram_D2          = $D2
ram_D3          = $D3
ram_D4          = $D4
ram_D5          = $D5
ram_D6          = $D6
;                 $D7  (i)
;                 $D8  (i)
ram_D9          = $D9
;                 $DA  (i)
;                 $DB  (i)
ram_DC          = $DC
ram_DD          = $DD
;                 $DE  (i)
;                 $DF  (i)
ram_E0          = $E0
ram_E1          = $E1

ram_E4          = $E4
ram_E5          = $E5
ram_E6          = $E6
ram_E7          = $E7
ram_E8          = $E8
ram_E9          = $E9
ram_EA          = $EA
;                 $EB  (i)
;                 $EC  (i)
;                 $ED  (i)
;                 $EE  (i)
;                 $EF  (i)
;                 $F0  (i)
ram_F1          = $F1
ram_F2          = $F2
ram_F3          = $F3
ram_F4          = $F4
;                 $F5  (i)
;                 $F6  (i)
ram_F7          = $F7
;                 $F8  (i)
;                 $F9  (i)

;                 $FB  (s)
;                 $FC  (s)
;                 $FD  (s)
;                 $FE  (s)
;                 $FF  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Start           = $F000
Break           = $F769


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $F000

Start
    sei                             ;2        
    cld                             ;2        
    ldx     #$FF                    ;2        
    txs                             ;2        
    inx                             ;2        
    txa                             ;2   =  12
LF007
    sta     VSYNC,x                 ;4        
    inx                             ;2        
    bne     LF007                   ;2/3      
    ldx     #$0F                    ;2        
    stx     AUDV1                   ;3        
    stx     AUDF1                   ;3        
    inx                             ;2   =  18
LF013
    lda     LF7F3,x                 ;4        
    sta     ram_D0,x                ;4        
    ldy     #$F6                    ;2        
    sty     ram_86,x                ;4        
    sty     ram_B1,x                ;4        
    iny                             ;2        
    sty     ram_B7,x                ;4        
    dex                             ;2        
    bpl     LF013                   ;2/3      
    sty     AUDV0                   ;3        
    jsr     LF3DD                   ;6        
    jsr     LF2E9                   ;6        
    lda     #$81                    ;2        
    sta     PF2                     ;3        
    lda     #$01                    ;2        
    ldx     #$06                    ;2        
    sta     PF1                     ;3        
    sta     CTRLPF                  ;3        
    stx     NUSIZ0                  ;3        
    stx     NUSIZ1                  ;3        
    sta     WSYNC                   ;3   =  67
;---------------------------------------
    dex                             ;2   =   2
LF03F
    dex                             ;2        
    bpl     LF03F                   ;2/3      
    sta     RESP1                   ;3        
    sta     RESP0                   ;3        
    lda     #$30                    ;2        
    sta     HMP1                    ;3        
    lda     #$40                    ;2        
    sta     HMP0                    ;3        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMOVE                   ;3   =   3
LF052
    lda     #$02                    ;2        
    ldy     #$82                    ;2        
    sta     WSYNC                   ;3   =   7
;---------------------------------------
    sty     VSYNC                   ;3        
    sty     VBLANK                  ;3        
    sta     WSYNC                   ;3   =   9
;---------------------------------------
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    ldy     #$00                    ;2        
    sta     WSYNC                   ;3   =   5
;---------------------------------------
    sty     VSYNC                   ;3        
    ldx     #$28                    ;2        
    stx     TIM64T                  ;4        
    jsr     LF27D                   ;6        
    inc     ram_A7                  ;5        
    lda     ram_A7                  ;3        
    bne     LF07C                   ;2/3      
    dec     ram_F2                  ;5        
    bne     LF07C                   ;2/3      
    ldx     #$FF                    ;2         *
    stx     ram_F3                  ;3   =  37 *
LF07C
    and     #$03                    ;2        
    sta     ram_AF                  ;3        
    bne     LF08E                   ;2/3      
    lda     SWCHA                   ;4        
    tay                             ;2        
    eor     ram_E5                  ;3        
    and     ram_E5                  ;3        
    sty     ram_E5                  ;3        
    sta     ram_E6                  ;3   =  25
LF08E
    dec     ram_A3                  ;5        
    bpl     LF09A                   ;2/3      
    lda     ram_E4                  ;3        
    ora     #$02                    ;2        
    sta     ram_E4                  ;3        
    inc     ram_A3                  ;5   =  20
LF09A
    lda     ram_E8                  ;3        
    asl                             ;2        
    eor     ram_E8                  ;3        
    asl                             ;2        
    asl                             ;2        
    rol     ram_E9                  ;5        
    rol     ram_E8                  ;5        
    ldx     ram_9B                  ;3        
    beq     LF0DA                   ;2/3      
    lda     ram_A3                  ;3        
    bne     LF0DA                   ;2/3      
    clc                             ;2        
    adc     #$03                    ;2        
    sta     ram_A3                  ;3        
    lda     ram_E9                  ;3        
    and     #$07                    ;2        
    tay                             ;2        
    sta     ram_AB                  ;3        
    lda     ram_E8                  ;3        
    eor     #$07                    ;2        
    and     #$07                    ;2        
    tax                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    ora     ram_AB                  ;3        
    and     #$FD                    ;2        
    sta     ram_AB                  ;3        
    cmp     #$34                    ;2        
    bcs     LF0DA                   ;2/3      
    lda     ram_EA,x                ;4        
    and     LF7E4,y                 ;4        
    beq     LF0DA                   ;2/3      
    eor     ram_EA,x                ;4        
    sta     ram_EA,x                ;4        
    dec     ram_9B                  ;5   =  99
LF0DA
    lda     INTIM                   ;4        
    bne     LF0DA                   ;2/3      
    sta     VBLANK                  ;3        
    ldy     #$80                    ;2        
    sty     ram_C8                  ;3        
    lda     #$01                    ;2        
    jsr     LF1E4                   ;6        
    lda     ram_CB                  ;3        
    tax                             ;2        
    jsr     LF13B                   ;6        
    ldx     ram_CC                  ;3        
    jsr     LF13F                   ;6        
    lda     ram_CC                  ;3        
    tax                             ;2        
    ldy     #$37                    ;2        
    jsr     LF13B                   ;6        
    lda     #$02                    ;2        
    sta     ram_C2                  ;3        
    lda     #$05                    ;2        
    jsr     LF1E4                   ;6        
    sta     ram_C2                  ;3        
    lda     ram_E6                  ;3        
    beq     LF110                   ;2/3      
    sty     ram_F2                  ;3        
    sty     ram_F3                  ;3   =  82
LF110
    dey                             ;2        
    lda     SWCHB                   ;4        
    and     #$08                    ;2        
    lsr                             ;2        
    bne     LF11B                   ;2/3      
    ldy     #$0F                    ;2   =  14 *
LF11B
    sty     ram_B0                  ;3        
    ora     #$03                    ;2        
    tay                             ;2        
    ldx     #$03                    ;2   =   9
LF122
    lda     ram_F2                  ;3        
    and     ram_F3                  ;3        
    and     ram_B0                  ;3        
    eor     LF7EC,y                 ;4        
    sta     ram_CA,x                ;4        
    dey                             ;2        
    dex                             ;2        
    bpl     LF122                   ;2/3      
    ldx     #$16                    ;2   =  25
LF133
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dex                             ;2        
    bpl     LF133                   ;2/3      
    jmp     LF052                   ;3   =   7
    
LF13B
    sty     ram_B0                  ;3        
    sta     COLUP1                  ;3   =   6
LF13F
    stx     COLUP0                  ;3        
    ldx     ram_C8                  ;3        
    ldy     #$10                    ;2   =   8
LF145
    lda     CXM0P,x                 ;4        
    sta     ram_B2                  ;3        
    asl                             ;2        
    asl                             ;2        
    adc     ram_B2                  ;3        
    adc     ram_B0                  ;3        
    asl                             ;2        
    cmp     #$60                    ;2        
    ror                             ;2        
    eor     #$80                    ;2        
    sta.wy  ram_B6,y                ;5        
    inx                             ;2        
    dey                             ;2        
    dey                             ;2        
    cpy     #$0C                    ;2        
    bcs     LF145                   ;2/3 =  40
LF15F
    lda     CXM0P,x                 ;4        
    and     #$0F                    ;2        
    sta     ram_B2                  ;3        
    asl                             ;2        
    asl                             ;2        
    adc     ram_B2                  ;3        
    adc     ram_B0                  ;3        
    ora     #$80                    ;2        
    sta.wy  ram_B6,y                ;5        
    dey                             ;2        
    dey                             ;2        
    lda     CXM0P,x                 ;4        
    and     #$F0                    ;2        
    lsr                             ;2        
    lsr                             ;2        
    sta     ram_B2                  ;3        
    lsr                             ;2        
    lsr                             ;2        
    adc     ram_B2                  ;3        
    adc     ram_B0                  ;3        
    sta.wy  ram_B6,y                ;5        
    inx                             ;2        
    dey                             ;2        
    dey                             ;2        
    bpl     LF15F                   ;2/3      
    stx     ram_C8                  ;3        
    ldy     #$04                    ;2        
    lda     (ram_B8),y              ;5        
    ora     (ram_B6),y              ;5        
    pha                             ;3        
    lda     (ram_C6),y              ;5        
    sta     WSYNC                   ;3   =  92
;---------------------------------------
    bcc     LF19E                   ;2/3 =   2
LF197
    lda     (ram_B8),y              ;5        
    ora     (ram_B6),y              ;5        
    pha                             ;3        
    lda     (ram_C6),y              ;5   =  18
LF19E
    sta     GRP1                    ;3        
    lda     (ram_C0),y              ;5        
    ora     (ram_BE),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_C4),y              ;5        
    tax                             ;2        
    lda     (ram_BC),y              ;5        
    ora     (ram_BA),y              ;5        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    lda     (ram_C2),y              ;5        
    sta     GRP1                    ;3        
    pla                             ;4        
    sta     GRP0                    ;3        
    lda     ram_B6                  ;3        
    lda     (ram_B8),y              ;5        
    ora     (ram_B6),y              ;5        
    pha                             ;3        
    lda     (ram_C6),y              ;5        
    sta     GRP1                    ;3        
    lda     (ram_C0),y              ;5        
    ora     (ram_BE),y              ;5        
    sta     GRP0                    ;3        
    lda     (ram_C4),y              ;5        
    tax                             ;2        
    lda     (ram_BC),y              ;5        
    ora     (ram_BA),y              ;5        
    stx     GRP1                    ;3        
    sta     GRP0                    ;3        
    lda     (ram_C2),y              ;5        
    sta     GRP1                    ;3        
    pla                             ;4        
    sta     GRP0                    ;3        
    dey                             ;2        
    bpl     LF197                   ;2/3      
    iny                             ;2        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    rts                             ;6   = 147
    
LF1E4
    sta     ram_C6                  ;3   =   3
LF1E6
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_CD                  ;3        
    sta     COLUP0                  ;3        
    lda     #$FF                    ;2        
    sta     GRP0                    ;3        
    lda     ram_CB                  ;3        
    sta     COLUPF                  ;3        
    ldx     #$04                    ;2   =  19
LF1F6
    ldy     #$00                    ;2        
    lda     (ram_C8),y              ;5        
    and     #$FC                    ;2        
    sta     ram_B0,x                ;4        
    eor     (ram_C8),y              ;5        
    tay                             ;2        
    lda.wy  ram_CA,y                ;4        
    sta     ram_B6,x                ;4        
    dex                             ;2        
    inc     ram_C8                  ;5        
    dex                             ;2        
    bpl     LF1F6                   ;2/3!     
    ldy     #$88                    ;2        
    cpy     ram_C8                  ;3        
    bcs     LF214                   ;2/3      
    inc     ram_C8                  ;5   =  51
LF214
    sed                             ;2   =   2
LF215
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     (ram_B4),y              ;5        
    sta     GRP0                    ;3        
    lda     ram_BA                  ;3        
    sta     COLUP0                  ;3        
    ldx     ram_AF                  ;3        
    lda     INPT0|$30,x             ;4        
    eor     #$FF                    ;2        
    asl                             ;2        
    lda     (ram_B0),y              ;5        
    tax                             ;2        
    lda     (ram_B2),y              ;5        
    sta     GRP0                    ;3        
    lda     ram_B8                  ;3        
    sta     COLUP0                  ;3        
    lda     ram_B6                  ;3        
    sta     COLUP0                  ;3        
    stx     GRP0                    ;3        
    dey                             ;2        
    sta     WSYNC                   ;3   =  60
;---------------------------------------
    lda     (ram_B4),y              ;5        
    sta     GRP0                    ;3        
    lda     ram_BA                  ;3        
    sta     COLUP0                  ;3        
    lda     ram_C2                  ;3        
    adc     ram_C9                  ;3        
    sta     ram_C2                  ;3        
    lda     (ram_B0),y              ;5        
    cpy     #$81                    ;2        
    tax                             ;2        
    lda     (ram_B2),y              ;5        
    sta     GRP0                    ;3        
    lda     ram_B8                  ;3        
    sta     COLUP0                  ;3        
    lda     ram_B6                  ;3        
    sta     COLUP0                  ;3        
    stx     GRP0                    ;3        
    bcs     LF215                   ;2/3      
    cld                             ;2        
    dec     ram_C6                  ;5        
    bmi     LF265                   ;2/3      
    jmp     LF1E6                   ;3   =  69
    
LF265
    ldy     #$00                    ;2        
    sty     GRP0                    ;3        
    lda     ram_CD                  ;3        
    sta     COLUBK                  ;3        
    sta     COLUPF                  ;3        
    lda     ram_C2                  ;3   =  17
LF271
    lsr                             ;2        
    pha                             ;3        
    and     #$08                    ;2        
    cmp     #$02                    ;2        
    pla                             ;4        
    bcc     LF27C                   ;2/3      
    sbc     #$03                    ;2   =  17
LF27C
    rts                             ;6   =   6
    
LF27D
    sty     AUDC1                   ;3        
    ldx     ram_F1                  ;3        
    txa                             ;2        
    beq     LF291                   ;2/3      
    lda     ram_E7                  ;3        
    and     LF6E8,x                 ;4        
    bne     LF28E                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    inx                             ;2   =  25
LF28E
    lda     LF7B1,x                 ;4   =   4
LF291
    sta     AUDC0                   ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    cpx     #$03                    ;2        
    beq     LF2A0                   ;2/3      
    cpx     #$06                    ;2        
    bne     LF2A6                   ;2/3      
    stx     AUDC0                   ;3   =  20
LF2A0
    lda     #$1F                    ;2        
    and     ram_E7                  ;3        
    adc     #$0C                    ;2   =   7
LF2A6
    lsr                             ;2        
    sta     AUDF0                   ;3        
    ldx     ram_E7                  ;3        
    beq     LF2CB                   ;2/3      
    dex                             ;2        
    stx     ram_E7                  ;3        
    bne     LF2C4                   ;2/3      
    lda     ram_F1                  ;3        
    cmp     #$03                    ;2        
    bne     LF2C2                   ;2/3      
    lda     ram_E9                  ;3        
    ora     ram_E8                  ;3        
    beq     LF2CB                   ;2/3      
    lda     ram_D2                  ;3        
    sta     ram_81                  ;3   =  38
LF2C2
    stx     ram_F1                  ;3   =   3
LF2C4
    txa                             ;2        
    lsr                             ;2        
    bcc     LF27C                   ;2/3      
    jmp     LF4BD                   ;3   =   9
    
LF2CB
    lda     ram_D4                  ;3        
    cmp     #$1E                    ;2        
    bcs     LF2D4                   ;2/3      
    jmp     LF379                   ;3   =  10
    
LF2D4
    lda     SWCHB                   ;4        
    tax                             ;2        
    eor     ram_E4                  ;3        
    and     ram_E4                  ;3        
    stx     ram_E4                  ;3        
    clc                             ;2        
    and     #$43                    ;2        
    ror                             ;2        
    ror                             ;2        
    bcc     LF328                   ;2/3!     
    lda     #$3F                    ;2         *
    sta     ram_A3                  ;3   =  30 *
LF2E9
    clc                             ;2        
    lda     ram_D5                  ;3        
    adc     #$20                    ;2        
    cmp     #$E0                    ;2        
    bcc     LF2F4                   ;2/3      
    lda     #$10                    ;2   =  13 *
LF2F4
    sta     ram_D5                  ;3        
    sta     ram_D1                  ;3        
    ldx     #$02                    ;2   =   8
LF2FA
    lda     ram_86,x                ;4        
    cmp     #$0A                    ;2        
    bcs     LF306                   ;2/3      
    sta     ram_D6,x                ;4         *
    lda     ram_89,x                ;4         *
    sta     ram_E1,x                ;4   =  20 *
LF306
    lda     #$0B                    ;2        
    sta     ram_86,x                ;4        
    tay                             ;2        
    lda     #$BB                    ;2        
    sta     ram_89,x                ;4        
    sta     ram_8F,x                ;4        
    lda     ram_D5                  ;3        
    and     LF7E4,x                 ;4        
    bne     LF322                   ;2/3      
    lda     ram_D6,x                ;4        
    sta     ram_86,x                ;4        
    lda     ram_E1,x                ;4        
    sta     ram_89,x                ;4        
    ldy     #$14                    ;2   =  45
LF322
    sty     ram_8C,x                ;4        
    dex                             ;2        
    bpl     LF2FA                   ;2/3!     
    rts                             ;6   =  14
    
LF328
    asl                             ;2        
    bne     LF345                   ;2/3      
    bcc     LF348                   ;2/3      
    sty     ram_F2                  ;3        
    sty     ram_F3                  ;3        
    tax                             ;2        
    lda     ram_D5                  ;3        
    sta     ram_C4                  ;3        
    lda     #$02                    ;2   =  22
LF338
    asl     ram_C4                  ;5        
    bcs     LF340                   ;2/3      
    sta     ram_86,x                ;4        
    sty     ram_89,x                ;4   =  15
LF340
    inx                             ;2        
    cpx     #$03                    ;2        
    bcc     LF338                   ;2/3 =   6
LF345
    dey                             ;2        
    sty     ram_D3                  ;3   =   5
LF348
    lda     ram_D3                  ;3        
    bpl     LF379                   ;2/3      
    lda     #$3C                    ;2        
    sta     ram_E7                  ;3        
    lda     #$03                    ;2        
    sta     ram_F1                  ;3        
    lda     #$6F                    ;2        
    sta     ram_81                  ;3        
    ldx     #$01                    ;2        
    lda     ram_E9                  ;3        
    ora     ram_E8                  ;3        
    bne     LF367                   ;2/3      
    bcs     LF365                   ;2/3      
    stx     ram_E7                  ;3        
    rts                             ;6   =  41
    
LF365
    inc     ram_E8                  ;5   =   5
LF367
    stx     ram_9B                  ;3        
    ldx     #$06                    ;2        
    stx     AUDC1                   ;3        
    lda     #$FF                    ;2   =  10
LF36F
    sta     ram_EA,x                ;4        
    dex                             ;2        
    bpl     LF36F                   ;2/3      
    ldx     #$21                    ;2        
    stx     ram_D3                  ;3        
    rts                             ;6   =  19
    
LF379
    clc                             ;2        
    lda     ram_D1                  ;3        
    cmp     #$F0                    ;2        
    bcs     LF3D5                   ;2/3      
    ldx     ram_AF                  ;3        
    lda     LF7E4,x                 ;4        
    bit     ram_D1                  ;3        
    bne     LF3D5                   ;2/3      
    tay                             ;2        
    lda     LF6FD,x                 ;4        
    and     ram_E6                  ;3        
    beq     LF3A0                   ;2/3      
    lda     #$0B                    ;2        
    sta     ram_8C,x                ;4        
    lda     #$00                    ;2        
    sta     ram_92,x                ;4        
    sta     ram_95,x                ;4        
    lda     #$04                    ;2        
    sta     AUDC1                   ;3        
    tya                             ;2   =  55
LF3A0
    ora     ram_D1                  ;3        
    sta     ram_D1                  ;3        
    lsr                             ;2        
    lda     ram_C2                  ;3        
    bcs     LF3C1                   ;2/3      
    ldy     ram_86,x                ;4        
    bne     LF3B3                   ;2/3      
    cmp     ram_89,x                ;4         *
    bcc     LF3B3                   ;2/3       *
    lda     ram_89,x                ;4   =  29 *
LF3B3
    sta     ram_8F,x                ;4        
    lda     ram_D1                  ;3        
    cmp     #$F0                    ;2        
    bcc     LF3D5                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_D4                  ;3        
    beq     LF3D5                   ;2/3 =  18
LF3C1
    ldy     #$01                    ;2        
    cmp     #$01                    ;2        
    beq     LF3D1                   ;2/3      
    iny                             ;2        
    cmp     #$25                    ;2        
    bcs     LF3D1                   ;2/3      
    lda     ram_CE,x                ;4        
    bne     LF3D1                   ;2/3      
    iny                             ;2   =  20
LF3D1
    sty     ram_92,x                ;4        
    sty     ram_95,x                ;4   =   8
LF3D5
    lda     ram_A3                  ;3        
    bne     LF426                   ;2/3!     
    ldy     ram_D4                  ;3        
    bne     LF40E                   ;2/3!=  10
LF3DD
    ldy     #$19                    ;2        
    lda     #$77                    ;2   =   4
LF3E1
    ldx     LF662,y                 ;4        
    sta     ram_80,x                ;4        
    dey                             ;2        
    bne     LF3E1                   ;2/3      
    sty     ram_9F                  ;3        
    ldx     #$02                    ;2   =  17
LF3ED
    lda     ram_86,x                ;4        
    bne     LF3FB                   ;2/3      
    lda     ram_89,x                ;4         *
    jsr     LF271                   ;6         *
    sec                             ;2         *
    sbc     ram_8F,x                ;4         *
    bmi     LF3FC                   ;2/3 =  24 *
LF3FB
    tya                             ;2   =   2
LF3FC
    sta     ram_CE,x                ;4        
    sty     ram_92,x                ;4        
    sty     ram_95,x                ;4        
    lda     LF7E4,x                 ;4        
    and     ram_D5                  ;3        
    bne     LF40B                   ;2/3      
    inc     ram_9F                  ;5   =  26
LF40B
    dex                             ;2        
    bpl     LF3ED                   ;2/3!=   4
LF40E
    ldy     ram_D4                  ;3        
    cpy     #$08                    ;2        
    bcs     LF417                   ;2/3      
    jmp     LF546                   ;3   =  10
    
LF417
    lda     ram_DC                  ;3        
    cpy     #$1D                    ;2        
    bcs     LF424                   ;2/3      
    cpy     #$14                    ;2        
    bcs     LF427                   ;2/3      
    jmp     LF5C8                   ;3   =  14
    
LF424
    beq     LF46F                   ;2/3 =   2
LF426
    rts                             ;6   =   6
    
LF427
    bne     LF435                   ;2/3      
    ldx     ram_D2                  ;3        
    stx     ram_81                  ;3        
    dec     ram_9F                  ;5        
    bmi     LF43F                   ;2/3      
    inc     ram_D4                  ;5        
    bne     LF451                   ;2/3 =  22
LF435
    cmp     #$16                    ;2        
    bcc     LF445                   ;2/3      
    ldx     #$00                    ;2        
    stx     ram_DC                  ;3        
    stx     ram_E0                  ;3   =  12
LF43F
    lda     #$1D                    ;2        
    sta     ram_D4                  ;3        
    bne     LF46C                   ;2/3 =   7
LF445
    jsr     LF641                   ;6        
    ldx     LF663,y                 ;4        
    sta     ram_80,x                ;4        
    ldx     #$03                    ;2        
    brk                             ;7   =  23
    
    brk                             ;7   =   7 *
    
LF451
    cmp     #$11                    ;2        
    bcs     LF464                   ;2/3      
    ldx     #$30                    ;2        
    clc                             ;2        
    adc     ram_E0                  ;3        
    cmp     #$11                    ;2        
    bcc     LF46C                   ;2/3      
    bne     LF464                   ;2/3      
    ldy     ram_E4                  ;3         *
    bmi     LF46C                   ;2/3 =  22 *
LF464
    cmp     #$16                    ;2        
    bcs     LF46C                   ;2/3      
    sta     ram_DC                  ;3        
    bne     LF43F                   ;2/3 =   9
LF46C
    stx     ram_A3                  ;3        
    rts                             ;6   =   9
    
LF46F
    ldx     #$02                    ;2   =   2
LF471
    lda     LF7E4,x                 ;4        
    and     ram_D5                  ;3        
    bne     LF4A0                   ;2/3      
    lda     ram_92,x                ;4        
    cmp     #$04                    ;2        
    bcs     LF4A0                   ;2/3      
    lda     ram_D9,x                ;4        
    cmp     #$0C                    ;2        
    bcs     LF486                   ;2/3      
    adc     ram_DD,x                ;4   =  29
LF486
    sta     ram_C2                  ;3        
    lda     ram_DC                  ;3        
    ldy     #$08                    ;2        
    cmp     ram_C2                  ;3        
    bne     LF498                   ;2/3      
    sty     ram_92,x                ;4         *
    sty     ram_95,x                ;4         *
    lda     ram_E4                  ;3         *
    bmi     LF4A0                   ;2/3 =  26 *
LF498
    dey                             ;2        
    bcs     LF49C                   ;2/3      
    dey                             ;2   =   6
LF49C
    tya                             ;2        
    jsr     LF651                   ;6   =   8
LF4A0
    dex                             ;2        
    bpl     LF471                   ;2/3      
    lda     ram_D5                  ;3        
    sta     ram_D1                  ;3        
    inx                             ;2        
    ldy     #$14                    ;2   =  14
LF4AA
    asl                             ;2        
    bcs     LF4AF                   ;2/3      
    sty     ram_8C,x                ;4   =   8
LF4AF
    inx                             ;2        
    cpx     #$03                    ;2        
    bcc     LF4AA                   ;2/3      
    lda     ram_E4                  ;3        
    ora     #$40                    ;2        
    sta     ram_E4                  ;3        
    inc     ram_D4                  ;5        
    rts                             ;6   =  25
    
LF4BD
    ldx     #$02                    ;2   =   2
LF4BF
    sed                             ;2        
    lda     ram_F4,x                ;4        
    asl                             ;2        
    lda     ram_F7,x                ;4        
    beq     LF51B                   ;2/3!     
    bcs     LF4E3                   ;2/3      
    sbc     #$00                    ;2        
    sta     ram_F7,x                ;4        
    lda     ram_89,x                ;4        
    adc     #$00                    ;2        
    sta     ram_89,x                ;4        
    cld                             ;2        
    lda     ram_86,x                ;4        
    adc     #$00                    ;2        
    sta     ram_86,x                ;4        
    eor     #$0A                    ;2        
    bne     LF519                   ;2/3!     
    lda     ram_F7,x                ;4         *
    jmp     LF4F8                   ;3   =  55 *
    
LF4E3
    sbc     #$01                    ;2        
    sta     ram_F7,x                ;4        
    lda     ram_89,x                ;4        
    sbc     #$01                    ;2        
    sta     ram_89,x                ;4        
    cld                             ;2        
    lda     ram_86,x                ;4        
    sbc     #$00                    ;2        
    sta     ram_86,x                ;4        
    bne     LF519                   ;2/3!     
    lda     ram_89,x                ;4   =  34 *
LF4F8
    bne     LF519                   ;2/3!      *
    sta     ram_E1,x                ;4         *
    lda     #$02                    ;2         *
    sta     ram_D6,x                ;4         *
    lda     #$0B                    ;2         *
    ldy     #$BB                    ;2         *
    bcc     LF50A                   ;2/3       *
    sta     ram_86,x                ;4         *
    sty     ram_89,x                ;4   =  26 *
LF50A
    sty     ram_8F,x                ;4         *
    sta     ram_8C,x                ;4         *
    lda     LF7E4,x                 ;4         *
    ora     ram_D5                  ;3         *
    sta     ram_D5                  ;3         *
    ora     ram_D1                  ;3         *
    sta     ram_D1                  ;3   =  24 *
LF519
    ldy     #$02                    ;2   =   2
LF51B
    dex                             ;2        
    bpl     LF4BF                   ;2/3!     
    tya                             ;2        
    bne     LF528                   ;2/3      
    ldx     ram_F1                  ;3        
    bne     LF528                   ;2/3      
    iny                             ;2        
    sty     ram_E7                  ;3   =  18
LF528
    and     ram_E7                  ;3        
    asl                             ;2        
    sta     AUDC1                   ;3        
    cld                             ;2        
    rts                             ;6   =  16
    
LF52F
    lda     LF663,y                 ;4        
    cmp     #$06                    ;2        
    and     #$03                    ;2        
    bcc     LF543                   ;2/3      
    tax                             ;2        
    lda     LF7E4,x                 ;4        
    and     ram_D5                  ;3        
    beq     LF543                   ;2/3      
    iny                             ;2        
    bne     LF52F                   ;2/3 =  25
LF543
    sty     ram_D4                  ;3        
    rts                             ;6   =   9
    
LF546
    jsr     LF52F                   ;6        
    jsr     LF641                   ;6        
    ldx     #$30                    ;2        
    stx     ram_A3                  ;3        
    ldx     LF663,y                 ;4        
    sta     ram_80,x                ;4        
    dex                             ;2        
    beq     LF559                   ;2/3      
    rts                             ;6   =  35
    
LF559
    sta     ram_D2                  ;3        
    lda     #$75                    ;2        
    sta     ram_81                  ;3        
    txa                             ;2        
    ldx     #$07                    ;2   =  12
LF562
    sta     ram_D9,x                ;4        
    dex                             ;2        
    bpl     LF562                   ;2/3      
    ldx     #$03                    ;2        
    lda     ram_80                  ;3        
    brk                             ;7   =  20
    
    brk                             ;7   =   7 *
    
    lda     ram_D2                  ;3        
    brk                             ;7   =  10
    
    brk                             ;7   =   7 *
    
    clc                             ;2        
    adc     ram_E0                  ;3        
    sta     ram_B2                  ;3        
    ldx     #$02                    ;2   =  10
LF578
    lda     ram_98,x                ;4        
    brk                             ;7   =  11
    
    brk                             ;7   =   7 *
    
    lda     ram_9C,x                ;4        
    brk                             ;7   =  11
    
    brk                             ;7   =   7 *
    
    ldy     ram_E4                  ;3        
    bpl     LF58E                   ;2/3      
    cmp     #$0A                    ;2         *
    bcc     LF58C                   ;2/3       *
    cmp     #$0C                    ;2         *
    bcc     LF58E                   ;2/3 =  13 *
LF58C
    sta     ram_CE,x                ;4   =   4 *
LF58E
    clc                             ;2        
    adc     ram_DD,x                ;4        
    cmp     #$15                    ;2        
    bne     LF5AC                   ;2/3      
    cmp     ram_B2                  ;3         *
    beq     LF5AC                   ;2/3       *
    lda     ram_8F,x                ;4         *
    jsr     LF271                   ;6         *
    clc                             ;2         *
    sed                             ;2         *
    adc     ram_8F,x                ;4         *
    cld                             ;2         *
    sta     ram_8F,x                ;4         *
    lda     #$04                    ;2         *
    ldy     #$02                    ;2         *
    jsr     LF64F                   ;6   =  49 *
LF5AC
    dex                             ;2        
    bpl     LF578                   ;2/3      
    lda     ram_B2                  ;3        
    cmp     #$15                    ;2        
    bne     LF5C5                   ;2/3      
    sta     ram_DC                  ;3        
    lda     ram_D2                  ;3        
    sta     ram_81                  ;3        
    lda     #$1D                    ;2        
    sta     ram_D4                  ;3        
    lda     #$01                    ;2        
    sta     ram_F1                  ;3        
    sta     ram_A3                  ;3   =  33
LF5C5
    jmp     LF624                   ;3   =   3
    
LF5C8
    lda     LF663,y                 ;4        
    and     #$03                    ;2        
    tax                             ;2        
    lda     ram_D1                  ;3        
    cmp     #$F0                    ;2        
    bcc     LF640                   ;2/3!     
    sty     ram_CE,x                ;4        
    lda     ram_92,x                ;4        
    ldx     LF663,y                 ;4        
    cmp     #$02                    ;2        
    beq     LF61C                   ;2/3!     
    jsr     LF641                   ;6        
    sta     ram_80,x                ;4        
    pha                             ;3        
    txa                             ;2        
    and     #$03                    ;2        
    tax                             ;2        
    pla                             ;4        
    brk                             ;7   =  61
    
    brk                             ;7   =   7 *
    
    cmp     #$16                    ;2        
    lda     ram_92,x                ;4        
    eor     #$03                    ;2        
    php                             ;3        
    bne     LF5FE                   ;2/3      
    sed                             ;2        
    clc                             ;2        
    lda     ram_8F,x                ;4        
    adc     ram_8F,x                ;4        
    sta     ram_8F,x                ;4        
    cld                             ;2   =  31
LF5FE
    lda     #$05                    ;2        
    ldy     #$01                    ;2        
    plp                             ;4        
    bcs     LF616                   ;2/3      
    beq     LF619                   ;2/3      
    lda     ram_E4                  ;3        
    bmi     LF624                   ;2/3      
    lda     ram_AC,x                ;4        
    cmp     #$77                    ;2        
    beq     LF624                   ;2/3      
    lda     #$06                    ;2         *
    ldy     #$00                    ;2         *
    clc                             ;2   =  31 *
LF616
    jsr     LF64F                   ;6   =   6
LF619
    ldy     ram_D4                  ;3        
    dey                             ;2   =   5
LF61C
    tya                             ;2        
    clc                             ;2        
    adc     #$04                    ;2        
    and     #$FC                    ;2        
    sta     ram_D4                  ;3   =  11
LF624
    ldy     ram_D4                  ;3        
    cpy     #$14                    ;2        
    bcs     LF637                   ;2/3      
    jsr     LF52F                   ;6        
    bcc     LF637                   ;2/3      
    lda     ram_92,x                ;4        
    cmp     #$04                    ;2        
    beq     LF61C                   ;2/3      
    bne     LF639                   ;2/3 =  25
LF637
    ldx     #$04                    ;2   =   2
LF639
    lda     LF7E4,x                 ;4        
    eor     #$F9                    ;2        
    sta     ram_D1                  ;3   =   9
LF640
    rts                             ;6   =   6
    
LF641
    lda     #$06                    ;2        
    sta     AUDC1                   ;3        
    dec     ram_D3                  ;5        
    lda     ram_AB                  ;3        
    asl                             ;2        
    inc     ram_9B                  ;5        
    inc     ram_D4                  ;5        
    rts                             ;6   =  31
    
LF64F
    sty     ram_F1                  ;3   =   3
LF651
    sta     ram_92,x                ;4        
    sta     ram_95,x                ;4        
    ror                             ;2        
    sta     ram_F4,x                ;4        
    lda     ram_8F,x                ;4        
    sta     ram_F7,x                ;4        
    lda     #$6C                    ;2        
    sta     ram_E7                  ;3        
    dec     ram_9F                  ;5   =  32
LF662
    rts                             ;6   =   6
    
LF663
    .byte   $1A,$19,$18,$00,$1E,$1D,$1C,$01 ; $F663 (D)
    .byte   $22,$26,$2A,$2E,$21,$25,$29,$2D ; $F66B (D)
    .byte   $20,$24,$28,$2C,$01,$02,$03,$04 ; $F673 (D)
    .byte   $05                             ; $F67B (D)
    .byte   $00,$01,$02,$03                 ; $F67C (*)
    
    .byte   %00000000 ; |        |            $F680 (G)
    .byte   %00100010 ; |  #   # |            $F681 (G)
    .byte   %00100010 ; |  #   # |            $F682 (G)
    .byte   %00100010 ; |  #   # |            $F683 (G)
    .byte   %00111110 ; |  ##### |            $F684 (G)
    .byte   %00100010 ; |  #   # |            $F685 (G)
    .byte   %00100010 ; |  #   # |            $F686 (G)
    .byte   %00011100 ; |   ###  |            $F687 (G)
    .byte   %00000000 ; |        |            $F688 (G)
    .byte   %00111110 ; |  ##### |            $F689 (G)
    .byte   %00100000 ; |  #     |            $F68A (G)
    .byte   %00100000 ; |  #     |            $F68B (G)
    .byte   %00111110 ; |  ##### |            $F68C (G)
    .byte   %00000010 ; |      # |            $F68D (G)
    .byte   %00000010 ; |      # |            $F68E (G)
    .byte   %00111110 ; |  ##### |            $F68F (G)
    .byte   %00000000 ; |        |            $F690 (G)
    .byte   %00111110 ; |  ##### |            $F691 (G)
    .byte   %00000010 ; |      # |            $F692 (G)
    .byte   %00000010 ; |      # |            $F693 (G)
    .byte   %00001110 ; |    ### |            $F694 (G)
    .byte   %00000010 ; |      # |            $F695 (G)
    .byte   %00000010 ; |      # |            $F696 (G)
    .byte   %00111110 ; |  ##### |            $F697 (G)
    .byte   %00000000 ; |        |            $F698 (G)
    .byte   %00000100 ; |     #  |            $F699 (G)
    .byte   %00000100 ; |     #  |            $F69A (G)
    .byte   %00000100 ; |     #  |            $F69B (G)
    .byte   %00111110 ; |  ##### |            $F69C (G)
    .byte   %00100100 ; |  #  #  |            $F69D (G)
    .byte   %00100100 ; |  #  #  |            $F69E (G)
    .byte   %00100000 ; |  #     |            $F69F (G)
    .byte   %00000000 ; |        |            $F6A0 (G)
    .byte   %00111110 ; |  ##### |            $F6A1 (G)
    .byte   %00000010 ; |      # |            $F6A2 (G)
    .byte   %00000010 ; |      # |            $F6A3 (G)
    .byte   %00111110 ; |  ##### |            $F6A4 (G)
    .byte   %00100000 ; |  #     |            $F6A5 (G)
    .byte   %00100000 ; |  #     |            $F6A6 (G)
    .byte   %00111110 ; |  ##### |            $F6A7 (G)
    .byte   %00000000 ; |        |            $F6A8 (G)
    .byte   %00111110 ; |  ##### |            $F6A9 (G)
    .byte   %00100010 ; |  #   # |            $F6AA (G)
    .byte   %00100010 ; |  #   # |            $F6AB (G)
    .byte   %00111110 ; |  ##### |            $F6AC (G)
    .byte   %00100000 ; |  #     |            $F6AD (G)
    .byte   %00100000 ; |  #     |            $F6AE (G)
    .byte   %00111110 ; |  ##### |            $F6AF (G)
    .byte   %00000000 ; |        |            $F6B0 (G)
    .byte   %00000010 ; |      # |            $F6B1 (G)
    .byte   %00000010 ; |      # |            $F6B2 (G)
    .byte   %00000010 ; |      # |            $F6B3 (G)
    .byte   %00000010 ; |      # |            $F6B4 (G)
    .byte   %00000010 ; |      # |            $F6B5 (G)
    .byte   %00000010 ; |      # |            $F6B6 (G)
    .byte   %00111110 ; |  ##### |            $F6B7 (G)
    .byte   %00000000 ; |        |            $F6B8 (G)
    
    .byte   $3E,$22,$22,$3E,$22,$22,$3E     ; $F6B9 (*)
    
    .byte   %00000000 ; |        |            $F6C0 (G)
    .byte   %00111110 ; |  ##### |            $F6C1 (G)
    .byte   %00000010 ; |      # |            $F6C2 (G)
    .byte   %00000010 ; |      # |            $F6C3 (G)
    .byte   %00111110 ; |  ##### |            $F6C4 (G)
    .byte   %00100010 ; |  #   # |            $F6C5 (G)
    .byte   %00100010 ; |  #   # |            $F6C6 (G)
    .byte   %00111110 ; |  ##### |            $F6C7 (G)
    .byte   %00000000 ; |        |            $F6C8 (G)
    .byte   %00101110 ; |  # ### |            $F6C9 (G)
    .byte   %00101010 ; |  # # # |            $F6CA (G)
    .byte   %00101010 ; |  # # # |            $F6CB (G)
    .byte   %00101010 ; |  # # # |            $F6CC (G)
    .byte   %00101010 ; |  # # # |            $F6CD (G)
    .byte   %00101010 ; |  # # # |            $F6CE (G)
    .byte   %00101110 ; |  # ### |            $F6CF (G)
    .byte   %00000000 ; |        |            $F6D0 (G)
    .byte   %00111100 ; |  ####  |            $F6D1 (G)
    .byte   %00100100 ; |  #  #  |            $F6D2 (G)
    .byte   %00000100 ; |     #  |            $F6D3 (G)
    .byte   %00000100 ; |     #  |            $F6D4 (G)
    .byte   %00000100 ; |     #  |            $F6D5 (G)
    .byte   %00000100 ; |     #  |            $F6D6 (G)
    .byte   %00001110 ; |    ### |            $F6D7 (G)
    .byte   %00000000 ; |        |            $F6D8 (G)
    .byte   %00000010 ; |      # |            $F6D9 (G)
    .byte   %00111100 ; |  ####  |            $F6DA (G)
    .byte   %00101100 ; |  # ##  |            $F6DB (G)
    .byte   %00100100 ; |  #  #  |            $F6DC (G)
    .byte   %00100100 ; |  #  #  |            $F6DD (G)
    .byte   %00100100 ; |  #  #  |            $F6DE (G)
    .byte   %00111100 ; |  ####  |            $F6DF (G)
    .byte   %00000000 ; |        |            $F6E0 (G)
    .byte   %00100010 ; |  #   # |            $F6E1 (G)
    .byte   %00100100 ; |  #  #  |            $F6E2 (G)
    .byte   %00101000 ; |  # #   |            $F6E3 (G)
    .byte   %00110000 ; |  ##    |            $F6E4 (G)
    .byte   %00101000 ; |  # #   |            $F6E5 (G)
    .byte   %00100100 ; |  #  #  |            $F6E6 (G)
    .byte   %00100010 ; |  #   # |            $F6E7 (G)
LF6E8
    .byte   %00000000 ; |        |            $F6E8 (G)
    
    .byte   $60                             ; $F6E9 (D)
    .byte   $10                             ; $F6EA (*)
    .byte   $21                             ; $F6EB (D)
    
    .byte   %11111111 ; |########|            $F6EC (G)
    .byte   %00111111 ; |  ######|            $F6ED (G)
    .byte   %01111111 ; | #######|            $F6EE (G)
    .byte   %00111111 ; |  ######|            $F6EF (G)
    .byte   %11000111 ; |##   ###|            $F6F0 (G)
    .byte   %11010101 ; |## # # #|            $F6F1 (G)
    .byte   %11010101 ; |## # # #|            $F6F2 (G)
    .byte   %11111000 ; |#####   |            $F6F3 (G)
    .byte   %11111111 ; |########|            $F6F4 (G)
    .byte   %11111111 ; |########|            $F6F5 (G)
    .byte   %11111111 ; |########|            $F6F6 (G)
    .byte   %11111111 ; |########|            $F6F7 (G)
    .byte   %11111111 ; |########|            $F6F8 (G)
    .byte   %11111111 ; |########|            $F6F9 (G)
    .byte   %11111111 ; |########|            $F6FA (G)
    .byte   %11111111 ; |########|            $F6FB (G)
    .byte   %11111111 ; |########|            $F6FC (G)
    
LF6FD
    .byte   $80                             ; $F6FD (D)
    .byte   $40,$08                         ; $F6FE (*)
    
    .byte   %01110000 ; | ###    |            $F700 (G)
    .byte   %01010000 ; | # #    |            $F701 (G)
    .byte   %01010000 ; | # #    |            $F702 (G)
    .byte   %01010000 ; | # #    |            $F703 (G)
    .byte   %01110000 ; | ###    |            $F704 (G)
    .byte   %00100000 ; |  #     |            $F705 (G)
    .byte   %00100000 ; |  #     |            $F706 (G)
    .byte   %00100000 ; |  #     |            $F707 (G)
    .byte   %00100000 ; |  #     |            $F708 (G)
    .byte   %00100000 ; |  #     |            $F709 (G)
    .byte   %01110000 ; | ###    |            $F70A (G)
    .byte   %01000000 ; | #      |            $F70B (G)
    .byte   %01110000 ; | ###    |            $F70C (G)
    .byte   %00010000 ; |   #    |            $F70D (G)
    .byte   %01110000 ; | ###    |            $F70E (G)
    .byte   %01110000 ; | ###    |            $F70F (G)
    .byte   %00010000 ; |   #    |            $F710 (G)
    .byte   %00110000 ; |  ##    |            $F711 (G)
    .byte   %00010000 ; |   #    |            $F712 (G)
    .byte   %01110000 ; | ###    |            $F713 (G)
    
    .byte   $10,$10,$70,$50,$40,$70,$10,$70 ; $F714 (*)
    .byte   $40,$70,$70,$50,$70,$40,$70     ; $F71C (*)
    
    .byte   %00010000 ; |   #    |            $F723 (G)
    .byte   %00010000 ; |   #    |            $F724 (G)
    .byte   %00010000 ; |   #    |            $F725 (G)
    .byte   %00010000 ; |   #    |            $F726 (G)
    .byte   %01110000 ; | ###    |            $F727 (G)
    .byte   %01110000 ; | ###    |            $F728 (G)
    .byte   %01010000 ; | # #    |            $F729 (G)
    .byte   %01110000 ; | ###    |            $F72A (G)
    .byte   %01010000 ; | # #    |            $F72B (G)
    .byte   %01110000 ; | ###    |            $F72C (G)
    .byte   %01110000 ; | ###    |            $F72D (G)
    .byte   %00010000 ; |   #    |            $F72E (G)
    .byte   %01110000 ; | ###    |            $F72F (G)
    .byte   %01010000 ; | # #    |            $F730 (G)
    .byte   %01110000 ; | ###    |            $F731 (G)
    
    .byte   $17,$15,$15,$15,$17             ; $F732 (*)
    
    .byte   %00000000 ; |        |            $F737 (G)
    .byte   %00000000 ; |        |            $F738 (G)
    .byte   %00000000 ; |        |            $F739 (G)
    .byte   %00000000 ; |        |            $F73A (G)
    .byte   %00000000 ; |        |            $F73B (G)
    .byte   %10010100 ; |#  # #  |            $F73C (G)
    .byte   %10010100 ; |#  # #  |            $F73D (G)
    .byte   %11110100 ; |#### #  |            $F73E (G)
    .byte   %10010100 ; |#  # #  |            $F73F (G)
    .byte   %10010101 ; |#  # # #|            $F740 (G)
    .byte   %11100100 ; |###  #  |            $F741 (G)
    .byte   %00100100 ; |  #  #  |            $F742 (G)
    .byte   %11100100 ; |###  #  |            $F743 (G)
    .byte   %10000100 ; |#    #  |            $F744 (G)
    .byte   %11101110 ; |### ### |            $F745 (G)
    .byte   %11101110 ; |### ### |            $F746 (G)
    .byte   %10101010 ; |# # # # |            $F747 (G)
    .byte   %10101110 ; |# # ### |            $F748 (G)
    .byte   %10101010 ; |# # # # |            $F749 (G)
    .byte   %11101110 ; |### ### |            $F74A (G)
    
    .byte   $07,$05,$07,$05,$07             ; $F74B (*)
    
    .byte   %01110111 ; | ### ###|            $F750 (G)
    .byte   %01010101 ; | # # # #|            $F751 (G)
    .byte   %01110101 ; | ### # #|            $F752 (G)
    .byte   %01010101 ; | # # # #|            $F753 (G)
    .byte   %01110101 ; | ### # #|            $F754 (G)
    .byte   %01101101 ; | ## ## #|            $F755 (G)
    .byte   %01010101 ; | # # # #|            $F756 (G)
    .byte   %01000101 ; | #   # #|            $F757 (G)
    .byte   %01000101 ; | #   # #|            $F758 (G)
    .byte   %01000101 ; | #   # #|            $F759 (G)
    .byte   %01110111 ; | ### ###|            $F75A (G)
    .byte   %01000101 ; | #   # #|            $F75B (G)
    .byte   %01000101 ; | #   # #|            $F75C (G)
    .byte   %01000101 ; | #   # #|            $F75D (G)
    .byte   %01000111 ; | #   ###|            $F75E (G)
    
    .byte   $25,$25,$25,$25,$75             ; $F75F (*)
    
    .byte   %00100000 ; |  #     |            $F764 (G)
    .byte   %00000000 ; |        |            $F765 (G)
    .byte   %00110000 ; |  ##    |            $F766 (G)
    .byte   %00010000 ; |   #    |            $F767 (G)
    .byte   %01110000 ; | ###    |            $F768 (G)
    
LF769
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    cmp     #$0A                    ;2        
    bcc     LF772                   ;2/3      
    lda     #$09                    ;2   =  12
LF772
    cmp     #$00                    ;2        
    bne     LF77A                   ;2/3      
    ldy     #$0A                    ;2        
    sty     ram_DD,x                ;4   =  10
LF77A
    sec                             ;2        
    adc     ram_D9,x                ;4        
    sta     ram_D9,x                ;4        
    rti                             ;6   =  16
    
    .byte   $07,$05,$05,$05,$07             ; $F780 (D)
    
    .byte   %00000010 ; |      # |            $F785 (G)
    .byte   %00000010 ; |      # |            $F786 (G)
    .byte   %00000010 ; |      # |            $F787 (G)
    .byte   %00000010 ; |      # |            $F788 (G)
    .byte   %00000010 ; |      # |            $F789 (G)
    .byte   %00000111 ; |     ###|            $F78A (G)
    .byte   %00000100 ; |     #  |            $F78B (G)
    .byte   %00000111 ; |     ###|            $F78C (G)
    .byte   %00000001 ; |       #|            $F78D (G)
    .byte   %00000111 ; |     ###|            $F78E (G)
    
    .byte   $07,$01,$03,$01,$07,$01,$01,$07 ; $F78F (D)
    .byte   $05,$04,$07,$01,$07,$04,$07,$07 ; $F797 (D)
    .byte   $05,$07,$04,$07,$01,$01,$01,$01 ; $F79F (D)
    .byte   $07,$07,$05,$07,$05,$07,$07,$01 ; $F7A7 (D)
    .byte   $07,$05                         ; $F7AF (D)
LF7B1
    .byte   $07                             ; $F7B1 (D)
    .byte   $3E                             ; $F7B2 (A)
    .byte   $31                             ; $F7B3 (*)
    .byte   $F0                             ; $F7B4 (A)
    .byte   $FE                             ; $F7B5 (A)
    .byte   $51                             ; $F7B6 (*)
    .byte   $00                             ; $F7B7 (A)
    .byte   $00,$00,$00,$00,$80,$80,$80,$80 ; $F7B8 (D)
    .byte   $C0,$A4,$A4,$E4,$AE,$EA,$EE,$88 ; $F7C0 (D)
    .byte   $8C,$88,$8E                     ; $F7C8 (D)
    .byte   $70,$50,$10,$10,$38             ; $F7CB (*)
    .byte   $72,$12,$72,$42,$77,$44,$4C,$54 ; $F7D0 (D)
    .byte   $64,$44,$77,$14,$76,$44,$77     ; $F7D8 (D)
    .byte   $C0,$00,$80,$00,$C0             ; $F7DF (*)
LF7E4
    .byte   $80,$40,$20,$10,$08,$04,$02,$01 ; $F7E4 (D)
LF7EC
    .byte   $08,$0E,$00,$06                 ; $F7EC (*)
    .byte   $36,$0C,$D0                     ; $F7F0 (D)
LF7F3
    .byte   $D6,$50,$77,$FF,$1E,$50,$02,$02 ; $F7F3 (D)
    .byte   $02,$00,$F0,$69                 ; $F7FB (D)
    .byte   $F7                             ; $F7FF (*)
