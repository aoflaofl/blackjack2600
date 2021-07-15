; Disassembly of ~/Downloads/Roms/HC ROMS.zip/HC ROMS/BY COMPANY/Atari - Sears/Blackjack - Black Jack.bin
; Disassembled Thu Jul 15 14:26:46 2021
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
CYAN             = $a0
CYAN_GREEN       = $b0
GREEN            = $c0
GREEN_YELLOW     = $d0
GREEN_BEIGE      = $e0
BEIGE            = $f0


;-----------------------------------------------------------
;      TIA and IO constants accessed
;-----------------------------------------------------------

CXM0P           = $00  ; (R)
INPT0           = $08  ; (R)
;INPT1          = $09  ; (Ri)
;INPT2          = $0a  ; (Ri)
;INPT3          = $0b  ; (Ri)

VSYNC           = $00  ; (W)
VBLANK          = $01  ; (W)
WSYNC           = $02  ; (W)
NUSIZ0          = $04  ; (W)
NUSIZ1          = $05  ; (W)
COLUP0          = $06  ; (W)
COLUP1          = $07  ; (W)
COLUPF          = $08  ; (W)
COLUBK          = $09  ; (W)
CTRLPF          = $0a  ; (W)
PF1             = $0e  ; (W)
PF2             = $0f  ; (W)
RESP0           = $10  ; (W)
RESP1           = $11  ; (W)
AUDC0           = $15  ; (W)
AUDC1           = $16  ; (W)
AUDF0           = $17  ; (W)
AUDF1           = $18  ; (W)
AUDV0           = $19  ; (W)
AUDV1           = $1a  ; (W)
GRP0            = $1b  ; (W)
GRP1            = $1c  ; (W)
HMP0            = $20  ; (W)
HMP1            = $21  ; (W)
HMOVE           = $2a  ; (W)

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
;                 $8a  (i)
;                 $8b  (i)
ram_8C          = $8c
;                 $8d  (i)
;                 $8e  (i)
ram_8F          = $8f
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
;                 $9a  (i)
ram_9B          = $9b
ram_9C          = $9c
;                 $9d  (i)
;                 $9e  (i)
ram_9F          = $9f
;                 $a0  (i)
;                 $a1  (i)
;                 $a2  (i)
ram_A3          = $a3
;                 $a4  (i)
;                 $a5  (i)
;                 $a6  (i)
ram_A7          = $a7
;                 $a8  (i)
;                 $a9  (i)
;                 $aa  (i)
ram_AB          = $ab
ram_AC          = $ac
;                 $ad  (i)
;                 $ae  (i)
ram_AF          = $af
ram_B0          = $b0
ram_B1          = $b1
ram_B2          = $b2
;                 $b3  (i)
ram_B4          = $b4
;                 $b5  (i)
ram_B6          = $b6
ram_B7          = $b7
ram_B8          = $b8
;                 $b9  (i)
ram_BA          = $ba
;                 $bb  (i)
ram_BC          = $bc
;                 $bd  (i)
ram_BE          = $be
;                 $bf  (i)
ram_C0          = $c0
;                 $c1  (i)
ram_C2          = $c2
;                 $c3  (i)
ram_C4          = $c4
;                 $c5  (i)
ram_C6          = $c6
;                 $c7  (i)
ram_C8          = $c8
ram_C9          = $c9
ram_CA          = $ca
ram_CB          = $cb
ram_CC          = $cc
ram_CD          = $cd
ram_CE          = $ce
;                 $cf  (i)
ram_D0          = $d0
ram_D1          = $d1
ram_D2          = $d2
ram_D3          = $d3
ram_D4          = $d4
ram_D5          = $d5
ram_D6          = $d6
;                 $d7  (i)
;                 $d8  (i)
ram_D9          = $d9
;                 $da  (i)
;                 $db  (i)
ram_DC          = $dc
ram_DD          = $dd
;                 $de  (i)
;                 $df  (i)
ram_E0          = $e0
ram_E1          = $e1

ram_E4          = $e4
ram_E5          = $e5
ram_E6          = $e6
ram_E7          = $e7
ram_E8          = $e8
ram_E9          = $e9
ram_EA          = $ea
;                 $eb  (i)
;                 $ec  (i)
;                 $ed  (i)
;                 $ee  (i)
;                 $ef  (i)
;                 $f0  (i)
ram_F1          = $f1
ram_F2          = $f2
ram_F3          = $f3
ram_F4          = $f4
;                 $f5  (i)
;                 $f6  (i)
ram_F7          = $f7
;                 $f8  (i)
;                 $f9  (i)

;                 $fb  (s)
;                 $fc  (s)
;                 $fd  (s)
;                 $fe  (s)
;                 $ff  (s)


;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

Break           = $f769


;***********************************************************
;      Bank 0
;***********************************************************

    SEG     CODE
    ORG     $f000

    sei                             ;2        
    cld                             ;2        
    ldx     #$ff                    ;2        
    txs                             ;2        
    inx                             ;2        
    txa                             ;2   =  12
Lf007
    sta     VSYNC,x                 ;4        
    inx                             ;2        
    bne     Lf007                   ;2/3      
    ldx     #$0f                    ;2        
    stx     AUDV1                   ;3        
    stx     AUDF1                   ;3        
    inx                             ;2   =  18
Lf013
    lda     Lf7f3,x                 ;4        
    sta     ram_D0,x                ;4        
    ldy     #$f6                    ;2        
    sty     ram_86,x                ;4        
    sty     ram_B1,x                ;4        
    iny                             ;2        
    sty     ram_B7,x                ;4        
    dex                             ;2        
    bpl     Lf013                   ;2/3      
    sty     AUDV0                   ;3        
    jsr     Lf3dd                   ;6        
    jsr     Lf2e9                   ;6        
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
Lf03f
    dex                             ;2        
    bpl     Lf03f                   ;2/3      
    sta     RESP1                   ;3        
    sta     RESP0                   ;3        
    lda     #$30                    ;2        
    sta     HMP1                    ;3        
    lda     #$40                    ;2        
    sta     HMP0                    ;3        
    sta     WSYNC                   ;3   =  23
;---------------------------------------
    sta     HMOVE                   ;3   =   3
Lf052
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
    jsr     Lf27d                   ;6        
    inc     ram_A7                  ;5        
    lda     ram_A7                  ;3        
    bne     Lf07c                   ;2/3      
    dec     ram_F2                  ;5        
    bne     Lf07c                   ;2/3      
    ldx     #$ff                    ;2         *
    stx     ram_F3                  ;3   =  37 *
Lf07c
    and     #$03                    ;2        
    sta     ram_AF                  ;3        
    bne     Lf08e                   ;2/3      
    lda     SWCHA                   ;4        
    tay                             ;2        
    eor     ram_E5                  ;3        
    and     ram_E5                  ;3        
    sty     ram_E5                  ;3        
    sta     ram_E6                  ;3   =  25
Lf08e
    dec     ram_A3                  ;5        
    bpl     Lf09a                   ;2/3      
    lda     ram_E4                  ;3        
    ora     #$02                    ;2        
    sta     ram_E4                  ;3        
    inc     ram_A3                  ;5   =  20
Lf09a
    lda     ram_E8                  ;3        
    asl                             ;2        
    eor     ram_E8                  ;3        
    asl                             ;2        
    asl                             ;2        
    rol     ram_E9                  ;5        
    rol     ram_E8                  ;5        
    ldx     ram_9B                  ;3        
    beq     Lf0da                   ;2/3      
    lda     ram_A3                  ;3        
    bne     Lf0da                   ;2/3      
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
    and     #$fd                    ;2        
    sta     ram_AB                  ;3        
    cmp     #$34                    ;2        
    bcs     Lf0da                   ;2/3      
    lda     ram_EA,x                ;4        
    and     Lf7e4,y                 ;4        
    beq     Lf0da                   ;2/3      
    eor     ram_EA,x                ;4        
    sta     ram_EA,x                ;4        
    dec     ram_9B                  ;5   =  99
Lf0da
    lda     INTIM                   ;4        
    bne     Lf0da                   ;2/3      
    sta     VBLANK                  ;3        
    ldy     #$80                    ;2        
    sty     ram_C8                  ;3        
    lda     #$01                    ;2        
    jsr     Lf1e4                   ;6        
    lda     ram_CB                  ;3        
    tax                             ;2        
    jsr     Lf13b                   ;6        
    ldx     ram_CC                  ;3        
    jsr     Lf13f                   ;6        
    lda     ram_CC                  ;3        
    tax                             ;2        
    ldy     #$37                    ;2        
    jsr     Lf13b                   ;6        
    lda     #$02                    ;2        
    sta     ram_C2                  ;3        
    lda     #$05                    ;2        
    jsr     Lf1e4                   ;6        
    sta     ram_C2                  ;3        
    lda     ram_E6                  ;3        
    beq     Lf110                   ;2/3      
    sty     ram_F2                  ;3        
    sty     ram_F3                  ;3   =  82
Lf110
    dey                             ;2        
    lda     SWCHB                   ;4        
    and     #$08                    ;2        
    lsr                             ;2        
    bne     Lf11b                   ;2/3      
    ldy     #$0f                    ;2   =  14 *
Lf11b
    sty     ram_B0                  ;3        
    ora     #$03                    ;2        
    tay                             ;2        
    ldx     #$03                    ;2   =   9
Lf122
    lda     ram_F2                  ;3        
    and     ram_F3                  ;3        
    and     ram_B0                  ;3        
    eor     Lf7ec,y                 ;4        
    sta     ram_CA,x                ;4        
    dey                             ;2        
    dex                             ;2        
    bpl     Lf122                   ;2/3      
    ldx     #$16                    ;2   =  25
Lf133
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    dex                             ;2        
    bpl     Lf133                   ;2/3      
    jmp     Lf052                   ;3   =   7
    
Lf13b
    sty     ram_B0                  ;3        
    sta     COLUP1                  ;3   =   6
Lf13f
    stx     COLUP0                  ;3        
    ldx     ram_C8                  ;3        
    ldy     #$10                    ;2   =   8
Lf145
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
    cpy     #$0c                    ;2        
    bcs     Lf145                   ;2/3 =  40
Lf15f
    lda     CXM0P,x                 ;4        
    and     #$0f                    ;2        
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
    and     #$f0                    ;2        
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
    bpl     Lf15f                   ;2/3      
    stx     ram_C8                  ;3        
    ldy     #$04                    ;2        
    lda     (ram_B8),y              ;5        
    ora     (ram_B6),y              ;5        
    pha                             ;3        
    lda     (ram_C6),y              ;5        
    sta     WSYNC                   ;3   =  92
;---------------------------------------
    bcc     Lf19e                   ;2/3 =   2
Lf197
    lda     (ram_B8),y              ;5        
    ora     (ram_B6),y              ;5        
    pha                             ;3        
    lda     (ram_C6),y              ;5   =  18
Lf19e
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
    bpl     Lf197                   ;2/3      
    iny                             ;2        
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    rts                             ;6   = 147
    
Lf1e4
    sta     ram_C6                  ;3   =   3
Lf1e6
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     ram_CD                  ;3        
    sta     COLUP0                  ;3        
    lda     #$ff                    ;2        
    sta     GRP0                    ;3        
    lda     ram_CB                  ;3        
    sta     COLUPF                  ;3        
    ldx     #$04                    ;2   =  19
Lf1f6
    ldy     #$00                    ;2        
    lda     (ram_C8),y              ;5        
    and     #$fc                    ;2        
    sta     ram_B0,x                ;4        
    eor     (ram_C8),y              ;5        
    tay                             ;2        
    lda.wy  ram_CA,y                ;4        
    sta     ram_B6,x                ;4        
    dex                             ;2        
    inc     ram_C8                  ;5        
    dex                             ;2        
    bpl     Lf1f6                   ;2/3!     
    ldy     #$88                    ;2        
    cpy     ram_C8                  ;3        
    bcs     Lf214                   ;2/3      
    inc     ram_C8                  ;5   =  51
Lf214
    sed                             ;2   =   2
Lf215
    sta     WSYNC                   ;3   =   3
;---------------------------------------
    lda     (ram_B4),y              ;5        
    sta     GRP0                    ;3        
    lda     ram_BA                  ;3        
    sta     COLUP0                  ;3        
    ldx     ram_AF                  ;3        
    lda     INPT0|$30,x             ;4        
    eor     #$ff                    ;2        
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
    bcs     Lf215                   ;2/3      
    cld                             ;2        
    dec     ram_C6                  ;5        
    bmi     Lf265                   ;2/3      
    jmp     Lf1e6                   ;3   =  69
    
Lf265
    ldy     #$00                    ;2        
    sty     GRP0                    ;3        
    lda     ram_CD                  ;3        
    sta     COLUBK                  ;3        
    sta     COLUPF                  ;3        
    lda     ram_C2                  ;3   =  17
Lf271
    lsr                             ;2        
    pha                             ;3        
    and     #$08                    ;2        
    cmp     #$02                    ;2        
    pla                             ;4        
    bcc     Lf27c                   ;2/3      
    sbc     #$03                    ;2   =  17
Lf27c
    rts                             ;6   =   6
    
Lf27d
    sty     AUDC1                   ;3        
    ldx     ram_F1                  ;3        
    txa                             ;2        
    beq     Lf291                   ;2/3      
    lda     ram_E7                  ;3        
    and     Lf6e8,x                 ;4        
    bne     Lf28e                   ;2/3      
    inx                             ;2        
    inx                             ;2        
    inx                             ;2   =  25
Lf28e
    lda     Lf7b1,x                 ;4   =   4
Lf291
    sta     AUDC0                   ;3        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    cpx     #$03                    ;2        
    beq     Lf2a0                   ;2/3      
    cpx     #$06                    ;2        
    bne     Lf2a6                   ;2/3      
    stx     AUDC0                   ;3   =  20
Lf2a0
    lda     #$1f                    ;2        
    and     ram_E7                  ;3        
    adc     #$0c                    ;2   =   7
Lf2a6
    lsr                             ;2        
    sta     AUDF0                   ;3        
    ldx     ram_E7                  ;3        
    beq     Lf2cb                   ;2/3      
    dex                             ;2        
    stx     ram_E7                  ;3        
    bne     Lf2c4                   ;2/3      
    lda     ram_F1                  ;3        
    cmp     #$03                    ;2        
    bne     Lf2c2                   ;2/3      
    lda     ram_E9                  ;3        
    ora     ram_E8                  ;3        
    beq     Lf2cb                   ;2/3      
    lda     ram_D2                  ;3        
    sta     ram_81                  ;3   =  38
Lf2c2
    stx     ram_F1                  ;3   =   3
Lf2c4
    txa                             ;2        
    lsr                             ;2        
    bcc     Lf27c                   ;2/3      
    jmp     Lf4bd                   ;3   =   9
    
Lf2cb
    lda     ram_D4                  ;3        
    cmp     #$1e                    ;2        
    bcs     Lf2d4                   ;2/3      
    jmp     Lf379                   ;3   =  10
    
Lf2d4
    lda     SWCHB                   ;4        
    tax                             ;2        
    eor     ram_E4                  ;3        
    and     ram_E4                  ;3        
    stx     ram_E4                  ;3        
    clc                             ;2        
    and     #$43                    ;2        
    ror                             ;2        
    ror                             ;2        
    bcc     Lf328                   ;2/3!     
    lda     #$3f                    ;2         *
    sta     ram_A3                  ;3   =  30 *
Lf2e9
    clc                             ;2        
    lda     ram_D5                  ;3        
    adc     #$20                    ;2        
    cmp     #$e0                    ;2        
    bcc     Lf2f4                   ;2/3      
    lda     #$10                    ;2   =  13 *
Lf2f4
    sta     ram_D5                  ;3        
    sta     ram_D1                  ;3        
    ldx     #$02                    ;2   =   8
Lf2fa
    lda     ram_86,x                ;4        
    cmp     #$0a                    ;2        
    bcs     Lf306                   ;2/3      
    sta     ram_D6,x                ;4         *
    lda     ram_89,x                ;4         *
    sta     ram_E1,x                ;4   =  20 *
Lf306
    lda     #$0b                    ;2        
    sta     ram_86,x                ;4        
    tay                             ;2        
    lda     #$bb                    ;2        
    sta     ram_89,x                ;4        
    sta     ram_8F,x                ;4        
    lda     ram_D5                  ;3        
    and     Lf7e4,x                 ;4        
    bne     Lf322                   ;2/3      
    lda     ram_D6,x                ;4        
    sta     ram_86,x                ;4        
    lda     ram_E1,x                ;4        
    sta     ram_89,x                ;4        
    ldy     #$14                    ;2   =  45
Lf322
    sty     ram_8C,x                ;4        
    dex                             ;2        
    bpl     Lf2fa                   ;2/3!     
    rts                             ;6   =  14
    
Lf328
    asl                             ;2        
    bne     Lf345                   ;2/3      
    bcc     Lf348                   ;2/3      
    sty     ram_F2                  ;3        
    sty     ram_F3                  ;3        
    tax                             ;2        
    lda     ram_D5                  ;3        
    sta     ram_C4                  ;3        
    lda     #$02                    ;2   =  22
Lf338
    asl     ram_C4                  ;5        
    bcs     Lf340                   ;2/3      
    sta     ram_86,x                ;4        
    sty     ram_89,x                ;4   =  15
Lf340
    inx                             ;2        
    cpx     #$03                    ;2        
    bcc     Lf338                   ;2/3 =   6
Lf345
    dey                             ;2        
    sty     ram_D3                  ;3   =   5
Lf348
    lda     ram_D3                  ;3        
    bpl     Lf379                   ;2/3      
    lda     #$3c                    ;2        
    sta     ram_E7                  ;3        
    lda     #$03                    ;2        
    sta     ram_F1                  ;3        
    lda     #$6f                    ;2        
    sta     ram_81                  ;3        
    ldx     #$01                    ;2        
    lda     ram_E9                  ;3        
    ora     ram_E8                  ;3        
    bne     Lf367                   ;2/3      
    bcs     Lf365                   ;2/3      
    stx     ram_E7                  ;3        
    rts                             ;6   =  41
    
Lf365
    inc     ram_E8                  ;5   =   5
Lf367
    stx     ram_9B                  ;3        
    ldx     #$06                    ;2        
    stx     AUDC1                   ;3        
    lda     #$ff                    ;2   =  10
Lf36f
    sta     ram_EA,x                ;4        
    dex                             ;2        
    bpl     Lf36f                   ;2/3      
    ldx     #$21                    ;2        
    stx     ram_D3                  ;3        
    rts                             ;6   =  19
    
Lf379
    clc                             ;2        
    lda     ram_D1                  ;3        
    cmp     #$f0                    ;2        
    bcs     Lf3d5                   ;2/3      
    ldx     ram_AF                  ;3        
    lda     Lf7e4,x                 ;4        
    bit     ram_D1                  ;3        
    bne     Lf3d5                   ;2/3      
    tay                             ;2        
    lda     Lf6fd,x                 ;4        
    and     ram_E6                  ;3        
    beq     Lf3a0                   ;2/3      
    lda     #$0b                    ;2        
    sta     ram_8C,x                ;4        
    lda     #$00                    ;2        
    sta     ram_92,x                ;4        
    sta     ram_95,x                ;4        
    lda     #$04                    ;2        
    sta     AUDC1                   ;3        
    tya                             ;2   =  55
Lf3a0
    ora     ram_D1                  ;3        
    sta     ram_D1                  ;3        
    lsr                             ;2        
    lda     ram_C2                  ;3        
    bcs     Lf3c1                   ;2/3      
    ldy     ram_86,x                ;4        
    bne     Lf3b3                   ;2/3      
    cmp     ram_89,x                ;4         *
    bcc     Lf3b3                   ;2/3       *
    lda     ram_89,x                ;4   =  29 *
Lf3b3
    sta     ram_8F,x                ;4        
    lda     ram_D1                  ;3        
    cmp     #$f0                    ;2        
    bcc     Lf3d5                   ;2/3      
    lda     #$00                    ;2        
    sta     ram_D4                  ;3        
    beq     Lf3d5                   ;2/3 =  18
Lf3c1
    ldy     #$01                    ;2        
    cmp     #$01                    ;2        
    beq     Lf3d1                   ;2/3      
    iny                             ;2        
    cmp     #$25                    ;2        
    bcs     Lf3d1                   ;2/3      
    lda     ram_CE,x                ;4        
    bne     Lf3d1                   ;2/3      
    iny                             ;2   =  20
Lf3d1
    sty     ram_92,x                ;4        
    sty     ram_95,x                ;4   =   8
Lf3d5
    lda     ram_A3                  ;3        
    bne     Lf426                   ;2/3!     
    ldy     ram_D4                  ;3        
    bne     Lf40e                   ;2/3!=  10
Lf3dd
    ldy     #$19                    ;2        
    lda     #$77                    ;2   =   4
Lf3e1
    ldx     Lf662,y                 ;4        
    sta     ram_80,x                ;4        
    dey                             ;2        
    bne     Lf3e1                   ;2/3      
    sty     ram_9F                  ;3        
    ldx     #$02                    ;2   =  17
Lf3ed
    lda     ram_86,x                ;4        
    bne     Lf3fb                   ;2/3      
    lda     ram_89,x                ;4         *
    jsr     Lf271                   ;6         *
    sec                             ;2         *
    sbc     ram_8F,x                ;4         *
    bmi     Lf3fc                   ;2/3 =  24 *
Lf3fb
    tya                             ;2   =   2
Lf3fc
    sta     ram_CE,x                ;4        
    sty     ram_92,x                ;4        
    sty     ram_95,x                ;4        
    lda     Lf7e4,x                 ;4        
    and     ram_D5                  ;3        
    bne     Lf40b                   ;2/3      
    inc     ram_9F                  ;5   =  26
Lf40b
    dex                             ;2        
    bpl     Lf3ed                   ;2/3!=   4
Lf40e
    ldy     ram_D4                  ;3        
    cpy     #$08                    ;2        
    bcs     Lf417                   ;2/3      
    jmp     Lf546                   ;3   =  10
    
Lf417
    lda     ram_DC                  ;3        
    cpy     #$1d                    ;2        
    bcs     Lf424                   ;2/3      
    cpy     #$14                    ;2        
    bcs     Lf427                   ;2/3      
    jmp     Lf5c8                   ;3   =  14
    
Lf424
    beq     Lf46f                   ;2/3 =   2
Lf426
    rts                             ;6   =   6
    
Lf427
    bne     Lf435                   ;2/3      
    ldx     ram_D2                  ;3        
    stx     ram_81                  ;3        
    dec     ram_9F                  ;5        
    bmi     Lf43f                   ;2/3      
    inc     ram_D4                  ;5        
    bne     Lf451                   ;2/3 =  22
Lf435
    cmp     #$16                    ;2        
    bcc     Lf445                   ;2/3      
    ldx     #$00                    ;2        
    stx     ram_DC                  ;3        
    stx     ram_E0                  ;3   =  12
Lf43f
    lda     #$1d                    ;2        
    sta     ram_D4                  ;3        
    bne     Lf46c                   ;2/3 =   7
Lf445
    jsr     Lf641                   ;6        
    ldx     Lf663,y                 ;4        
    sta     ram_80,x                ;4        
    ldx     #$03                    ;2        
    brk                             ;7   =  23
    
    brk                             ;7   =   7 *
    
Lf451
    cmp     #$11                    ;2        
    bcs     Lf464                   ;2/3      
    ldx     #$30                    ;2        
    clc                             ;2        
    adc     ram_E0                  ;3        
    cmp     #$11                    ;2        
    bcc     Lf46c                   ;2/3      
    bne     Lf464                   ;2/3      
    ldy     ram_E4                  ;3         *
    bmi     Lf46c                   ;2/3 =  22 *
Lf464
    cmp     #$16                    ;2        
    bcs     Lf46c                   ;2/3      
    sta     ram_DC                  ;3        
    bne     Lf43f                   ;2/3 =   9
Lf46c
    stx     ram_A3                  ;3        
    rts                             ;6   =   9
    
Lf46f
    ldx     #$02                    ;2   =   2
Lf471
    lda     Lf7e4,x                 ;4        
    and     ram_D5                  ;3        
    bne     Lf4a0                   ;2/3      
    lda     ram_92,x                ;4        
    cmp     #$04                    ;2        
    bcs     Lf4a0                   ;2/3      
    lda     ram_D9,x                ;4        
    cmp     #$0c                    ;2        
    bcs     Lf486                   ;2/3      
    adc     ram_DD,x                ;4   =  29 *
Lf486
    sta     ram_C2                  ;3        
    lda     ram_DC                  ;3        
    ldy     #$08                    ;2        
    cmp     ram_C2                  ;3        
    bne     Lf498                   ;2/3      
    sty     ram_92,x                ;4         *
    sty     ram_95,x                ;4         *
    lda     ram_E4                  ;3         *
    bmi     Lf4a0                   ;2/3 =  26 *
Lf498
    dey                             ;2        
    bcs     Lf49c                   ;2/3      
    dey                             ;2   =   6
Lf49c
    tya                             ;2        
    jsr     Lf651                   ;6   =   8
Lf4a0
    dex                             ;2        
    bpl     Lf471                   ;2/3      
    lda     ram_D5                  ;3        
    sta     ram_D1                  ;3        
    inx                             ;2        
    ldy     #$14                    ;2   =  14
Lf4aa
    asl                             ;2        
    bcs     Lf4af                   ;2/3      
    sty     ram_8C,x                ;4   =   8
Lf4af
    inx                             ;2        
    cpx     #$03                    ;2        
    bcc     Lf4aa                   ;2/3      
    lda     ram_E4                  ;3        
    ora     #$40                    ;2        
    sta     ram_E4                  ;3        
    inc     ram_D4                  ;5        
    rts                             ;6   =  25
    
Lf4bd
    ldx     #$02                    ;2   =   2
Lf4bf
    sed                             ;2        
    lda     ram_F4,x                ;4        
    asl                             ;2        
    lda     ram_F7,x                ;4        
    beq     Lf51b                   ;2/3!     
    bcs     Lf4e3                   ;2/3      
    sbc     #$00                    ;2        
    sta     ram_F7,x                ;4        
    lda     ram_89,x                ;4        
    adc     #$00                    ;2        
    sta     ram_89,x                ;4        
    cld                             ;2        
    lda     ram_86,x                ;4        
    adc     #$00                    ;2        
    sta     ram_86,x                ;4        
    eor     #$0a                    ;2        
    bne     Lf519                   ;2/3!     
    lda     ram_F7,x                ;4         *
    jmp     Lf4f8                   ;3   =  55 *
    
Lf4e3
    sbc     #$01                    ;2        
    sta     ram_F7,x                ;4        
    lda     ram_89,x                ;4        
    sbc     #$01                    ;2        
    sta     ram_89,x                ;4        
    cld                             ;2        
    lda     ram_86,x                ;4        
    sbc     #$00                    ;2        
    sta     ram_86,x                ;4        
    bne     Lf519                   ;2/3!     
    lda     ram_89,x                ;4   =  34 *
Lf4f8
    bne     Lf519                   ;2/3!      *
    sta     ram_E1,x                ;4         *
    lda     #$02                    ;2         *
    sta     ram_D6,x                ;4         *
    lda     #$0b                    ;2         *
    ldy     #$bb                    ;2         *
    bcc     Lf50a                   ;2/3       *
    sta     ram_86,x                ;4         *
    sty     ram_89,x                ;4   =  26 *
Lf50a
    sty     ram_8F,x                ;4         *
    sta     ram_8C,x                ;4         *
    lda     Lf7e4,x                 ;4         *
    ora     ram_D5                  ;3         *
    sta     ram_D5                  ;3         *
    ora     ram_D1                  ;3         *
    sta     ram_D1                  ;3   =  24 *
Lf519
    ldy     #$02                    ;2   =   2
Lf51b
    dex                             ;2        
    bpl     Lf4bf                   ;2/3!     
    tya                             ;2        
    bne     Lf528                   ;2/3      
    ldx     ram_F1                  ;3        
    bne     Lf528                   ;2/3      
    iny                             ;2        
    sty     ram_E7                  ;3   =  18
Lf528
    and     ram_E7                  ;3        
    asl                             ;2        
    sta     AUDC1                   ;3        
    cld                             ;2        
    rts                             ;6   =  16
    
Lf52f
    lda     Lf663,y                 ;4        
    cmp     #$06                    ;2        
    and     #$03                    ;2        
    bcc     Lf543                   ;2/3      
    tax                             ;2        
    lda     Lf7e4,x                 ;4        
    and     ram_D5                  ;3        
    beq     Lf543                   ;2/3      
    iny                             ;2        
    bne     Lf52f                   ;2/3 =  25
Lf543
    sty     ram_D4                  ;3        
    rts                             ;6   =   9
    
Lf546
    jsr     Lf52f                   ;6        
    jsr     Lf641                   ;6        
    ldx     #$30                    ;2        
    stx     ram_A3                  ;3        
    ldx     Lf663,y                 ;4        
    sta     ram_80,x                ;4        
    dex                             ;2        
    beq     Lf559                   ;2/3      
    rts                             ;6   =  35
    
Lf559
    sta     ram_D2                  ;3        
    lda     #$75                    ;2        
    sta     ram_81                  ;3        
    txa                             ;2        
    ldx     #$07                    ;2   =  12
Lf562
    sta     ram_D9,x                ;4        
    dex                             ;2        
    bpl     Lf562                   ;2/3      
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
Lf578
    lda     ram_98,x                ;4        
    brk                             ;7   =  11
    
    brk                             ;7   =   7 *
    
    lda     ram_9C,x                ;4        
    brk                             ;7   =  11
    
    brk                             ;7   =   7 *
    
    ldy     ram_E4                  ;3        
    bpl     Lf58e                   ;2/3      
    cmp     #$0a                    ;2         *
    bcc     Lf58c                   ;2/3       *
    cmp     #$0c                    ;2         *
    bcc     Lf58e                   ;2/3 =  13 *
Lf58c
    sta     ram_CE,x                ;4   =   4 *
Lf58e
    clc                             ;2        
    adc     ram_DD,x                ;4        
    cmp     #$15                    ;2        
    bne     Lf5ac                   ;2/3      
    cmp     ram_B2                  ;3        
    beq     Lf5ac                   ;2/3      
    lda     ram_8F,x                ;4        
    jsr     Lf271                   ;6        
    clc                             ;2        
    sed                             ;2        
    adc     ram_8F,x                ;4        
    cld                             ;2        
    sta     ram_8F,x                ;4        
    lda     #$04                    ;2        
    ldy     #$02                    ;2        
    jsr     Lf64f                   ;6   =  49
Lf5ac
    dex                             ;2        
    bpl     Lf578                   ;2/3      
    lda     ram_B2                  ;3        
    cmp     #$15                    ;2        
    bne     Lf5c5                   ;2/3      
    sta     ram_DC                  ;3         *
    lda     ram_D2                  ;3         *
    sta     ram_81                  ;3         *
    lda     #$1d                    ;2         *
    sta     ram_D4                  ;3         *
    lda     #$01                    ;2         *
    sta     ram_F1                  ;3         *
    sta     ram_A3                  ;3   =  33 *
Lf5c5
    jmp     Lf624                   ;3   =   3
    
Lf5c8
    lda     Lf663,y                 ;4        
    and     #$03                    ;2        
    tax                             ;2        
    lda     ram_D1                  ;3        
    cmp     #$f0                    ;2        
    bcc     Lf640                   ;2/3!     
    sty     ram_CE,x                ;4        
    lda     ram_92,x                ;4        
    ldx     Lf663,y                 ;4        
    cmp     #$02                    ;2        
    beq     Lf61c                   ;2/3!     
    jsr     Lf641                   ;6        
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
    bne     Lf5fe                   ;2/3      
    sed                             ;2         *
    clc                             ;2         *
    lda     ram_8F,x                ;4         *
    adc     ram_8F,x                ;4         *
    sta     ram_8F,x                ;4         *
    cld                             ;2   =  31 *
Lf5fe
    lda     #$05                    ;2        
    ldy     #$01                    ;2        
    plp                             ;4        
    bcs     Lf616                   ;2/3      
    beq     Lf619                   ;2/3      
    lda     ram_E4                  ;3        
    bmi     Lf624                   ;2/3      
    lda     ram_AC,x                ;4        
    cmp     #$77                    ;2        
    beq     Lf624                   ;2/3      
    lda     #$06                    ;2         *
    ldy     #$00                    ;2         *
    clc                             ;2   =  31 *
Lf616
    jsr     Lf64f                   ;6   =   6
Lf619
    ldy     ram_D4                  ;3        
    dey                             ;2   =   5
Lf61c
    tya                             ;2        
    clc                             ;2        
    adc     #$04                    ;2        
    and     #$fc                    ;2        
    sta     ram_D4                  ;3   =  11
Lf624
    ldy     ram_D4                  ;3        
    cpy     #$14                    ;2        
    bcs     Lf637                   ;2/3      
    jsr     Lf52f                   ;6        
    bcc     Lf637                   ;2/3      
    lda     ram_92,x                ;4        
    cmp     #$04                    ;2        
    beq     Lf61c                   ;2/3      
    bne     Lf639                   ;2/3 =  25
Lf637
    ldx     #$04                    ;2   =   2
Lf639
    lda     Lf7e4,x                 ;4        
    eor     #$f9                    ;2        
    sta     ram_D1                  ;3   =   9
Lf640
    rts                             ;6   =   6
    
Lf641
    lda     #$06                    ;2        
    sta     AUDC1                   ;3        
    dec     ram_D3                  ;5        
    lda     ram_AB                  ;3        
    asl                             ;2        
    inc     ram_9B                  ;5        
    inc     ram_D4                  ;5        
    rts                             ;6   =  31
    
Lf64f
    sty     ram_F1                  ;3   =   3
Lf651
    sta     ram_92,x                ;4        
    sta     ram_95,x                ;4        
    ror                             ;2        
    sta     ram_F4,x                ;4        
    lda     ram_8F,x                ;4        
    sta     ram_F7,x                ;4        
    lda     #$6c                    ;2        
    sta     ram_E7                  ;3        
    dec     ram_9F                  ;5   =  32
Lf662
    rts                             ;6   =   6
    
Lf663
    .byte   $1a,$19,$18,$00,$1e,$1d,$1c,$01 ; $f663 (D)
    .byte   $22,$26,$2a,$2e,$21,$25,$29,$2d ; $f66b (D)
    .byte   $20,$24,$28,$2c,$01,$02,$03,$04 ; $f673 (D)
    .byte   $05                             ; $f67b (D)
    .byte   $00,$01,$02,$03                 ; $f67c (*)
    
    .byte   %00000000 ; |        |            $f680 (G)
    .byte   %00100010 ; |  #   # |            $f681 (G)
    .byte   %00100010 ; |  #   # |            $f682 (G)
    .byte   %00100010 ; |  #   # |            $f683 (G)
    .byte   %00111110 ; |  ##### |            $f684 (G)
    .byte   %00100010 ; |  #   # |            $f685 (G)
    .byte   %00100010 ; |  #   # |            $f686 (G)
    .byte   %00011100 ; |   ###  |            $f687 (G)
    .byte   %00000000 ; |        |            $f688 (G)
    .byte   %00111110 ; |  ##### |            $f689 (G)
    .byte   %00100000 ; |  #     |            $f68a (G)
    .byte   %00100000 ; |  #     |            $f68b (G)
    .byte   %00111110 ; |  ##### |            $f68c (G)
    .byte   %00000010 ; |      # |            $f68d (G)
    .byte   %00000010 ; |      # |            $f68e (G)
    .byte   %00111110 ; |  ##### |            $f68f (G)
    .byte   %00000000 ; |        |            $f690 (G)
    .byte   %00111110 ; |  ##### |            $f691 (G)
    .byte   %00000010 ; |      # |            $f692 (G)
    .byte   %00000010 ; |      # |            $f693 (G)
    .byte   %00001110 ; |    ### |            $f694 (G)
    .byte   %00000010 ; |      # |            $f695 (G)
    .byte   %00000010 ; |      # |            $f696 (G)
    .byte   %00111110 ; |  ##### |            $f697 (G)
    .byte   %00000000 ; |        |            $f698 (G)
    .byte   %00000100 ; |     #  |            $f699 (G)
    .byte   %00000100 ; |     #  |            $f69a (G)
    .byte   %00000100 ; |     #  |            $f69b (G)
    .byte   %00111110 ; |  ##### |            $f69c (G)
    .byte   %00100100 ; |  #  #  |            $f69d (G)
    .byte   %00100100 ; |  #  #  |            $f69e (G)
    .byte   %00100000 ; |  #     |            $f69f (G)
    .byte   %00000000 ; |        |            $f6a0 (G)
    .byte   %00111110 ; |  ##### |            $f6a1 (G)
    .byte   %00000010 ; |      # |            $f6a2 (G)
    .byte   %00000010 ; |      # |            $f6a3 (G)
    .byte   %00111110 ; |  ##### |            $f6a4 (G)
    .byte   %00100000 ; |  #     |            $f6a5 (G)
    .byte   %00100000 ; |  #     |            $f6a6 (G)
    .byte   %00111110 ; |  ##### |            $f6a7 (G)
    .byte   %00000000 ; |        |            $f6a8 (G)
    .byte   %00111110 ; |  ##### |            $f6a9 (G)
    .byte   %00100010 ; |  #   # |            $f6aa (G)
    .byte   %00100010 ; |  #   # |            $f6ab (G)
    .byte   %00111110 ; |  ##### |            $f6ac (G)
    .byte   %00100000 ; |  #     |            $f6ad (G)
    .byte   %00100000 ; |  #     |            $f6ae (G)
    .byte   %00111110 ; |  ##### |            $f6af (G)
    .byte   %00000000 ; |        |            $f6b0 (G)
    .byte   %00000010 ; |      # |            $f6b1 (G)
    .byte   %00000010 ; |      # |            $f6b2 (G)
    .byte   %00000010 ; |      # |            $f6b3 (G)
    .byte   %00000010 ; |      # |            $f6b4 (G)
    .byte   %00000010 ; |      # |            $f6b5 (G)
    .byte   %00000010 ; |      # |            $f6b6 (G)
    .byte   %00111110 ; |  ##### |            $f6b7 (G)
    .byte   %00000000 ; |        |            $f6b8 (G)
    .byte   %00111110 ; |  ##### |            $f6b9 (G)
    .byte   %00100010 ; |  #   # |            $f6ba (G)
    .byte   %00100010 ; |  #   # |            $f6bb (G)
    .byte   %00111110 ; |  ##### |            $f6bc (G)
    .byte   %00100010 ; |  #   # |            $f6bd (G)
    .byte   %00100010 ; |  #   # |            $f6be (G)
    .byte   %00111110 ; |  ##### |            $f6bf (G)
    .byte   %00000000 ; |        |            $f6c0 (G)
    .byte   %00111110 ; |  ##### |            $f6c1 (G)
    .byte   %00000010 ; |      # |            $f6c2 (G)
    .byte   %00000010 ; |      # |            $f6c3 (G)
    .byte   %00111110 ; |  ##### |            $f6c4 (G)
    .byte   %00100010 ; |  #   # |            $f6c5 (G)
    .byte   %00100010 ; |  #   # |            $f6c6 (G)
    .byte   %00111110 ; |  ##### |            $f6c7 (G)
    .byte   %00000000 ; |        |            $f6c8 (G)
    .byte   %00101110 ; |  # ### |            $f6c9 (G)
    .byte   %00101010 ; |  # # # |            $f6ca (G)
    .byte   %00101010 ; |  # # # |            $f6cb (G)
    .byte   %00101010 ; |  # # # |            $f6cc (G)
    .byte   %00101010 ; |  # # # |            $f6cd (G)
    .byte   %00101010 ; |  # # # |            $f6ce (G)
    .byte   %00101110 ; |  # ### |            $f6cf (G)
    .byte   %00000000 ; |        |            $f6d0 (G)
    .byte   %00111100 ; |  ####  |            $f6d1 (G)
    .byte   %00100100 ; |  #  #  |            $f6d2 (G)
    .byte   %00000100 ; |     #  |            $f6d3 (G)
    .byte   %00000100 ; |     #  |            $f6d4 (G)
    .byte   %00000100 ; |     #  |            $f6d5 (G)
    .byte   %00000100 ; |     #  |            $f6d6 (G)
    .byte   %00001110 ; |    ### |            $f6d7 (G)
    .byte   %00000000 ; |        |            $f6d8 (G)
    .byte   %00000010 ; |      # |            $f6d9 (G)
    .byte   %00111100 ; |  ####  |            $f6da (G)
    .byte   %00101100 ; |  # ##  |            $f6db (G)
    .byte   %00100100 ; |  #  #  |            $f6dc (G)
    .byte   %00100100 ; |  #  #  |            $f6dd (G)
    .byte   %00100100 ; |  #  #  |            $f6de (G)
    .byte   %00111100 ; |  ####  |            $f6df (G)
    .byte   %00000000 ; |        |            $f6e0 (G)
    .byte   %00100010 ; |  #   # |            $f6e1 (G)
    .byte   %00100100 ; |  #  #  |            $f6e2 (G)
    .byte   %00101000 ; |  # #   |            $f6e3 (G)
    .byte   %00110000 ; |  ##    |            $f6e4 (G)
    .byte   %00101000 ; |  # #   |            $f6e5 (G)
    .byte   %00100100 ; |  #  #  |            $f6e6 (G)
    .byte   %00100010 ; |  #   # |            $f6e7 (G)
Lf6e8
    .byte   %00000000 ; |        |            $f6e8 (G)
    
    .byte   $60,$10,$21                     ; $f6e9 (D)
    
    .byte   %11111111 ; |########|            $f6ec (G)
    .byte   %00111111 ; |  ######|            $f6ed (G)
    .byte   %01111111 ; | #######|            $f6ee (G)
    .byte   %00111111 ; |  ######|            $f6ef (G)
    .byte   %11000111 ; |##   ###|            $f6f0 (G)
    .byte   %11010101 ; |## # # #|            $f6f1 (G)
    .byte   %11010101 ; |## # # #|            $f6f2 (G)
    .byte   %11111000 ; |#####   |            $f6f3 (G)
    .byte   %11111111 ; |########|            $f6f4 (G)
    .byte   %11111111 ; |########|            $f6f5 (G)
    .byte   %11111111 ; |########|            $f6f6 (G)
    .byte   %11111111 ; |########|            $f6f7 (G)
    .byte   %11111111 ; |########|            $f6f8 (G)
    .byte   %11111111 ; |########|            $f6f9 (G)
    .byte   %11111111 ; |########|            $f6fa (G)
    .byte   %11111111 ; |########|            $f6fb (G)
    .byte   %11111111 ; |########|            $f6fc (G)
    
Lf6fd
    .byte   $80                             ; $f6fd (D)
    .byte   $40,$08                         ; $f6fe (*)
    
    .byte   %01110000 ; | ###    |            $f700 (G)
    .byte   %01010000 ; | # #    |            $f701 (G)
    .byte   %01010000 ; | # #    |            $f702 (G)
    .byte   %01010000 ; | # #    |            $f703 (G)
    .byte   %01110000 ; | ###    |            $f704 (G)
    .byte   %00100000 ; |  #     |            $f705 (G)
    .byte   %00100000 ; |  #     |            $f706 (G)
    .byte   %00100000 ; |  #     |            $f707 (G)
    .byte   %00100000 ; |  #     |            $f708 (G)
    .byte   %00100000 ; |  #     |            $f709 (G)
    .byte   %01110000 ; | ###    |            $f70a (G)
    .byte   %01000000 ; | #      |            $f70b (G)
    .byte   %01110000 ; | ###    |            $f70c (G)
    .byte   %00010000 ; |   #    |            $f70d (G)
    .byte   %01110000 ; | ###    |            $f70e (G)
    .byte   %01110000 ; | ###    |            $f70f (G)
    .byte   %00010000 ; |   #    |            $f710 (G)
    .byte   %00110000 ; |  ##    |            $f711 (G)
    .byte   %00010000 ; |   #    |            $f712 (G)
    .byte   %01110000 ; | ###    |            $f713 (G)
    .byte   %00010000 ; |   #    |            $f714 (G)
    .byte   %00010000 ; |   #    |            $f715 (G)
    .byte   %01110000 ; | ###    |            $f716 (G)
    .byte   %01010000 ; | # #    |            $f717 (G)
    .byte   %01000000 ; | #      |            $f718 (G)
    .byte   %01110000 ; | ###    |            $f719 (G)
    .byte   %00010000 ; |   #    |            $f71a (G)
    .byte   %01110000 ; | ###    |            $f71b (G)
    .byte   %01000000 ; | #      |            $f71c (G)
    .byte   %01110000 ; | ###    |            $f71d (G)
    .byte   %01110000 ; | ###    |            $f71e (G)
    .byte   %01010000 ; | # #    |            $f71f (G)
    .byte   %01110000 ; | ###    |            $f720 (G)
    .byte   %01000000 ; | #      |            $f721 (G)
    .byte   %01110000 ; | ###    |            $f722 (G)
    .byte   %00010000 ; |   #    |            $f723 (G)
    .byte   %00010000 ; |   #    |            $f724 (G)
    .byte   %00010000 ; |   #    |            $f725 (G)
    .byte   %00010000 ; |   #    |            $f726 (G)
    .byte   %01110000 ; | ###    |            $f727 (G)
    .byte   %01110000 ; | ###    |            $f728 (G)
    .byte   %01010000 ; | # #    |            $f729 (G)
    .byte   %01110000 ; | ###    |            $f72a (G)
    .byte   %01010000 ; | # #    |            $f72b (G)
    .byte   %01110000 ; | ###    |            $f72c (G)
    .byte   %01110000 ; | ###    |            $f72d (G)
    .byte   %00010000 ; |   #    |            $f72e (G)
    .byte   %01110000 ; | ###    |            $f72f (G)
    .byte   %01010000 ; | # #    |            $f730 (G)
    .byte   %01110000 ; | ###    |            $f731 (G)
    .byte   %00010111 ; |   # ###|            $f732 (G)
    .byte   %00010101 ; |   # # #|            $f733 (G)
    .byte   %00010101 ; |   # # #|            $f734 (G)
    .byte   %00010101 ; |   # # #|            $f735 (G)
    .byte   %00010111 ; |   # ###|            $f736 (G)
    .byte   %00000000 ; |        |            $f737 (G)
    .byte   %00000000 ; |        |            $f738 (G)
    .byte   %00000000 ; |        |            $f739 (G)
    .byte   %00000000 ; |        |            $f73a (G)
    .byte   %00000000 ; |        |            $f73b (G)
    .byte   %10010100 ; |#  # #  |            $f73c (G)
    .byte   %10010100 ; |#  # #  |            $f73d (G)
    .byte   %11110100 ; |#### #  |            $f73e (G)
    .byte   %10010100 ; |#  # #  |            $f73f (G)
    .byte   %10010101 ; |#  # # #|            $f740 (G)
    .byte   %11100100 ; |###  #  |            $f741 (G)
    .byte   %00100100 ; |  #  #  |            $f742 (G)
    .byte   %11100100 ; |###  #  |            $f743 (G)
    .byte   %10000100 ; |#    #  |            $f744 (G)
    .byte   %11101110 ; |### ### |            $f745 (G)
    .byte   %11101110 ; |### ### |            $f746 (G)
    .byte   %10101010 ; |# # # # |            $f747 (G)
    .byte   %10101110 ; |# # ### |            $f748 (G)
    .byte   %10101010 ; |# # # # |            $f749 (G)
    .byte   %11101110 ; |### ### |            $f74a (G)
    .byte   %00000111 ; |     ###|            $f74b (G)
    .byte   %00000101 ; |     # #|            $f74c (G)
    .byte   %00000111 ; |     ###|            $f74d (G)
    .byte   %00000101 ; |     # #|            $f74e (G)
    .byte   %00000111 ; |     ###|            $f74f (G)
    .byte   %01110111 ; | ### ###|            $f750 (G)
    .byte   %01010101 ; | # # # #|            $f751 (G)
    .byte   %01110101 ; | ### # #|            $f752 (G)
    .byte   %01010101 ; | # # # #|            $f753 (G)
    .byte   %01110101 ; | ### # #|            $f754 (G)
    .byte   %01101101 ; | ## ## #|            $f755 (G)
    .byte   %01010101 ; | # # # #|            $f756 (G)
    .byte   %01000101 ; | #   # #|            $f757 (G)
    .byte   %01000101 ; | #   # #|            $f758 (G)
    .byte   %01000101 ; | #   # #|            $f759 (G)
    .byte   %01110111 ; | ### ###|            $f75a (G)
    .byte   %01000101 ; | #   # #|            $f75b (G)
    .byte   %01000101 ; | #   # #|            $f75c (G)
    .byte   %01000101 ; | #   # #|            $f75d (G)
    .byte   %01000111 ; | #   ###|            $f75e (G)
    .byte   %00100101 ; |  #  # #|            $f75f (G)
    .byte   %00100101 ; |  #  # #|            $f760 (G)
    .byte   %00100101 ; |  #  # #|            $f761 (G)
    .byte   %00100101 ; |  #  # #|            $f762 (G)
    .byte   %01110101 ; | ### # #|            $f763 (G)
    .byte   %00100000 ; |  #     |            $f764 (G)
    .byte   %00000000 ; |        |            $f765 (G)
    .byte   %00110000 ; |  ##    |            $f766 (G)
    .byte   %00010000 ; |   #    |            $f767 (G)
    .byte   %01110000 ; | ###    |            $f768 (G)
    
Break
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    cmp     #$0a                    ;2        
    bcc     Lf772                   ;2/3      
    lda     #$09                    ;2   =  12
Lf772
    cmp     #$00                    ;2        
    bne     Lf77a                   ;2/3      
    ldy     #$0a                    ;2        
    sty     ram_DD,x                ;4   =  10
Lf77a
    sec                             ;2        
    adc     ram_D9,x                ;4        
    sta     ram_D9,x                ;4        
    rti                             ;6   =  16
    
    .byte   %00000111 ; |     ###|            $f780 (G)
    .byte   %00000101 ; |     # #|            $f781 (G)
    .byte   %00000101 ; |     # #|            $f782 (G)
    .byte   %00000101 ; |     # #|            $f783 (G)
    .byte   %00000111 ; |     ###|            $f784 (G)
    .byte   %00000010 ; |      # |            $f785 (G)
    .byte   %00000010 ; |      # |            $f786 (G)
    .byte   %00000010 ; |      # |            $f787 (G)
    .byte   %00000010 ; |      # |            $f788 (G)
    .byte   %00000010 ; |      # |            $f789 (G)
    .byte   %00000111 ; |     ###|            $f78a (G)
    .byte   %00000100 ; |     #  |            $f78b (G)
    .byte   %00000111 ; |     ###|            $f78c (G)
    .byte   %00000001 ; |       #|            $f78d (G)
    .byte   %00000111 ; |     ###|            $f78e (G)
    .byte   %00000111 ; |     ###|            $f78f (G)
    .byte   %00000001 ; |       #|            $f790 (G)
    .byte   %00000011 ; |      ##|            $f791 (G)
    .byte   %00000001 ; |       #|            $f792 (G)
    .byte   %00000111 ; |     ###|            $f793 (G)
    .byte   %00000001 ; |       #|            $f794 (G)
    .byte   %00000001 ; |       #|            $f795 (G)
    .byte   %00000111 ; |     ###|            $f796 (G)
    .byte   %00000101 ; |     # #|            $f797 (G)
    .byte   %00000100 ; |     #  |            $f798 (G)
    .byte   %00000111 ; |     ###|            $f799 (G)
    .byte   %00000001 ; |       #|            $f79a (G)
    .byte   %00000111 ; |     ###|            $f79b (G)
    .byte   %00000100 ; |     #  |            $f79c (G)
    .byte   %00000111 ; |     ###|            $f79d (G)
    .byte   %00000111 ; |     ###|            $f79e (G)
    .byte   %00000101 ; |     # #|            $f79f (G)
    .byte   %00000111 ; |     ###|            $f7a0 (G)
    .byte   %00000100 ; |     #  |            $f7a1 (G)
    .byte   %00000111 ; |     ###|            $f7a2 (G)
    .byte   %00000001 ; |       #|            $f7a3 (G)
    .byte   %00000001 ; |       #|            $f7a4 (G)
    .byte   %00000001 ; |       #|            $f7a5 (G)
    .byte   %00000001 ; |       #|            $f7a6 (G)
    .byte   %00000111 ; |     ###|            $f7a7 (G)
    .byte   %00000111 ; |     ###|            $f7a8 (G)
    .byte   %00000101 ; |     # #|            $f7a9 (G)
    .byte   %00000111 ; |     ###|            $f7aa (G)
    .byte   %00000101 ; |     # #|            $f7ab (G)
    .byte   %00000111 ; |     ###|            $f7ac (G)
    .byte   %00000111 ; |     ###|            $f7ad (G)
    .byte   %00000001 ; |       #|            $f7ae (G)
    .byte   %00000111 ; |     ###|            $f7af (G)
    .byte   %00000101 ; |     # #|            $f7b0 (G)
Lf7b1
    .byte   %00000111 ; |     ###|            $f7b1 (G)
    
    .byte   $3e                             ; $f7b2 (A)
    .byte   $31                             ; $f7b3 (A)
    .byte   $f0                             ; $f7b4 (A)
    .byte   $fe                             ; $f7b5 (A)
    .byte   $51                             ; $f7b6 (A)

    .byte   %00000000 ; |        |            $f7b7 (G)
    .byte   %00000000 ; |        |            $f7b8 (G)
    .byte   %00000000 ; |        |            $f7b9 (G)
    .byte   %00000000 ; |        |            $f7ba (G)
    .byte   %00000000 ; |        |            $f7bb (G)
    .byte   %10000000 ; |#       |            $f7bc (G)
    .byte   %10000000 ; |#       |            $f7bd (G)
    .byte   %10000000 ; |#       |            $f7be (G)
    .byte   %10000000 ; |#       |            $f7bf (G)
    .byte   %11000000 ; |##      |            $f7c0 (G)
    .byte   %10100100 ; |# #  #  |            $f7c1 (G)
    .byte   %10100100 ; |# #  #  |            $f7c2 (G)
    .byte   %11100100 ; |###  #  |            $f7c3 (G)
    .byte   %10101110 ; |# # ### |            $f7c4 (G)
    .byte   %11101010 ; |### # # |            $f7c5 (G)
    .byte   %11101110 ; |### ### |            $f7c6 (G)
    .byte   %10001000 ; |#   #   |            $f7c7 (G)
    .byte   %10001100 ; |#   ##  |            $f7c8 (G)
    .byte   %10001000 ; |#   #   |            $f7c9 (G)
    .byte   %10001110 ; |#   ### |            $f7ca (G)
    .byte   %01110000 ; | ###    |            $f7cb (G)
    .byte   %01010000 ; | # #    |            $f7cc (G)
    .byte   %00010000 ; |   #    |            $f7cd (G)
    .byte   %00010000 ; |   #    |            $f7ce (G)
    .byte   %00111000 ; |  ###   |            $f7cf (G)
    .byte   %01110010 ; | ###  # |            $f7d0 (G)
    .byte   %00010010 ; |   #  # |            $f7d1 (G)
    .byte   %01110010 ; | ###  # |            $f7d2 (G)
    .byte   %01000010 ; | #    # |            $f7d3 (G)
    .byte   %01110111 ; | ### ###|            $f7d4 (G)
    .byte   %01000100 ; | #   #  |            $f7d5 (G)
    .byte   %01001100 ; | #  ##  |            $f7d6 (G)
    .byte   %01010100 ; | # # #  |            $f7d7 (G)
    .byte   %01100100 ; | ##  #  |            $f7d8 (G)
    .byte   %01000100 ; | #   #  |            $f7d9 (G)
    .byte   %01110111 ; | ### ###|            $f7da (G)
    .byte   %00010100 ; |   # #  |            $f7db (G)
    .byte   %01110110 ; | ### ## |            $f7dc (G)
    .byte   %01000100 ; | #   #  |            $f7dd (G)
    .byte   %01110111 ; | ### ###|            $f7de (G)
 
    .byte   $c0,$00,$80,$00,$c0             ; $f7df (*)
Lf7e4
    .byte   $80,$40,$20,$10,$08,$04,$02,$01 ; $f7e4 (D)
Lf7ec
    .byte   $08,$0e,$00,$06                 ; $f7ec (*)
    .byte   $36,$0c,$d0                     ; $f7f0 (D)
Lf7f3
    .byte   $d6,$50,$77,$ff,$1e,$50,$02,$02 ; $f7f3 (D)
    .byte   $02,$00,$f0,$69                 ; $f7fb (D)
    .byte   $f7                             ; $f7ff (*)
