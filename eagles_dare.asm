;
; EAGLES DARE
;

; Code and graphics by T.M.R/Cosine
; Music by aNdy/Cosine


; This source code is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer which can be downloaded at
; https://csdb.dk/release/?id=167084

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "eagles_dare.prg",cbm


; Pull in some binary data
		* = $0400
		!binary "data/eagle_col.raw",$3c0

		* = $1500
music		!binary "data/smaciwch_fy_ffolennau.sid",,$7e

		* = $2000
		!binary "data/eagle_bmp.raw",$1e00

		* = $3e00
		!binary "data/copyright.spr"


; Constants
rstr1p	= $00
rstr2p	= $f1


; Label assignments
rn		= $50
irq_store_1	= $51

scrl_x		= $52
scrl_char_col	= $53
scrl_ebcm_col	= $54
scrl_speed	= $55

font_select	= $56

bar_pos_1	= $57
bar_pos_2	= $58
bar_pos_3	= $59
bar_pos_tmr	= $5a

cos_at_1	= $5b
cos_speed_1	= $5c
cos_at_2	= $5d
cos_speed_2	= $5e
cos_at_3	= $5f
cos_speed_3	= $60

scrl_font	= $0800


; Entry point at $0940
		* = $0940
entry		sei

		lda #$35
		sta $01

; Set NMI and IRQ
		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

; Raster mode on
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

; Set video registers
		lda #rstr1p
		sta $d012

		lda #$5b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Nuke the zeropage and initialise some labels
		ldx #$20
		lda #$00
zp_nuke		sta $00,x
		inx
		bne zp_nuke

		lda #$01
		sta rn

		lda #$03
		sta scrl_speed

		lda #$00
		sta cos_at_1
		lda #$02
		sta cos_speed_1

		lda #$3b
		sta cos_at_2
		lda #$03
		sta cos_speed_2

		lda #$b4
		sta cos_at_3
		lda #$ff
		sta cos_speed_3

; Colour RAM clear
		ldx #$00
		txa
screen_clr	sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dae8,x
		inx
		bne screen_clr

; Scrub the scroller's work space
		ldx #$00
		lda #$ff
font_clr	sta scrl_font+$000,x
		sta scrl_font+$040,x
		inx
		bne font_clr

; Generate the scroll line
		ldx #$00
scrl_gen	txa
		clc
		adc #$01
		sta scrl_line,x

		lda #$00
		sta scrl_ctrl_line,x
		sta scrl_col_line,x

		inx
		cpx #$27
		bne scrl_gen

; Set up the music
		lda #$00
		jsr music+$00

		cli


; Check to see if space has been pressed and exit
main_loop	lda $dc01
		cmp #$ef
		beq *+$05
		jmp main_loop

; Zero the volume and then reset the machine
		sei
		lda #$37
		sta $01

		lda #$00
		sta $d418
		lda #$0b
		sta $d011
		sta $d020

		jmp $fce2


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn

		cmp #$02
		bne *+$05
		jmp rout2


; Raster split 1
rout1

; Update some video registers
		lda #$0b
		sta $d020

; Set video registers for the picture
		lda #$3b
		sta $d011
		lda #$07
		sta $d016
		lda #$18
		sta $d018

; Position the copyright symbol sprites
		lda #$ff
		sta $d015

		ldx #$00
		ldy #$00
set_sprites_1	lda sprite_x_pos_1,x
		sta $d000,y
		lda sprite_y_pos_1,x
		sta $d001,y
		lda sprite_cols_1,x
		sta $d027,x
		lda sprite_dps,x
		sta $07f8,x
		iny
		iny
		inx
		cpx #$08
		bne set_sprites_1

		lda sprite_x_msb_1
		sta $d010

; Play the music
		jsr music+$03

; Wait for end of first sprite row, then move it
		lda sprite_y_pos_1+$00
		clc
		adc #$18
		cmp $d012
		bcs *-$03

		lda sprite_y_pos_2+$00
		sta $d001
		sta $d003
		sta $d005

		lda sprite_x_pos_2+$00
		sta $d000
		lda sprite_x_pos_2+$01
		sta $d002
		lda sprite_x_pos_2+$02
		sta $d004

		lda sprite_cols_2+$00
		sta $d027
		lda sprite_cols_2+$01
		sta $d028
		lda sprite_cols_2+$02
		sta $d029

		lda $d010
		and #%11111000
		sta irq_store_1

		lda sprite_x_msb_2
		and #%00000111
		ora irq_store_1
		sta $d010

; Wait for end of second sprite row, then move it
		lda sprite_y_pos_1+$03
		clc
		adc #$18
		cmp $d012
		bcs *-$03

		lda sprite_y_pos_2+$03
		sta $d007
		sta $d009

		lda sprite_x_pos_2+$03
		sta $d006
		lda sprite_x_pos_2+$04
		sta $d008

		lda sprite_cols_2+$03
		sta $d02a
		lda sprite_cols_2+$04
		sta $d02b

		lda $d010
		and #%11100111
		sta irq_store_1

		lda sprite_x_msb_2
		and #%00011000
		ora irq_store_1
		sta $d010

; Wait for end of first sprite row, then move it
		lda sprite_y_pos_1+$05
		clc
		adc #$18
		cmp $d012
		bcs *-$03

		lda sprite_y_pos_2+$05
		sta $d00b
		sta $d00d
		sta $d00f

		lda sprite_x_pos_2+$05
		sta $d00a
		lda sprite_x_pos_2+$06
		sta $d00c
		lda sprite_x_pos_2+$07
		sta $d00e

		lda sprite_cols_2+$05
		sta $d02c
		lda sprite_cols_2+$06
		sta $d02d
		lda sprite_cols_2+$07
		sta $d02e

		lda $d010
		and #%00011111
		sta irq_store_1

		lda sprite_x_msb_2
		and #%11100000
		ora irq_store_1
		sta $d010

; Set up for the second split
		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

; Exit the IRQ
		jmp ea31


; Raster split 2
rout2		ldx #$08
		dex
		bne *-$01

; Scroller splits - line $00
ebcm_2_ln_00	lda #$00
ebcm_3_ln_00	ldx #$00
ebcm_4_ln_00	ldy #$00

		sta $d022
		stx $d023
		sty $d024

ebcm_1_ln_00	lda #$02
		sta $d021

; Set video registers for the scroller
		ldx #$5b
		lda scrl_x
		eor #$07
		ldy #$12
		stx $d011
		sty $d018
		sta $d016

; Scroller splits - line $01
ebcm_2_ln_01	lda #$00
ebcm_3_ln_01	ldx #$00
ebcm_4_ln_01	ldy #$00

		sta $d022
		stx $d023
		sty $d024

ebcm_1_ln_01	lda #$08
		sta $d021

		ldx #$07
		dex
		bne *-$01
		bit $ea

; Scroller splits - line $02
ebcm_2_ln_02	lda #$00
ebcm_3_ln_02	ldx #$00
ebcm_4_ln_02	ldy #$00

		sta $d022
		stx $d023
		sty $d024

ebcm_1_ln_02	lda #$07
		sta $d021

		ldx #$07
		dex
		bne *-$01
		bit $ea

; Scroller splits - line $03
ebcm_2_ln_03	lda #$00
ebcm_3_ln_03	ldx #$00
ebcm_4_ln_03	ldy #$00

		sta $d022
		stx $d023
		sty $d024

ebcm_1_ln_03	lda #$0d
		sta $d021

		ldx #$07
		dex
		bne *-$01
		bit $ea

; Scroller splits - line $04
ebcm_2_ln_04	lda #$00
ebcm_3_ln_04	ldx #$00
ebcm_4_ln_04	ldy #$00

		sta $d022
		stx $d023
		sty $d024

ebcm_1_ln_04	lda #$03
		sta $d021

		ldx #$07
		dex
		bne *-$01
		bit $ea

; Scroller splits - line $05
ebcm_2_ln_05	lda #$00
ebcm_3_ln_05	ldx #$00
ebcm_4_ln_05	ldy #$00

		sta $d022
		stx $d023
		sty $d024

ebcm_1_ln_05	lda #$05
		sta $d021

		ldx #$07
		dex
		bne *-$01
		bit $ea

; Scroller splits - line $06
ebcm_2_ln_06	lda #$00
ebcm_3_ln_06	ldx #$00
ebcm_4_ln_06	ldy #$00

		sta $d022
		stx $d023
		sty $d024

ebcm_1_ln_06	lda #$0e
		sta $d021

		ldx #$07
		dex
		bne *-$01
		bit $ea

; Scroller splits - line $07
ebcm_2_ln_07	lda #$00
ebcm_3_ln_07	ldx #$00
ebcm_4_ln_07	ldy #$00

		sta $d022
		stx $d023
		sty $d024

ebcm_1_ln_07	lda #$04
		sta $d021

		ldx #$08
		dex
		bne *-$01
		bit $ea

		lda #$00
		sta $d021

		jmp code_skip


; Sprite cosinus tables
sprite_x_cos	!byte $ec,$ec,$ec,$ec,$ec,$ec,$eb,$eb
		!byte $ea,$ea,$e9,$e8,$e7,$e7,$e6,$e5
		!byte $e3,$e2,$e1,$e0,$de,$dd,$dc,$da
		!byte $d8,$d7,$d5,$d3,$d2,$d0,$ce,$cc
		!byte $ca,$c8,$c5,$c3,$c1,$bf,$bc,$ba
		!byte $b8,$b5,$b3,$b0,$ae,$ab,$a9,$a6
		!byte $a3,$a0,$9e,$9b,$98,$95,$93,$90
		!byte $8d,$8a,$87,$84,$81,$7f,$7c,$79

		!byte $76,$73,$70,$6d,$6a,$67,$64,$62
		!byte $5f,$5c,$59,$56,$53,$51,$4e,$4b
		!byte $48,$46,$43,$40,$3e,$3b,$39,$36
		!byte $34,$32,$2f,$2d,$2b,$28,$26,$24
		!byte $22,$20,$1e,$1c,$1a,$18,$17,$15
		!byte $13,$12,$10,$0f,$0d,$0c,$0b,$0a
		!byte $08,$07,$06,$05,$04,$04,$03,$02
		!byte $02,$01,$01,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$01,$01
		!byte $02,$02,$03,$04,$05,$06,$07,$08
		!byte $09,$0a,$0b,$0c,$0e,$0f,$11,$12
		!byte $14,$15,$17,$19,$1b,$1d,$1f,$21
		!byte $23,$25,$27,$29,$2b,$2e,$30,$32
		!byte $35,$37,$3a,$3c,$3f,$41,$44,$47
		!byte $49,$4c,$4f,$51,$54,$57,$5a,$5d
		!byte $5f,$62,$65,$68,$6b,$6e,$71,$74

		!byte $77,$7a,$7c,$7f,$82,$85,$88,$8b
		!byte $8e,$91,$93,$96,$99,$9c,$9f,$a1
		!byte $a4,$a7,$a9,$ac,$ae,$b1,$b4,$b6
		!byte $b8,$bb,$bd,$bf,$c2,$c4,$c6,$c8
		!byte $ca,$cc,$ce,$d0,$d2,$d4,$d6,$d7
		!byte $d9,$db,$dc,$dd,$df,$e0,$e1,$e3
		!byte $e4,$e5,$e6,$e7,$e8,$e8,$e9,$ea
		!byte $ea,$eb,$eb,$ec,$ec,$ec,$ec,$ec

sprite_y_cos	!byte $78,$78,$78,$78,$78,$78,$78,$78
		!byte $78,$78,$78,$77,$77,$77,$77,$76
		!byte $76,$76,$75,$75,$75,$74,$74,$74
		!byte $73,$73,$72,$72,$71,$71,$70,$70
		!byte $6f,$6f,$6e,$6e,$6d,$6c,$6c,$6b
		!byte $6a,$6a,$69,$68,$68,$67,$66,$66
		!byte $65,$64,$64,$63,$62,$61,$61,$60
		!byte $5f,$5e,$5e,$5d,$5c,$5b,$5a,$5a

		!byte $59,$58,$57,$57,$56,$55,$54,$54
		!byte $53,$52,$51,$51,$50,$4f,$4e,$4e
		!byte $4d,$4c,$4b,$4b,$4a,$49,$49,$48
		!byte $47,$47,$46,$46,$45,$44,$44,$43
		!byte $43,$42,$42,$41,$41,$40,$40,$3f
		!byte $3f,$3e,$3e,$3e,$3d,$3d,$3c,$3c
		!byte $3c,$3c,$3b,$3b,$3b,$3b,$3a,$3a
		!byte $3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a

		!byte $3a,$3a,$3a,$3a,$3a,$3a,$3a,$3a
		!byte $3a,$3a,$3a,$3b,$3b,$3b,$3b,$3c
		!byte $3c,$3c,$3d,$3d,$3d,$3e,$3e,$3e
		!byte $3f,$3f,$40,$40,$41,$41,$42,$42
		!byte $43,$43,$44,$45,$45,$46,$46,$47
		!byte $48,$48,$49,$4a,$4a,$4b,$4c,$4c
		!byte $4d,$4e,$4f,$4f,$50,$51,$52,$52
		!byte $53,$54,$55,$55,$56,$57,$58,$58

		!byte $59,$5a,$5b,$5b,$5c,$5d,$5e,$5f
		!byte $5f,$60,$61,$62,$62,$63,$64,$65
		!byte $65,$66,$67,$67,$68,$69,$69,$6a
		!byte $6b,$6b,$6c,$6d,$6d,$6e,$6e,$6f
		!byte $6f,$70,$70,$71,$71,$72,$72,$73
		!byte $73,$74,$74,$75,$75,$75,$76,$76
		!byte $76,$76,$77,$77,$77,$77,$78,$78
		!byte $78,$78,$78,$78,$78,$78,$78,$78


; Labels to make the scroll controls "easier" to read
ebcm_0		= $80	; static rainbow bar
ebcm_1		= $81	; grey bar rolling out
ebcm_2		= $82	; blue bar rolling up
ebcm_3		= $83	; brown bar rolling down

char_black	= $90
char_white	= $91
char_dk_red	= $92
char_cyan	= $93
char_purple	= $94
char_dk_green	= $95
char_dk_blue	= $96
char_yellow	= $97
char_orange	= $98
char_brown	= $99
char_lt_red	= $9a
char_dk_grey	= $9b
char_med_grey	= $9c
char_lt_green	= $9d
char_lt_blue	= $9e
char_lt_grey	= $9f

speed_1		= $a1
speed_2		= $a2
speed_3		= $a3
speed_4		= $a4
speed_5		= $a5
speed_6		= $a6
speed_7		= $a7

uc_norm		= $ff
lc_norm		= $fe
uc_inv		= $fd
lc_inv		= $fc

; Woohoo!
scrl_text	!byte lc_norm,ebcm_0,char_black,speed_3
		!scr "Welcome to  "

		!byte ebcm_1,char_brown,speed_1,uc_inv
		!scr char_dk_red,"  S "
		!scr char_black,"eagles dare"
		!scr char_dk_red," S  "

		!byte ebcm_0,lc_norm,char_black,speed_2
		!scr "      "

		!scr "An "
		!scr ebcm_3,char_dk_grey," Eagle Soft "

		!scr ebcm_0,char_black," inspired intro by "
		!scr ebcm_2,speed_1,char_dk_grey," Cosine "
		!scr char_black,speed_3,"     "

		!scr ebcm_3,"Coding and graphics wiring/"
		!scr "manipulation by "
		!scr char_brown,speed_1
		!scr " The Magic Roundabout "

		!scr char_black,speed_2,"  "

		!scr ebcm_2,"Music composed by "
		!scr char_dk_blue,speed_1
		!scr " aNdy "

		!scr char_black,speed_3,"     "

		!byte ebcm_0,char_black
		!scr "This ",ebcm_1,"EBCM scroller "
		!scr ebcm_0,"has several embedded "
		!scr ebcm_2,"control "
		!scr ebcm_3,"codes "
		!scr ebcm_0,"for "

		!byte ebcm_1
		!scr char_dk_red,"c",char_orange,"o"
		!scr char_yellow,"l",char_lt_green,"o"
		!scr char_lt_blue,"u",char_purple,"r"

		!byte ebcm_0,char_black
		!scr " and font selection, so there isn't "
		!scr "much space left for the "
		!scr ebcm_1,speed_2,"actual message!"
		!scr "   "

		!byte ebcm_0,speed_4
		!scr "That means I need to crack on with "
		!scr "things, so...   "

		!byte ebcm_1,speed_1
		!scr "Cosine send their greetings to:"
		!scr "   ",speed_5

		!scr ebcm_2,lc_norm,"Absence"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Abyss Connection"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Arkanix Labs"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Artstate"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Ate Bit"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Atlantis"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Booze"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Camelot"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Censor"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Chorus"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Chrome"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"CNCD"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"CMS"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"CPU"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Crescent"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Crest"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Covert Bitops"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Defame"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Defence Force"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Dekadence"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Desire"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"DAC"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"DMAgic"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Dual Crew"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Exclusive On"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Fairlight"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"F4CG"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"FIRE"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Flat 3"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Focus"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"French Touch"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"FSP"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Genesis Project"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Gheymaid Inc"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Hitmen"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Hoaxers"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Hokuto Force"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"LOD"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Level 64"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Maniacs Of Noise"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Mayday"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Mean Team"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Metalvotze"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Noname"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Nostalgia"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Nuance"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Offence"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"OMG"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Onslaught"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Orb"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Oxyron"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Padua"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Performers"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Plush"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"PPCS"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Psytronik"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Reptilia"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Resource"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"RGCD"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Samar"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Secure"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"SHAPE"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Side B"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Singular"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Slash"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Slipstream"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"SCS*TRC"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Style"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Suicyco Industries"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Taquart"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Tempest"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"TEK"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"The Solution"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Triad"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"TRSI"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Viruz"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Vision"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"WOW"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Wrath"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_3,lc_norm,"Xenon"
		!scr ebcm_0,uc_norm," S "

		!scr ebcm_2,lc_norm,"Xentax"

		!scr "     "

		!byte ebcm_0,char_black,speed_3
		!scr "Now I just have space to plug "

		!byte ebcm_1,char_dk_blue,speed_1
		!scr " Cosine.org.uk "

		!byte ebcm_0,char_black,speed_3
		!scr " and this was "

		!byte ebcm_1,char_dk_red,speed_1
		!scr " T.M.R of Cosine "

		!scr char_black,speed_3
		!scr " signing off on "

		!scr char_dk_red,speed_1
		!scr " 2019-12-02 "

		!scr char_black,speed_4,"             "

		!byte $00		; end of text marker


; The remaining code and data at $4000 onwards
		* = $4000

code_skip

; Scroll updater
		lda scrl_speed
		sta irq_store_1

; Scroll loop
scrl_loop	ldx scrl_x
		inx
		cpx #$08
		bcs *+$05
		jmp sx_xb

		ldy scrl_line

; Shift the various lines in the back buffer
		ldx #$00
mover		lda scrl_line+$01,x
		sta scrl_line+$00,x

		lda scrl_ctrl_line+$01,x
		sta scrl_ctrl_line+$00,x

		lda scrl_col_line+$01,x
		sta scrl_col_line+$00,x

		inx
		cpx #$26
		bne mover

		sty scrl_line+$26

		sty def_copy_write+$01

		lda #$00
		asl def_copy_write+$01
		rol
		asl def_copy_write+$01
		rol
		asl def_copy_write+$01
		rol
		clc
		adc #>scrl_font
		sta def_copy_write+$02

; Read the next byte of scroll text
mread		lda scrl_text
		bne okay

		lda #<scrl_text
		sta mread+$01
		lda #>scrl_text
		sta mread+$02

		jmp mread

; Update the scroll reader
okay		inc mread+$01
		bne *+$05
		inc mread+$02

; Check for $FF - upper case inverted
		cmp #$ff
		bne okay_2

		lda #$d4
		sta font_select

		jmp mread

; Check for $FE - lower case inverted
okay_2		cmp #$fe
		bne okay_3

		lda #$dc
		sta font_select

		jmp mread

; Check for $FD - upper case
okay_3		cmp #$fd
		bne okay_4

		lda #$d0
		sta font_select

		jmp mread

; Check for $FC - lower case
okay_4		cmp #$fc
		bne okay_5

		lda #$d8
		sta font_select

		jmp mread

; Check for $A0 upwards - speed settings
okay_5		cmp #$a0
		bcc okay_6

; Decode the speed and loop back for another char
		and #$07
		sta scrl_speed

		jmp mread

; Check for $9x - character colour
okay_6		cmp #$90
		bcc okay_7

; Decode the colour and loop back for another char
		and #$0f
		sta scrl_char_col

		jmp mread

; Check for $9x - character colour
okay_7		cmp #$80
		bcc okay_8

; Decode the colour and loop back for another char
		and #$03
		sta scrl_ebcm_col

		jmp mread

; Commands checked for/processed
okay_8		sta def_copy_read+$01

		lda #$00
		asl def_copy_read+$01
		rol
		asl def_copy_read+$01
		rol
		asl def_copy_read+$01
		rol
		clc
		adc font_select
		sta def_copy_read+$02

; Fetch a character
		lda #$32
		sta $01

		ldx #$00
def_copy_read	lda $d800,x
def_copy_write	sta scrl_font,x
		inx
		cpx #$08
		bne def_copy_read

		lda #$35
		sta $01

		ldx #$00
sx_xb		stx scrl_x

; Add colour data for the new character
		lda scrl_ebcm_col
		asl
		asl
		asl
		asl
		asl
		asl
		sta scrl_ctrl_line+$26

		lda scrl_char_col
		sta scrl_col_line+$26

; Are we done yet...?
		dec irq_store_1
		beq *+$05
		jmp scrl_loop


; Merge the scroll and it's "attribute" data
		ldx #$00
scrl_merge	lda scrl_line,x
		ora scrl_ctrl_line,x
		sta $07c0,x

		lda scrl_col_line,x
		sta $dbc0,x
		inx
		cpx #$28
		bne scrl_merge

; Change the bar positions
		ldx bar_pos_tmr
		inx
		cpx #$03
		bne bpt_xb

		ldx bar_pos_1
		inx
		cpx #$0a
		bne *+$04
		ldx #$00
		stx bar_pos_1

		ldx bar_pos_2
		inx
		cpx #$0e
		bne *+$04
		ldx #$00
		stx bar_pos_2

		ldx bar_pos_3
		dex
		cpx #$ff
		bne *+$04
		ldx #$0d
		stx bar_pos_3

		ldx #$00
bpt_xb		stx bar_pos_tmr

; Update the self mod for the bar colours
		ldy bar_pos_1
		lda bar_colours_1+$04,y
		sta ebcm_2_ln_00+$01
		lda bar_colours_1+$03,y
		sta ebcm_2_ln_01+$01
		sta ebcm_2_ln_07+$01
		lda bar_colours_1+$02,y
		sta ebcm_2_ln_02+$01
		sta ebcm_2_ln_06+$01
		lda bar_colours_1+$01,y
		sta ebcm_2_ln_03+$01
		sta ebcm_2_ln_05+$01
		lda bar_colours_1+$00,y
		sta ebcm_2_ln_04+$01

		ldy bar_pos_2
		lda bar_colours_2+$00,y
		sta ebcm_3_ln_00+$01
		lda bar_colours_2+$01,y
		sta ebcm_3_ln_01+$01
		lda bar_colours_2+$02,y
		sta ebcm_3_ln_02+$01
		lda bar_colours_2+$03,y
		sta ebcm_3_ln_03+$01
		lda bar_colours_2+$04,y
		sta ebcm_3_ln_04+$01
		lda bar_colours_2+$05,y
		sta ebcm_3_ln_05+$01
		lda bar_colours_2+$06,y
		sta ebcm_3_ln_06+$01
		lda bar_colours_2+$07,y
		sta ebcm_3_ln_07+$01

		ldy bar_pos_3
		lda bar_colours_3+$00,y
		sta ebcm_4_ln_00+$01
		lda bar_colours_3+$01,y
		sta ebcm_4_ln_01+$01
		lda bar_colours_3+$02,y
		sta ebcm_4_ln_02+$01
		lda bar_colours_3+$03,y
		sta ebcm_4_ln_03+$01
		lda bar_colours_3+$04,y
		sta ebcm_4_ln_04+$01
		lda bar_colours_3+$05,y
		sta ebcm_4_ln_05+$01
		lda bar_colours_3+$06,y
		sta ebcm_4_ln_06+$01
		lda bar_colours_3+$07,y
		sta ebcm_4_ln_07+$01


; Update the sprite X positions - upper sprites
		lda #$00
		sta sprite_x_msb_1

		lda cos_at_1
		clc
		adc cos_speed_1
		cmp #$fc
		bcc *+$04
		lda #$01
		sta cos_at_1
		tax

		lda sprite_x_cos,x
		clc
		adc #$1d
		sta sprite_x_pos_1+$00
		sta sprite_x_pos_1+$03
		sta sprite_x_pos_1+$05
		cmp sprite_x_cos,x
		bcs spr_msb_skip_1
		sta irq_store_1
		lda sprite_x_msb_1
		ora #$29
		sta sprite_x_msb_1
		lda irq_store_1
spr_msb_skip_1	clc
		adc #$18
		sta sprite_x_pos_1+$01
		sta sprite_x_pos_1+$06
		cmp sprite_x_cos,x
		bcs spr_msb_skip_2
		sta irq_store_1
		lda sprite_x_msb_1
		ora #$42
		sta sprite_x_msb_1
		lda irq_store_1
spr_msb_skip_2	clc
		adc #$18
		sta sprite_x_pos_1+$02
		sta sprite_x_pos_1+$04
		sta sprite_x_pos_1+$07
		cmp sprite_x_cos,x
		bcs spr_msb_skip_3
		lda sprite_x_msb_1
		ora #$94
		sta sprite_x_msb_1
spr_msb_skip_3

; Update the sprite Y positions - upper sprites
		lda cos_at_2
		clc
		adc cos_speed_2
		sta cos_at_2
		tax

		lda sprite_y_cos,x
		sta sprite_y_pos_1+$00
		sta sprite_y_pos_1+$01
		sta sprite_y_pos_1+$02
		clc
		adc #$15
		sta sprite_y_pos_1+$03
		sta sprite_y_pos_1+$04
		clc
		adc #$15
		sta sprite_y_pos_1+$05
		sta sprite_y_pos_1+$06
		sta sprite_y_pos_1+$07

; Update the sprite X positions - lower sprites
		lda #$00
		sta sprite_x_msb_2

		lda cos_at_3
		clc
		adc cos_speed_3
		sta cos_at_3
		tax

		lda sprite_x_cos,x
		clc
		adc #$1d
		sta sprite_x_pos_2+$00
		sta sprite_x_pos_2+$03
		sta sprite_x_pos_2+$05
		cmp sprite_x_cos,x
		bcs spr_msb_skip_4
		sta irq_store_1
		lda sprite_x_msb_2
		ora #$29
		sta sprite_x_msb_2
		lda irq_store_1
spr_msb_skip_4	clc
		adc #$18
		sta sprite_x_pos_2+$01
		sta sprite_x_pos_2+$06
		cmp sprite_x_cos,x
		bcs spr_msb_skip_5
		sta irq_store_1
		lda sprite_x_msb_2
		ora #$42
		sta sprite_x_msb_2
		lda irq_store_1
spr_msb_skip_5	clc
		adc #$18
		sta sprite_x_pos_2+$02
		sta sprite_x_pos_2+$04
		sta sprite_x_pos_2+$07
		cmp sprite_x_cos,x
		bcs spr_msb_skip_6
		lda sprite_x_msb_2
		ora #$94
		sta sprite_x_msb_2
spr_msb_skip_6

; Update the sprite Y positions - lower sprites
		lda cos_at_2
		clc
		adc #$20
		tax

		lda sprite_y_cos,x
		clc
		adc #$34	;3a
		sta sprite_y_pos_2+$00
		sta sprite_y_pos_2+$01
		sta sprite_y_pos_2+$02
		clc
		adc #$15
		sta sprite_y_pos_2+$03
		sta sprite_y_pos_2+$04
		clc
		adc #$15
		sta sprite_y_pos_2+$05
		sta sprite_y_pos_2+$06
		sta sprite_y_pos_2+$07


; Set up for the first split
		lda #$01
		sta rn
		lda #rstr1p
		sta $d012

; Exit the IRQ
ea31		pla
		tay
		pla
		tax
		pla
nmi		rti


; Sprite positions
sprite_x_pos_1	!byte $00,$00,$00,$00,$00,$00,$00,$00
sprite_x_msb_1	!byte $00
sprite_y_pos_1	!byte $43,$43,$43,$58,$58,$6d,$6d,$6d
sprite_cols_1	!byte $0a,$05,$0e,$0a,$0e,$0a,$05,$0e

sprite_x_pos_2	!byte $40,$40,$00,$00,$00,$00,$00,$00
sprite_x_msb_2	!byte $00
sprite_y_pos_2	!byte $43,$43,$43,$58,$58,$6d,$6d,$6d
sprite_cols_2	!byte $0a,$0a,$0a,$05,$05,$0e,$0e,$0e

sprite_dps	!byte $f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff


; Raster bar colours for the scroller
bar_colours_1	!byte $09,$0b,$0c,$0f,$07,$01,$07,$0f
		!byte $0c,$0b,$09,$0b,$0c,$0f,$07,$01
		!byte $0f

bar_colours_2	!byte $06,$0b,$04,$0e,$05,$03,$0d,$01
		!byte $0d,$03,$05,$0e,$04,$0b,$06,$0b
		!byte $04,$0e,$05,$03,$0d,$01,$0d,$03

bar_colours_3	!byte $09,$02,$08,$0c,$0a,$0f,$07,$01
		!byte $07,$0f,$0a,$0c,$08,$02,$09,$02
		!byte $08,$0c,$0a,$0f,$07,$01,$07,$0f


; Scroller work buffers
scrl_line	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00

scrl_ctrl_line	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00

scrl_col_line	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00
