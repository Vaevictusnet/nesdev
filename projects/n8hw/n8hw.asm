  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;
; CONSTANTS

ACCELH =$2 ; horizontal accelleration factor
ACCELV =$2 ; vertical accelleration factor
LIMIT  =$4 ; velocity limit
SLIMIT =$8 ; speed button velocity limit
JUMPLIMIT =$20
; Button codes and combinations
BUTRIGHT  =$1 
BUTLEFT   =$2 
BUTDOWN   =$4
BUTUP     =$8
BUTSTART  =$10
BUTSELECT =$20
BUTB      =$40
BUTA      =$80
BUTRIGHTB =$41
BUTLEFTB  =$42

;;;;;;;;;;;;;;;

  .rsset $0000  ; put variables at $0000
buttons1 .rs 1  ; put controller data for player 2 at $0000
buttons2 .rs 1  ; put controller data for player 2 at $0001
bgaddress1 .rs 1 ; first byte of bg address
bgaddress2 .rs 1 ; second byte ...
velh .rs 1 ; velocity - horizontal
velv .rs 1 ; velocity - vertical
jumptime .rs 1 ; jump timer
jumpflag .rs 1 ; can jump or not
posx .rs 1 ; X coordinate for player position
posy .rs 1 ; Y coordinate for player position
rng0 .rs 1; random number generator output
rng1 .rs 1; random number generator output
rng2 .rs 1; random number generator output
rng3 .rs 1; random number generator output
rng4 .rs 1; random number generator output
rng5 .rs 1; random number generator output
rng6 .rs 1; random number generator output
rng7 .rs 1; random number generator output
rng8 .rs 1; temp byte for rng manipulation

;;;;;;;;;;;;;;;
    
  .bank 0
  .org $C000 
vblankwait:
  BIT $2002
  BPL vblankwait
  RTS


LoadPalettes:
  LDA $2002    ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006    ; write the high byte of $3F00 address
  LDA #$00
  STA $2006    ; write the low byte of $3F00 address
  LDX #$00
LoadPalettesLoop:
  LDA palette, x        ;load palette byte
  STA $2007             ;write to PPU
  INX                   ;set index to next byte
  CPX #$30            
  BNE LoadPalettesLoop  ;if x = $20, 32 bytes copied, all done
  RTS


RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  JSR vblankwait

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x    ;move all sprites off screen
  INX
  BNE clrmem
   
  JSR vblankwait



; ************** NEW CODE ****************
  ; seed rng
  LDA #$AA
  STA rng0
  LDA #$00
  STA rng1
  LDA #$34
  STA rng2
  LDA #$56
  STA rng3
  LDA #$78
  STA rng4
  LDA #$9A
  STA rng5
  LDA #$BC
  STA rng6
  LDA #$DF
  STA rng7
  JSR LoadPalettes

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0

  LDA #$00
  ;STA $10
  STA bgaddress1
  LDA #$E0
  STA bgaddress2
  ;STA $11

LoadBackgroundOuterLoop:
  LDY #$00

LoadBackgroundLoop:
  ;LDA [$10],Y   
  LDA [bgaddress1],Y   
  STA $01
  PHA
  ;LDA bgoffset
  ;LDA nametable, a     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  CPY #$FF              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  INY                   ; X = X + 1
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                          ; if compare was equal to 128, keep going down
  ;INC $11  ; 41?
  INC bgaddress2  ; 41?
  INX
  CPX #$04
  BNE LoadBackgroundOuterLoop
 

LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address

  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop


LoadSprites:
  LDX #$00
LoadSpritesLoop:
  LDA sprites, x
  STA $0200,x
  INX
  CPX #$10;
  BNE LoadSpritesLoop

ConfigSprites:
  LDA #$80
  STA posx
  LDA #$C0
  STA posy


  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00011110   ; enable sprites
  STA $2001

  LDA #$0F
  STA $4015 ; Enable 4 wave generators, no sample channel

Forever:

  JMP Forever     ;jump back to Forever, infinite loop

LatchButtons:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  RTS

ControllerCollect:
  ; x is our bit counter, set $00 to zero
  LDX #$08
  ; set buttons zero -- theoretically unnecessary
  LDA #$00
  STA buttons1
  STA buttons2
  ; loop through each button...
  ; A B Select Start Up Down Left Right
ControllerCollectLoop:
  
  LDA $4016  ; read button
  ROR A  ; store pertainent bit in carry
  ROL buttons1 ; out of carry into position

  DEX ; finished after all 8 buttons
  BNE ControllerCollectLoop

  LDX #$08
ControllerCollectLoop2:
  LDA $4017  ; read button
  ROR A  ; store pertainent bit in carry
  ROL buttons2 ; out of carry into position

  DEX ; finished after all 8 buttons
  BNE ControllerCollectLoop2

ControllerCollectLoopEnd:
  RTS

  
HandleHorizVelocity:
  ;LDA $0203  ; load current position
  ;CLC
  ;ADC #$01
  ;ADC velh  ; apply velocity, should be from -8 to +8  
  LDA velh
  CLC
  ADC posx
  STA posx
  RTS

DrawPlayer:
; ### FIXME ### ... should probably all be based on the center of the bottom
;               of the character's tiles: currently, it's left middle. i think.
  LDA posx
  CLC
  ADC #-4 ; position centered on posx
  STA $0203 ; adjust horizontal position for top left tile
  STA $020b ; adjust horizontal position for bottom left tile
  CLC
  ADC #8  ; adjust position by a tile to the right.
  STA $0207 ; adjust horizontal position for top right tile
  STA $020f ; adjust horizontal position for bottom right tile

  LDA posy
  STA $0200 ; vpos top left
  STA $0204 ; vpos top right
  CLC
  ADC #8
  STA $0208 ; vpos bottom left
  STA $020c ; vpos bottom right
  
  RTS

HandleVertVelocity:
  LDA velv
  CLC
  ADC posy
  STA posy
  RTS

CheckJump:
  LDA jumpflag
  BEQ CheckJumpButton
  ; degrade jumptime and vertical velocity.
  CMP #2
  BEQ CheckJumpFall
  ; jumping

  INC jumptime
  LDA jumptime
  CMP #JUMPLIMIT
  BNE CheckJumpPhys

  ; jumptime expired
  INC jumpflag
  LDA #3
  STA velv
  LDA #0
  STA jumptime
  RTS

CheckJumpFall:
  INC jumptime
  LDA jumptime
  CMP #JUMPLIMIT
  BEQ CheckJumpStopFall
  lda #$52 ; G3 maybe
  sta $4002
  lda #$01
  sta $4003
  RTS

CheckJumpStopFall:
  LDA #0
  STA velv
  STA jumpflag

  ; stop beep
  lda #$B0
  sta $4000  ; duty 50%, volume max
  RTS

CheckJumpPhys:
  
  RTS

CheckJumpButton:
  LDA buttons1
  AND #BUTA
  ;FIXME <-- 
  ; temp code to randomize posx
;  BNE CheckJumpStartJump
  BEQ CJSJE
  JSR GenerateRandomNumber
  LDA rng1
  STA posy
  JSR GenerateRandomNumber
  LDA rng1
  STA posx
CJSJE:
  RTS
CheckJumpStartJump:
  LDA #1
  STA jumpflag
  LDA #0
  STA jumptime
  LDA #-3
  STA velv

  ; beep
  lda #$BF
  sta $4000  ; duty 50%, volume max

  ;lda #$C9 ; c#3 maybe
  lda #$1C ; G3 maybe
  sta $4002
  lda #$01
  sta $4003
  RTS
  

CheckLeftNorRight:
  LDA buttons1
  AND #3
  ; test code
  BNE CheckLeftNorRightDone

  LDA #0
  STA velh

CheckLeftNorRightDone:
  RTS

CheckLeft:
  LDA buttons1
  AND #BUTLEFT

  BEQ CheckLeftDone
  LDA buttons1
  AND #BUTB
  BEQ CheckLeftNormal

CheckLeftSpeed:
  LDA #-6
  STA velh
  JMP CheckLeftDone

CheckLeftNormal:
  LDA #-2
  STA velh
  JMP CheckLeftDone
  LDA buttons1
  AND #BUTLEFT
  ;BEQ CheckLeftDone
  ; test code for movement.
  BEQ CheckLeftDone
  LDA #-6
  STA velh
  JMP CheckLeftDone
  ; end test code

  LDA velh
  BMI AccLeft
  BEQ StartAccLeft
  
StartAccLeft:
  LDA #-1
  STA velh
  JMP CheckLeftDone

AccLeft:
  LDA #LIMIT
  CMP velh
  BMI CheckLeftDone

  LDA velh
  SEC
  SBC #ACCELH
  STA velh

CheckLeftDone:
  RTS

OldMoveLeft:
  LDA $0203
  SEC
  SBC #$01
  STA $0203
  STA $020b
  CLC
  ADC #$08
  STA $0207
  STA $020f
  RTS

CheckRight:
  LDA buttons1
  AND #BUTRIGHT

  BEQ CheckRightDone
  LDA buttons1
  AND #BUTB
  BEQ CheckRightNormal

CheckRightSpeed:
  LDA #6
  STA velh
  JMP CheckRightDone

CheckRightNormal:
  LDA #2
  STA velh
  JMP CheckRightDone
  ;-----------------------------------------

  BNE CheckRightDone
  LDA #6
  STA velh
  JMP CheckRightDone
  ; end test code

  LDA #2
  CMP $00
  BEQ CheckRightDone

  LDA velh
  BEQ StartAccRight
  BPL AccRight
  
StartAccRight:
  LDA #1
  STA velh
  JMP CheckRightDone

AccRight:
  LDA #LIMIT
  CMP velh
  BPL CheckRightDone
  BEQ CheckRightDone

  LDA velh
  ADC #ACCELH
  STA velh

CheckRightDone:
  RTS


OldMoveRight:
  LDA $0203
  CLC
  ADC #$01
  STA $0203
  STA $020b
  CLC
  ADC #$08
  STA $0207
  STA $020f
  RTS

CheckUp:
  LDA $4016
  AND #%00000001
  BEQ CheckUpDone

;  LDA $0203
;  CLC
;  ADC #$01
;  STA $0203
CheckUpDone:
  RTS

CheckDown:
  LDA $4016 
  AND #%00000001
  BEQ CheckDownDone

;  LDA $0203
;  SEC
;  SBC #$01
;  STA $0203
CheckDownDone:
  RTS
 
GenerateRandomNumber:
  LDA rng6
  ROL A     
  ROL rng7  ; put bit7 from rng6 as bit0 in rng7
  ROL A     ; keep bit6 from rng6 in carry
  LDA rng7  ; load rng7
  ROL A     ; shift A with bit6 from rng6 to align all 8 eor pairs
  EOR rng7  ; xor
  EOR #$FF  ; xnor
  STA rng8  ; store result in temp storage
  LDA rng6  ; slide all registers to the next register
  STA rng7  
  LDA rng5
  STA rng6
  LDA rng4
  STA rng5
  LDA rng3
  STA rng4
  LDA rng2
  STA rng3
  LDA rng1
  STA rng2
  LDA rng0
  STA rng1
  LDA rng8  ; temp into 0
  STA rng0
  RTS

NMI:
  LDA #$00
  STA $2003  ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014  ; set the high byte (02) of the RAM address, start the transfer

;  JSR GenerateRandomNumber

  JSR LatchButtons
  JSR ControllerCollect

  JSR CheckLeft
  JSR CheckRight
  JSR CheckLeftNorRight

  JSR CheckJump

  JSR HandleHorizVelocity
  JSR HandleVertVelocity

  JSR DrawPlayer

  LDA #0
  STA $2005
  STA $2005


  RTI        ; return from interrupt

;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000

;must be first ... 
nametable:
; sky 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 6
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 7
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 8
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; bricks 9
  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  


; sky 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 11
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$24,$31,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 12
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$24,$30,$26,$34,$33,$24,$24,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$24,$30,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24,$24,$24  

; sky 13
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $24,$30,$26,$34,$26,$26,$34,$26,$33,$24,$24,$24,$24,$24,$24,$24  

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
  .db $30,$26,$26,$26,$26,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24  

; earth 14
  .db $B4,$B5,$B4,$B5,$B4,$B5,$60,$61,$62,$63,$B4,$B5,$B4,$B5,$B4,$B5  
  .db $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5  

  .db $B6,$B7,$B6,$B7,$B6,$B7,$64,$65,$66,$67,$B6,$B7,$B6,$B7,$B6,$B7  
  .db $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7  

; earth 15
  .db $B4,$B5,$B4,$B5,$B4,$B5,$68,$69,$26,$6A,$B4,$B5,$B4,$B5,$B4,$B5  
  .db $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5  

  .db $B6,$B7,$B6,$B7,$B6,$B7,$68,$69,$26,$6A,$B6,$B7,$B6,$B7,$B6,$B7  
  .db $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7  

attribute:
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $FF,$FF,$FF,$FF, $FF,$FF,$FF,$FF
  .db $00,$00,$00,$00, $00,$00,$00,$00
  .db $50,$10,$40,$50, $50,$50,$50,$50
  .db $55,$51,$54,$55, $55,$55,$55,$55


palette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$16,$36,$08,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F
  
sprites:
  .db $C0, $32, $00, $80  ;sprite 0
  .db $C0, $33, $00, $88  ;sprite 1
  .db $C8, $34, $00, $80  ;sprite 2
  .db $C8, $35, $00, $88  ;sprite 3


  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "..\..\sprites\mario.chr"   ;includes 8KB graphics file from SMB1
