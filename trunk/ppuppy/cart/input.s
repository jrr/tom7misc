; Joypad reading code. Based on code written by Doug Fraker in 2015.

.importzp _joy1, _joy1test, _joy2, _joy2test
.export _GetInput

.segment "CODE"

; due to dmc audio playback occasionally messing with input, it is best
; to read twice and compare them
_GetInput:
        ldx #$01        ; strobe controller 1
        stx $4016
        dex
        stx $4016

        ldy #$08
Get_Input2:             ; get first read, store them as a test
  	lda $4016
        and #$03
        cmp #$01
        rol _joy1test
        lda $4017
        and #$03
        cmp #$01
        rol _joy2test
        dey
        bne Get_Input2

GetInputFixBug:
        ldx #$01        ; restrobe strobe controller 1
        stx $4016
        dex
        stx $4016

        ldy #$08
Get_Input3:             ; read again, store them as joypads
        lda $4016
        and #$03
        cmp #$01
        rol _joy1
        lda $4017
        and #$03
        cmp #$01
        rol _joy2
        dey
        bne Get_Input3

CompareInput:
        lda _joy1
        cmp _joy1test
        bne :+
        lda _joy2
        cmp _joy2test
        bne :+
        rts                             ; if same, done

:       lda _joy1
        sta _joy1test
        lda _joy2
        sta _joy2test

        jmp GetInputFixBug ; if different, reread

