    .export _DoDMA
  
    .segment "CODE"
 
_DoDMA: 
    lda #0
    sta $2003
    lda #2
    sta $4014 ;push sprite data to OAM from $200-2ff


  ;;  I ended up not using this...
_EndVBlank: 
    lda #$90
    sta $2000 ;nmi on
    lda #$1e  
    sta $2001 ;screen on
    lda $2002 ;reset the latch
    lda #0
    sta $2005
    sta $2005 ;double checking that the scroll position is reset
    rts
