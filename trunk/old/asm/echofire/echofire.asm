
;; I patched this code to work with MASM (removing spurious "offset"
;; keywords and TASM-specific macros, and doing a little code motion)
;; Other than that, it's just as it was in 1995. (I was young...)

;; This program and associated include files are in the public domain,
;; despite my claims of copyright below (which are left for historic
;; reasons). The program comes with ABSOLUTELY NO WARRANTY.
    
;;  - Tom 7,    24 Jul 2001
        
;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ Version 8.0 (!) is Copyright (c) 1995
;³ Ûß Ûß Û Û ÛßÛ Ûß ßÛß ÛßßÛ Ûß ³ Tom Murphy of EùCùHùO.
;³ Û  Û  Û Û Û Û Û   Û  Û  Û Û  ³ All Rights reserved.
;³ Ûß Û  ÛßÛ Û Û Ûß  Û  ÛßÛß Ûß ³  
;³ Û  Û  Û Û Û Û Û   Û  Û Û  Û  ³ 
;³ ÛÜ ÛÜ Û Û ÛÜÛ Û  ÜÛÜ Û Û  ÛÜ ³ 
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ 
; I'm distributing this source code so that others may learn from it...
; That is, after all, how I learned.
; However, you may not just take your group's name and stick it all over
; this... If you sufficiently change this program (enough so that I wouldn't
; immediately recognize it, for example), then you have my permission
; to distribute it, as long as I am credited for my work.

; Under no circumstances may this program, source code, or .OBJ files 
; created from this source or a modified version thereof be sold.

; The comments are almost all wrong. I always write code, comment it,
; and then change the code and just leave the comments. Do not trust the
; comments.

; You'll also save yourself a lot of trouble by not un-remming lines of
; code which have been remmed. Most usually (unless noted) these are lines
; that make it crash or something. 

; Good ways to crash this program:
; Load DeluxePaint's CAMERA.COM, run fire, take a picture, and exit.
; Mess around with important routines like Waitretrace or Getnextal

; If anyone comes up with anything cool from this source, by all means,
; send it over. I'd be thrilled to see it.

; You can contact me at:

; IMightBeTM@aol.com

; Or...

; Tom Murphy
; 339 Still Hill Rd.
; Hamden, CT 06518.1830396

; My humble attempt at a fire demo...
; number 8

_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME  CS:_TEXT,DS:_TEXT
               ASSUME  ES:_TEXT,SS:_TEXT
               ORG     100H
.386  ; Yes... you need a 386
START:         JMP     MAIN

; Constants - the EQUs 
CR             EQU     13
LF             EQU     10
crlf           EQU     13,10
CTRL_Z         EQU     26
SPACE          EQU     32
;etc etc

; ------------
RandSeed     DW 348Bh   ; seeds for random number generator
RandSeed2    DW 7F43h
RandSeed3    DW 32bfh
Startx       dw 30
starty       dw 10
echox        dw ?
echoy        dw ?
r db ?
g db ?
b db ?
color db ?
fading db 0
ir db ?

;              CODE AREA
;              ---------
MAIN           PROC    FAR
               
               ; zero out virtual screen
               xor bx,bx
               xor ax,ax
zerotop:       add BX,2
               mov [offset startbuffer+BX],AX               
               cmp bx,32960
               jnae zerotop
               xor bx,bx
               xor ax,ax

               ; try to fade out the DOS screen
               ;get the colors of DOS color 7d
               call Waitretrace               
               mov dx,3c7h
               mov al,7
               out dx,al
               mov dx,3c9h                              
               in al,dx
               mov r,al   ; bl=r
               in al,dx
               mov g,al   ; bh=g
               in al,dx    ; al=b
               mov b,al
               mov ir,al   ; for later
               
               call fade15
               jmp okay
fadeloop:      
               call waitretrace         
               dec r
               dec g
               dec b
               mov dx,3c8h
               mov al,7
               out dx,al
               mov dx,3c9h
               mov al,r
               out dx,al
               mov al,g
               out dx,al
               mov al,b
               out dx,al
               cmp r,0
               jne fadeloop
okay:
               Mov     ax,0013h               
               int     10h



               mov     ax,0a000h
               mov     es,ax
               xor     di,di

; set the pallette
               xor ax,ax
               xor bx,bx
               xor cx,cx  
               xor dx,dx
               
toppal:
               mov ax,cx
               mov dx,3c8h
               out DX,AL
               call getnextal
               mov dx,3c9h
               out dx,al
               call getnextal
               out dx,al
               call getnextal
               out dx,al
               inc cx
               cmp cx,0ffh
               jna toppal               

outpal:        
               ; locate cursor and print message

               ; Load the message...

               call putmsg

toprndbot:     ; sets up the bottom row in the startbuffer
               inc dx        
redornd:               
               call getrandomnumber
               and ax,0000001111111111b 
               cmp ax,319
               jae redornd          ; get a random number from 0-319
               mov bx,ax
               push bx
               call getrandomnumber
               pop bx
               and ax,0000000000000111b
               add ax,92
        
               ; check if we're to put an echomessage...
               cmp bx,42
               jne noecho
               cmp ax,94
               jne noecho
               ; yes, we are.
               mov doecho,1
               jmp yesecho
noecho:

               mov cl,0f0h
               call bpixel
yesecho:               
               cmp dx,200
               jne toprndbot
               
bigloop:               

               xor bx,bx
               xor ax,ax
               xor cx,cx
littleloop:
               ; do pixels
               mov al,ds:[ startbuffer+bx]  ; get a pixel
               mov cl,ds:[ startbuffer+bx+320] ; get the pixel 1 below it
               add ax,cx ;add the colors 
               mov cl,ds:[ startbuffer+bx+321] ; 1 below, 1 to the right
               add ax,cx
               mov cl,ds:[ startbuffer+bx+319] ; 1 below, 1 to the left
               add ax,cx
               shr ax,2  ; divide by 4   (average)
               sub ax,1  ; decrease it so it fades
               cmp ah,0  ; see if it went over
               je lnext
               xor ax,ax
lnext:
               mov ds:[ startbuffer+bx],al ; put it back
               inc bx
               cmp bx,32320 ; go 1 line past end of buffer
               jne littleloop
               cmp doecho,1
               jne outo
               call putecho
               outo:
               call showscreen
               ;cmp fading,1
               ;je fadeit
               call checkkey
               cmp dx,0
               ;je bigloop
               je toprndbot
               ;mov fading,1
               ;jmp toprndbot
               ; key pressed... exit             
               jmp fade256
;fadeit:
;call fade256
;jmp toprndbot

; -----------------END-------------------------------------------------

MAIN           ENDP
; ----------===== SUBS ======----------
Ppixel:    ; bx = x coord
           ; ax = y coord     
           ; cl = color (byte)
      push dx
      cmp  ax,320
      jnl  outp
      cmp  bx,200
      jnl  outp
      mov  dx,320d
      mul  dx
      mov  Dx,Ax
      add  Dx,Bx
      mov  Al,cl
      mov  bx,dx
      mov  es:[BX],AL
outp: pop dx
ret

Getnextal:
     mov al,[ fpal+bx]
     inc bx
     ret

Getnextpic:
     mov al,[ firemsg+bx]
     inc bx
     ret
Getnextpic2:
     mov al,[ echomsg+bx]
     inc bx
     ret

Bpixel:    ; bx = x coord             ; puts it in the buffer
           ; ax = y coord     
           ; cl = color (byte)
     push dx
     cmp  ax,200
     jnl  outp2
     cmp  bx,320
     jnl  outp2
     mov  dx,320d
     mul  dx
     mov  Dx,Ax
     add  Dx,Bx
     mov  Al,cl
     mov  bx,dx
     mov  ds:[ Startbuffer+BX],AL
outp2:   pop dx
ret

Showscreen:
     call waitretrace
     xor di,di
     mov di,32000
     mov cx,7520    ; switch these for a different effect on the bottom
     ;mov cx,8000   ; of the fire
     mov ax,offset startbuffer
     mov si,ax
     rep movsd
     ret


PRINT:
               MOV     AH,9
               INT     21h
               RET

Checkkey:   ; checks to see if a key was pressed - DX>0 if it was
               PUSH    AX
               PUSH    ES

               XOR     AX,AX
               MOV     ES,AX
               MOV     AX,ES:[41Ah]
               CMP     AX,ES:[41Ch] 
               JNZ     Keypressed
               ; key not pressed
               XOR     DX,DX
               pop es
               pop ax
               ret
keypressed:               
               XOR     AX,AX
               INT     16h
               mov     dx,00ffh
               POP     ES
               POP     AX
               ret

GetRandomNumber:   ; somebody elses - works great though.
        MOV     AX,[RandSeed]
        MOV     BX,[RandSeed2]
        MOV     BP,[RandSeed3]
        ADD     AX,0a137h
        ADD     BX,63f7h
        ADD     BP,784Ah
        ROL     AX,2
        MOV     [RandSeed],AX
        ADD     BX,AX
        ROR     BX,1
        MOV     [RandSeed2],BX
        SUB     BP,BX
        XOR     AX,BP
        MOV     [RandSeed3],BP
        ADD     AX,BX
          RET

Putmsg:
               pusha
               xor ax,ax
               xor bx,bx
call               getnextpic
               add ax,starty
               mov height,ax
               call getnextpic
               add ax,startx
               mov bwidth,ax
               
               xor cx,cx
               xor ax,ax
               xor dx,dx
               ;xor bx,bx
               xor di,di
               mov dx,0ffffh
               mov cx,dx
               ;cx=current x coord
               ;dx=current y coord
               ;al=color
               ;bx=data counter
               ;mov dx,0ffffh
               xor di,di ; reset di
               mov ax,320
               mul starty
               add ax,startx
               mov di,ax
               mov cx,startx ; reset x coord
               mov dx,starty
donextline:
               inc dx
               
doline:        inc cx
               call getnextpic ; now we have a color byte
               cmp al,0   ; don't write if it's 0
               je yahoo
               mov es:[DI],AL
yahoo:    
               inc di
yahoo2:
               ; check if we've gone to the end of the line
               cmp cx,bwidth
               jnae doline
               xor di,di ; reset di
               mov ax,320
               push dx
               mul dx
               add ax,startx
               mov di,ax
               pop dx
               mov cx,startx ; reset x coord
               xor ax,ax ; reset ah to 0
               ; check if it was the last line
               cmp dx,height
               jnae donextline

               ; done... I hope
               popa
ret
Putecho:
               mov doecho,0
               pusha
               ; first get coordinates
redornd2:               
               call getrandomnumber
               and ax,0000001111111111b 
               cmp ax,319
               jae redornd2          ; get a random number from 0-319
               mov echox,ax               
               call getrandomnumber
               and ax,0000000000000111b
               add ax,82
               mov echoy,ax                                             
               
               xor ax,ax
               xor bx,bx
call               getnextpic2
               add ax,echoy
               mov eheight,ax
               call getnextpic2
               add ax,echox
               mov ewidth,ax
               
               xor cx,cx
               xor ax,ax
               xor dx,dx
               ;xor bx,bx
               xor di,di
               mov dx,0ffffh
               mov cx,dx
               ;cx=current x coord
               ;dx=current y coord
               ;al=color
               ;bx=data counter
               ;mov dx,0ffffh
               xor di,di ; reset di
               mov ax,320
               mul echoy
               add ax,echox
               mov di,ax
               mov cx,echox ; reset x coord
               mov dx,echoy
donextline2:
               inc dx
               
doline2:        inc cx
               call getnextpic2 ; now we have a color byte
               cmp al,0   ; don't write if it's 0
               je yahoo3
               mov ds:[Startbuffer+DI],AL
yahoo3:        inc di
               ; check if we've gone to the end of the line
               cmp cx,ewidth
               jnae doline2
               xor di,di ; reset di
               mov ax,320
               push dx
               mul dx
               add ax,echox
               mov di,ax
               pop dx
               mov cx,echox ; reset x coord
               xor ax,ax ; reset ah to 0
               ; check if it was the last line
               cmp dx,eheight
               jnae donextline2

               ; done... I hope
               popa
ret
Waitretrace:
    push ax
    push dx
    mov dx,3DAh
l1:
    in al,dx
    and al,08h
    jnz l1
l2:
    in al,dx
    and al,08h
    jz  l2
    pop dx
    pop ax
ret

Fade15:
     ; This routine doesn't work for some colors... don't know why...
     ; But I don't think anyone will notice.
     ; fades out all 16 colors (for text mode)
     call waitretrace
     mov color,0
bigcycle2:
     ; get current color (color number in "color")
     mov dx,3c7h
     mov al,color
     out dx,al
     mov dx,3c9h
     in al,dx
     mov r,al
     in al,dx
     mov g,al
     in al,dx
     mov b,al
     cmp r,0     ; then decrease the R,G,B, values if they aren't zero
     je n12
     dec r
n12:  cmp g,0
     je n22
     dec g
n22:  cmp b,0
     je n32
     dec b
n32:  mov dx,3c8h           ; and put them back in
     mov al,color
     out dx,al
     mov dx,3c9h
     mov al,r
     out dx,al
     mov al,g
     out dx,al
     mov al,b
     out dx,al
     inc color
     cmp color,15
     jne bigcycle2
     inc steps
     mov color,0
     cmp steps,63
     jne fade15
     mov steps,0
ret

Fade256:
     ; this is a jerk... but it looks cool
     ; fades out all 256 colors
     call waitretrace
     mov color,0
bigcycle:
     ; get current color (color number in "color")
     mov dx,3c7h
     mov al,color
     out dx,al
     mov dx,3c9h
     in al,dx
     mov r,al
     in al,dx
     mov g,al
     in al,dx
     mov b,al
     
     cmp r,0     ; then decrease the R,G,B, values if they aren't zero
     je n1
     dec r
n1:  cmp g,0
     je n2
     dec g
n2:  cmp b,0
     je n3
     dec b
n3:  mov dx,3c8h           ; and put them back in
     mov al,color
     out dx,al
     mov dx,3c9h
     mov al,r
     out dx,al
     mov al,g
     out dx,al
     mov al,b
     out dx,al
     inc color
     cmp color,255
     jne bigcycle
     inc steps
     mov color,0
     cmp steps,63
     jne fade256

    ;; moved "terminate" routine here, since it is the
    ;; only place where it is called.
         MOV     AX,0003
               INT     10H
               
               ; try to fade in the DOS screen
               ; first set it black and write the message.
               call Waitretrace               
               mov r,0
               mov dx,3c8h
               mov al,7
               out dx,al
               mov dx,3c9h
               mov al,r
               out dx,al
               out dx,al
               out dx,al
               MOV     DX, OFFSET Outmessage
               CALL    PRINT
               
fadeloop2:     call waitretrace         
               inc r
               mov dx,3c8h
               mov al,7
               out dx,al
               mov dx,3c9h
               mov al,r
               out dx,al
               out dx,al
               out dx,al
               cmp al,ir
               jne fadeloop2

               ; done... exit to dos
               
               MOV     AX, 0008                ;Return to DOS.
               INT     21H

    ret

; Data Bytes - the D_s (Or, Pseudo-ops, as the book calls them)
Outmessage    db  "Thank you for viewing this E.C.H.O. Demo!",crlf
              db  crlf,"Please distribute it (in unmodified form)!",crlf
              db  crlf,"This demo is Copyright (c) 1995 Tom Murphy",crlf,"of The East Coast Hacking Organization.",crlf
              db  crlf,"Contact the ECHO crew at:",crlf,crlf,"     IMightBeTM@aol.com"
              db  crlf,crlf,"Or:",crlf,crlf, "     E.C.H.O.",crlf,"     c/o Tom Murphy",crlf,"     339 Still Hill Rd.",crlf,"     Hamden, CT 06518.1830396",crlf,crlf,"$"
bwidth         dw ?
height        dw ?
eheight        dw ?
ewidth        dw ?
doecho         db ?
steps    db 0
INCLUDE FPAL.INC   ; pallette
INCLUDE FIREM.INC  ; "Your monitor is on fire!"
INCLUDE ECHOM.INC  ; EùCùHùO
Startbuffer  db   32960 dup (?) ; big array for virtual screen
                                 
_TEXT          ENDS

END     START
