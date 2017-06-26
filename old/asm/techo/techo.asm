; The Echo Text-Mask 'intro'
; Copyright (c) 1995 Tom Murphy (Frood).
; Questions/comments: IMightBeTM@aol.com
; 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
; This code is for learning purposes only. Feel free to mess around
; with it and compile variations of it, but please don't steal it.
; I'm distributing this code for your benefit. All rights reserved.

_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME CS:_TEXT               
               ASSUME DS:_TEXT
org 100h

START:         JMP     MAIN

; Constants - the EQUs (The book calls them "Equates")
CR             EQU     13
LF             EQU     10
crlf           equ     13,10
CTRL_Z         EQU     26
SPACE          EQU     32
BOX            EQU     254
BLACK          EQU     0
BLUE           EQU     1
;etc etc


; Data Bytes - the D_s (Or, Pseudo-ops, as the book calls them)
    

;              CODE AREA
;              ---------
MAIN           PROC    NEAR
               MOV     AX,0003   ; reset screen
               INT     10H
toploop:   
               ; move attribute mask
               mov al,speedx
               add startcol,al
               cmp startcol,2               
               jne leftok
               mov speedx,1
leftok:
               cmp startcol,48
               jne rightok
               mov speedx, -1
rightok:

               mov ax,0b800h
               mov es,ax
               mov cx,80*25
               mov di,1
               xor bx,bx
               mov bl,startcol
attribloop:
               ; sets attributes
               mov al,[attributes+bx]
               stosb
               inc di ; skip byte
               inc bx
               dec cx
               jnz attribloop

               ; put block of text in memory...
;               mov ax,0b800h
;               mov es,ax
               mov cx,80*25
               xor bx,bx
               ;mov bl,startcol

rndloop:
               push bx
               call getrandomnumber
               and ax,0000000000001111b; 00001111b ; so ah and al are 0-15
               mov bx,ax
               mov ah,[offset digits+bx] ; turn ax into a number
               ;add al,48 ; turn ax into a number
               pop bx
               mov [characters+bx],ah
               ;mov [characters+bx+1],al
               ;add bx,2
               inc bx
               dec cx
               jnz rndloop

               ; write block to screen memory
               call waitretrace
               mov cx,80*25 
               xor di,di
               xor bx,bx
textloop:
               ; sets attributes
               mov al,[characters+bx]
               stosb
               inc di ; skip byte
               inc bx
               dec cx
               jnz textloop
               
               XOR     AX,AX
               MOV     ES,AX
               MOV     AX,ES:[41Ah] ; loop if no key waiting.
               CMP     AX,ES:[41Ch] 
               JZ      topLoop

               XOR     AX,AX
               INT     16h
               
               ; key was pressed            

               MOV     AX,0003   ; reset screen
               INT     10H
               MOV     AH,9
               
               mov dx,offset outmsg
               INT     21h


               MOV     AX, 0008                ;Return to DOS.
               INT     21H

MAIN           ENDP
; ----------===== SUBS ======----------
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

Waitretrace:
    push ax dx
    mov dx,3DAh
l1:
    in al,dx
    and al,08h
    jnz l1
l2:
    in al,dx
    and al,08h
    jz  l2
    pop dx ax
ret

RandSeed     DW 348Bh
RandSeed2    DW 7F34h
RandSeed3    DW 32bfh

outmsg db 'Copyright (c) 1995 Tom Murphy (Frood) IMightBeTM@aol.com',crlf,'$'
digits db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
include etext.inc
startcol db 2
speedx db 1
characters db 80 dup(25 dup(?))
_TEXT          ENDS
               END     START
