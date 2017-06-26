; "STATIC" program - Version 2.0 of snow, I guess.
; Copyright (c) 1995 Tom Murphy of ECHO. All rights reserved.
; This is fast-stat - sped up for the stupid 286 whitney machine

_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME  CS:_TEXT,DS:_TEXT
               ASSUME  ES:_TEXT,SS:_TEXT
               ORG     100H

START:         JMP     STATPROC

; Constants - the EQUs (The book calls them "Equates")
CR             EQU     13
LF             EQU     10
crlf           equ     13,10
CTRL_Z         EQU     26
SPACE          EQU     32
;etc etc


; Data Bytes - the D_s (Or, Pseudo-ops, as the book calls them)
message1       db      "---------- Thank you for your patronage! ----------",crlf,crlf,"$"
MESSAGE2       DB      "SNOW 2.0 is Copyright (c) 1995 Tom Murphy of EùCùHùO Productions.",crlf,"$"
message3       db      crlf,"All rights reserved.",crlf,"$"
; ------------
RandSeed     DW 348Bh
RandSeed2    DW 7F34h
RandSeed3    DW 32bfh


;              CODE AREA
;              ---------
STATPROC       PROC    NEAR

               MOV     AX,000Dh
               INT     10H
               ; main loop
floop:               
               mov       ax,0a000h           ; set es to screen D segment
               mov       es,ax               ; thru ax
               xor       di,di
               mov       cx,4000

rloop:         call getrandomnumber     
               stosb
               not ax
               stosb
               dec    cx
               jnz rloop
               
               XOR     AX,AX
               MOV     ES,AX
               MOV     AX,ES:[41Ah] ; loop if no key waiting.
               CMP     AX,ES:[41Ch] 
               JZ     floop

               XOR     AX,AX
               INT     16h
               
               ; key was pressed            

TERMINATE:                 
               MOV     AX,0003   ; reset screen
               INT     10H

               MOV     AX, 0008                ;Return to DOS.
               INT     21H

GetRandomNumber:   ; somebody elses - works great though.
        MOV     AX,[RandSeed]
        MOV     BX,[RandSeed2]
        MOV     BP,[RandSeed3]
        ADD     AX,0a137h
        ADD     BX,63f7h
        ADD     BP,784Ah
        ROL     AX,1
	ROL     AX,1
        MOV     [RandSeed],AX
        ADD     BX,AX
        ROR     BX,1
        MOV     [RandSeed2],BX
        SUB     BP,BX
        XOR     AX,BP
        MOV     [RandSeed3],BP
        ADD     AX,BX
        RET

STATPROC       ENDP
_TEXT          ENDS
               END     START
