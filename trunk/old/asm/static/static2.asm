; "STATIC" program - Version 2.0 of snow, I guess.
; Copyright (c) 1995 Tom Murphy of ECHO. All rights reserved.

_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME  CS:_TEXT,DS:_TEXT
               ASSUME  ES:_TEXT,SS:_TEXT
               ORG     100H

START:         JMP     MAIN

; Constants - the EQUs (The book calls them "Equates")
CR             EQU     13
LF             EQU     10
crlf           equ     13,10
CTRL_Z         EQU     26
SPACE          EQU     32
;etc etc


; Data Bytes - the D_s (Or, Pseudo-ops, as the book calls them)
message1       db      "---------- Thank you for your patronage! ----------",crlf,crlf,"$"
message15      DB      "(This was the sound version)",crlf,crlf,"$"
MESSAGE2       DB      "SNOW 2.0 is Copyright (c) 1995 Tom Murphy of EùCùHùO Productions.",crlf,"$"
message3       db      crlf,"All rights reserved.",crlf,"$"
PORT_OFF       Db      0
; ------------
RandSeed     DW 348Bh
RandSeed2    DW 7F34h
RandSeed3    DW 32bfh


;              CODE AREA
;              ---------
MAIN           PROC    NEAR

               MOV     AX,000Dh
               INT     10H
               
               in     Al,61h                   ;set variable for
               mov    port_off, al             ;port with speaker off

               ; main loop

floop:               
               mov       ax,0a000h           ; set es to screen D segment
               mov       es,ax               ; thru ax
               xor       di,di
rloop:               
               inc dx
               cmp dx,1000
               jne inside
               call getrandomnumber
               mov bx,ax
               and bx,0000000000010000b
               call sound
               mov dx,0
inside:                              
               call getrandomnumber
               stosw
               inc    cx
               cmp    cx,8000
               jne rloop
               xor    cx,cx
               inc dx               
               XOR     AX,AX
               MOV     ES,AX
               MOV     AX,ES:[41Ah] ; loop if no key waiting.
               CMP     AX,ES:[41Ch] 
               JZ      fLoop

               XOR     AX,AX
               INT     16h
               
               ; key was pressed            

TERMINATE:                 
               mov     al,port_off
               out     61h, al
               
               MOV     AX,0003   ; reset screen
               INT     10H

               MOV     DX, OFFSET Message1
               CALL    PRINT
               MOV     DX, OFFSET Message15
               CALL    PRINT
               MOV     DX, OFFSET Message2
               CALL    PRINT
               MOV     DX, OFFSET Message3
               CALL    PRINT
               

               MOV     AX, 0008                ;Return to DOS.
               INT     21H

MAIN           ENDP
; ----------===== SUBS ======----------
PRINT          PROC   NEAR
               MOV     AH,9
               INT     21h
               RET
PRINT          ENDP

SOUND          PROC   NEAR  ; makes a sound - frequency in BX
                            ; higher BX = lower sound.
               MOV     AL,10110110b         ; channel 2, wite LSB/MSB
               OUT     43h,AL   ; I don't understand this routine
               MOV     AX, BX   ; cuz I didn't write it.
               OUT     42h, AL  ; it's from a book.
               MOV     Al,Ah
               OUT     42h, AL
               IN      AL,61h
               OR      AL,00000011b     ; enable speaker
               OUT     61h, AL
               RET
SOUND          ENDP

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


_TEXT          ENDS
               END     START
