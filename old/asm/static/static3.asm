; "STATIC" program - Version 3.0 of snow, I guess.
; It works _AWESOME_! consider this my first real assembly program!

; This is for Qbasic - it stops when the mouse is moved or a key is
; pressed.

;     ****  ASSUMES A MOUSE IS INSTALLED ALREADY, BECAUSE QBASIC
;           DETECTS IT! *******
;     Unrem the lines with ° on them to have the detector work.


_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME  CS:_TEXT,DS:_TEXT
               ASSUME  ES:_TEXT,SS:_TEXT
               ORG     100H

START:         JMP     MAIN

; Constants - the EQUs (The book calls them "Equates")
; These don't take up memory so it makes sense to have lots of them
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
;message1       db      "---------- Thank you for your patronage! ----------",crlf,crlf,"$"
;message15      DB      "(This was the sound version)",crlf,crlf,"$"
;MESSAGE2       DB      "SNOW 2.0 is Copyright (c) 1995 Tom Murphy of Mooseware Development.",crlf,"$"
;message3       db      crlf,"All rights reserved.",crlf,"$"
message4       db      "No mouse, fool.",crlf,crlf,"$"
PORT_OFF       Db      0
; ------------
RandSeed     DW 348Bh
RandSeed2    DW 7F34h
RandSeed3    DW 32bfh


;              CODE AREA
;              ---------
MAIN           PROC    NEAR

               CLD                             ;All string operations forward.
                                               ; (don't know what that means)
               MOV     AX,000Dh
               INT     10H
               
               in     Al,61h                   ;set variable for
               mov    port_off, al             ;port with speaker off


               ;MOV             AX,0000H  ;°      ; initialize mouse
               ;INT               33H     ;°
               ;cmp ax,0                  ;°
               ;je  crapout               ;°


;Returns:         AX = 0000H, No mouse installed
;                             FFFFH, Mouse is installed
;                     BX = The Number Of Buttons
               
               
               MOV       AX,0004H              ; put cursor at 0,0
               MOV       CX,0
               MOV       DX,0
               INT          33H
               
               
               ; main loop

floop:               
               mov       ax,0a000h           ; set es to screen 10 segment
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
               stosb
               inc    cx
               cmp    cx,8000
               jne rloop
               xor    cx,cx
               inc dx               
               XOR     AX,AX
               MOV     ES,AX
               MOV     AX,ES:[41Ah] ; loop if no key waiting.
               CMP     AX,ES:[41Ch] 
               JNZ     keyhit
               CALL    Checkmouse
               cmp     ax,0
               je      floop
               jmp     terminate
keyhit:
               XOR     AX,AX
               INT     16h
               
               ; key was pressed            

TERMINATE:                 
               mov     al,port_off
               out     61h, al
               
               MOV     AX,0003   ; reset screen
               INT     10H

;               MOV     DX, OFFSET Message1
;               CALL    PRINT
;               MOV     DX, OFFSET Message15
;               CALL    PRINT
;               MOV     DX, OFFSET Message2
;               CALL    PRINT
;               MOV     DX, OFFSET Message3
;               CALL    PRINT
               

               MOV     AX, 0008                ;Return to DOS.
               INT     21H

crapout:
               MOV     AX,0003   ; reset screen
               INT     10H

               MOV     DX, OFFSET Message4
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

Checkmouse:    ; checks to see if the mouse has been moved or pressed.
               ; returns ax>0 if it has
                       push cx
                       push dx
                       MOV         AX,0003H
                       INT            33H
                       add cx,dx
                       add cx,bx
                       cmp cx,0
                       jne mouseused
                       ; mouse not moved
                       mov ax,0
                       pop dx
                       pop cx
                       ret
mouseused:             mov ax,1
                       pop dx
                       pop cx
                       ret


;Returns:           BX = Button Status
;                       CX = X Position
;                       DX = Y Position




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


_TEXT          ENDS
               END     START

;temp area


;Use this to deactivate the mouse cursor:
    
;                     MOV         AX,0002H
;                     INT           33H

;Use this to determine whether or not a mouse button has been preesed:

;            MOV     AH,0005H
;            MOV     BX,BUTTON (0=LEFT, 1=RIGHT)  
;            INT        33H

;Returns:
;            AX = Button Status  (1= pressed, 0=not pressed)
;            BX = Count of button presses
;            CX = Mouse X Location at time of press
 ;           DX = Mouse Y Location at time of press



