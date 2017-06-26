; "STATIC" program - Version 2.0 of snow, I guess.
_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME  CS:_TEXT,DS:_TEXT
               ASSUME  ES:_TEXT,SS:_TEXT
               ORG     100H

START:         JMP     MAIN

; Constants - the EQUs (The book calls them "Equates")
; These don't take up memory so it makes sense to have lots of them
CR             EQU     13
LF             EQU     10
CTRL_Z         EQU     26
SPACE          EQU     32
BOX            EQU     254
BLACK          EQU     0
BLUE           EQU     1
data1          equ     4243
data2          equ     41
data3          equ     4322
;etc etc


; Data Bytes - the D_s (Or, Pseudo-ops, as the book calls them)
TOM_IS_COOL    DB      "You're Welcome!",CR,LF,CR,LF,CR,LF, "This was a Tom Murphy/Mooseware 95 Production.",CR,LF,"$"

;              CODE AREA
;              ---------
MAIN           PROC    NEAR

               CLD                             ;All string operations forward.
                                               ; (don't know what that means)
               MOV     AX,0013h
               INT     10H
               mov       cx,1
               ; main loop
            
floop:               
               mov       ax,0a000h           ; set es to screen 13 segment
               mov       es,ax               ; thru ax
               xor       di,di
rloop:               
               add    dx, data1
               rcl    dx,1
               or     dx, data2
               rcl    dx,1
               sub    dx, data3
               rcl    dx,1
               inc    dx               
               mov    al, dl
               
                              
               ;mov       al,blue
               
               stosb
               inc    cx
               cmp    cx,64000
               jne rloop
               mov cx,0
               MOV AH,1   ; wait for key - messes around with AX
               INT 16h
               JZ floop  ; end of loop
               
               
               ; key was pressed            

TERMINATE:                 
               MOV     AX,0003   ; reset screen
               INT     10H

               MOV     DX, OFFSET TOM_IS_COOL
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

randal         proc near
randal         endp

_TEXT          ENDS
               END     START
