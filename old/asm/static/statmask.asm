; "STATIC" with a mask...

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
message1       db      "---------- Thank you for your patronage! ----------",crlf,crlf
      DB      "(This was the sound version)",crlf,crlf
       DB      "STATMASK is Copyright (c) 1995 Tom Murphy of EùCùHùO Productions.",crlf
       db      crlf,"All rights reserved.",crlf,"$"
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
               call      waitretrace
               mov       ax,0a000h           ; set es to screen D segment
               mov       es,ax               ; thru ax
               xor       di,di
               xor       cx,cx

rloop:               
               inc dx
               cmp dx,1000
               jne inside
               call getrandomnumber
               mov bx,ax
               and bx,0000000000010000b
               call sound
               xor dx,dx
inside:                              
               call getrandomnumber
               mov bx,cx
               and al,[Emask+Bx]         ; mask it
               and ah,[Emask+Bx+1]
               stosw               
               add cx,2
               cmp cx,8000
               jne rloop
               
               cmp fading,1
               je terminate
               
               inc dx               
               XOR     AX,AX
               MOV     ES,AX
               MOV     AX,ES:[41Ah] ; loop if no key waiting.
               CMP     AX,ES:[41Ch] 
               JZ      fLoop

               XOR     AX,AX  ; get rid of key
               INT     16h
               
               mov fading,1

               ; key was pressed            

TERMINATE:                 
               call fade16
               cmp steps,63
               jne floop

               mov     al,port_off
               out     61h, al
               
               MOV     AX,0003   ; reset screen
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
               MOV     DX, OFFSET Message1
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
               cmp al,42
               jne fadeloop2

               ; done... exit to dos


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
        MOV     RandSeed,AX
        ADD     BX,AX
        ROR     BX,1
        MOV     RandSeed2,BX
        SUB     BP,BX
        XOR     AX,BP
        MOV     RandSeed3,BP
        ADD     AX,BX
        RET

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

Fade16:
     ; this is a jerk... but it looks cool
     Push ax
     push bx
     push cx
     push dx
     mov color,0
     call waitretrace
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
     cmp color,64
     jne bigcycle
     inc steps
     ;mov color,0
     ;cmp steps,63
     ;je terminate
     pop dx
     pop cx
     pop bx
     pop ax
ret

steps db 0
fading db 0
Include MASK.INC
Port_off   Db      ?
r db ?
g db ?
b db ?
color db ?
_TEXT          ENDS
               END     START
