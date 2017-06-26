; EYE-PONG v.0003


_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME  CS:_TEXT,DS:_TEXT
               ASSUME  ES:_TEXT,SS:_TEXT
               ORG     100H
.386
START:         JMP     MAIN

; Constants - the EQUs (The book calls them "Equates")
; These don't take up memory so it makes sense to have lots of them
CR             EQU     13
LF             EQU     10
crlf           EQU     13,10
CTRL_Z         EQU     26
SPACE          EQU     32
BOX            EQU     254
BLACK          EQU     0
BLUE           EQU     1
instructions db crlf,crlf,'Left paddle: A/Z',crlf,'Right paddle: 6/3',crlf,'[ESC] to quit.$',26

;etc etc

;              CODE AREA
;              ---------
MAIN           PROC    FAR

               in     Al,61h                   ;set variable for
               mov    port_off, ax             ;port with speaker off

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
               call waitretrace               
               mov ax,001
                              
paltop:        

               MOV DX,3c8h
               xchg ax,cx
               out DX,al
               xchg ax,cx
               MOV dX,3c9h
               out dx,Al ;red
               out dX,Al ;green
               out dX,Al ;blue
               inc cx
               MOV DX,3c8h
               xchg ax,cx
               out DX,al
               xchg ax,cx
               MOV dX,3c9h
               out dX,Al ;red
               out dX,Al ;green
               out dX,Al ;blue
               inc cx
               MOV DX,3c8h
               xchg ax,cx
               out DX,al
               xchg ax,cx
               MOV dX,3c9h
               out dX,Al ;red
               out dX,Al ;green
               out dX,Al ;blue
               inc cx
               MOV DX,3c8h
               xchg ax,cx
               out DX,al
               xchg ax,cx
               MOV dX,3c9h
               out dX,Al ;red
               out dX,Al ;green
               out dX,Al ;blue
               inc cx
               inc ax
               mov bx,ax
               ;putpixel bx,bx,cl
               
               cmp cx,0ffh
               jnae paltop

               ; pallette is grey scale now
               ; (I hope so at least)

outpal:        
               xor cx,cx
               xor ax,ax
               xor dx,dx
               xor bx,bx

toprndbot:
floop:               

; check origin movement, courtesy of TOM

;    push ax 
               mov ax,xspeed
               add startx,ax ; move it up or down
               mov ax,startx               
               cmp ax, 1h ; arbitrary number
               ja oktop1
               ; went over the left. change it to 2
               mov ax,2
               mov xspeed,1
               call lefthit
oktop1:        cmp ax, 248 ; 320 x coord
               jb okbot1
               ; went over on the right. change to 319
               mov xspeed,-1
               mov ax,247
               mov startx,ax
               call righthit
okbot1:
               mov ax,yspeed
               add starty,ax ; move it up or down
               mov ax,starty               
               cmp ax, 4242h ; arbitrary number
               jnae oktop2
               ; went over the top. change it to 0
               mov ax,1
               mov yspeed,1
               mov soundcycles,10
               mov bx,1151
               call sound
oktop2:        cmp ax, 150 ; 200 y coord
               jb okbot2
               ; went over on the right. change to 149
               mov yspeed,-1
               mov ax,149
               mov starty,ax
               mov soundcycles,10
               mov bx,1151
               call sound
okbot2:
 ;              pop ax

               ; sound...
               cmp soundon,1
               jne nosound
               dec soundcycles
               cmp soundcycles,0
               jne nosound
               call spkoff
nosound:
;               call waitretrace
               call waitloop
               call putmsg
               call movepaddles
               call drawpaddles
               call waitkey               
               ;call checkkey
               jmp floop
               ; key pressed... exit             

; -----------------END-------------------------------------------------
TERMINATE:     call spkoff
               MOV     AX,0003
               INT     10H
               
               mov    al,rscore
               mov    si,offset rscorestring
               call byte2str
               mov    al,lscore
               mov    si,offset lscorestring
               call byte2str
               mov    dx, offset leftwords
               call print
               mov    dx, offset rightwords
               call print
               mov    dx, offset instructions
               call print
              
               MOV     DX, OFFSET Outmessage
               CALL    PRINT
                    
               MOV     AX, 0008                ;Return to DOS.
               INT     21H

MAIN           ENDP
; ----------===== SUBS ======----------
term1: jmp terminate
startx dw 10
starty dw 10
soundcycles dw 0
soundon db 0
lpos   dw 80
rpos   dw 80
rdir dw 0
ldir dw 0
lscore db 0
rscore db 0

waitloop:
     mov cx,speed
delay1:
     push cx
     mov cx,speed
delay2:
     loop delay2
     pop cx
     loop delay1
     ret

movepaddles:
               mov ldont,0
               mov rdont,0
               ; move the paddles accordingly
               ;----left paddle------
               mov ax,ldir
               ;and ax,0000000000000111b
               add lpos,ax ; move it up or down
               mov ax,lpos               
               cmp ax, 4242h ; arbitrary number
               jnae oktopy1
               ; went over the top. change it to 0
               xor ax,ax
               ;inc ldir 
               mov ldont,1
               mov ldir,ax
oktopy1:        cmp ax, 180 ; 180 y coord
               jb okboty1
               ; went over on the bottom. change to 179
               mov ldir,0
               ;dec lpos
               mov ldont,1
               mov ax,179
okboty1:        mov lpos,ax       
               ; -----right paddle-----
               mov ax,rdir
               ;and ax,0000000000000111b
               add rpos,ax ; move it up or down
               mov ax,rpos               
               cmp ax, 4242h ; arbitrary number
               jnae oktopy2
               ; went over the top. change it to 0
               xor ax,ax
               mov rdir,ax
               mov rdont,1
               ;inc rpos
oktopy2:        cmp ax, 180 ; 180 y coord
               jb okboty2
               ; went over on the bottom. change to 179
               mov rdir,0
               mov ax,179
               mov rdont,1
               ;dec rpos
okboty2:        mov rpos,ax       
          ret

lefthit:
          ; hit the left side... paddle?
               mov ax,lpos
               cmp ax,starty
               je ldidhit
               jb lcase1
               ; case 2...
               mov ax,starty
               add ax,46
               cmp ax,lpos
               jae ldidhit
   ; didn't hit...
lnohit:               
               mov soundcycles,25
               mov bx,5542
               call sound
               inc rscore
               ret
lcase1:               
               add ax,20  ; check bottom of paddle
               cmp ax,starty
               jnae lnohit
ldidhit:               
               mov soundcycles,10
               mov bx,1100
               call sound
     ret


righthit:

          ; hit the right side... paddle?
               mov ax,rpos
               cmp ax,starty
               je rdidhit
               jb rcase1
               ; case 2...
               mov ax,starty
               add ax,46
               cmp ax,rpos
               jae rdidhit
   ; didn't hit...
rnohit:               
               mov soundcycles,25
               mov bx,5742
               call sound
               inc lscore
               ret
rcase1:               
               add ax,20  ; check bottom of paddle
               cmp ax,starty
               jnae rnohit
rdidhit:               
               mov soundcycles,10
               mov bx,1250
               call sound
     ret

waitkey:
               ;PUSH    ES     ; checks if a key's been pressed - much
               XOR     AX,AX  ; better than int 16.1
               MOV     ES,AX
               MOV     AX,ES:[41Ah]
               CMP     AX,ES:[41Ch] 
               JZ      floop ; key not pressed
               
               XOR     AX,AX ; key pressed - get it
               INT     16h
               
               ; do key stuff
               ;mov key,al
               ; make sure its lowercase               
               cmp al,65
               jnae keyok               
               cmp al,90
               ja  keyok
               add al,32 ; lowercaseize it
keyok:
               cmp al,'^'
               jne ne1
               mov al,'6'
ne1:           cmp al,'#'
               jne ne2
               mov al,'3'
ne2:           
               ; check quit
               cmp al,27 ; ESC
               je term1
               cmp al,'a'
               jne n1
               ; left paddle up
               sub ldir,1
               jmp keydone
n1:            cmp al,'z'
               jne n2
               add ldir,1               
               jmp keydone
n2:            cmp al,'6'
               jne n3
               sub rdir,1
               jmp keydone
n3:            cmp al,'3'
               jne n4
               add rdir,1
               jmp keydone
n4:            cmp al,'+'
               jne n5
               add speed,10
               cmp speed,1000
               jna n5
               mov speed,1000
n5:            cmp al,'-'
               jne n6
               sub speed,10
               cmp speed,30000
               jb n6
               mov speed,1
n6:            cmp speed,0
               jne n7
               mov speed,1
n7:            
               ; maybe later
keydone:       ; done parsing keys, go back
               xor ax,ax ; just for safety
               ;and ldir,1000000000000011b
               ;and rdir,1000000000000011b
               jmp floop

Waitretrace:
    push dx ax
    mov dx,3DAh
l1:
    in al,dx
    and al,08h
    jnz l1
l2:
    in al,dx
    and al,08h
    jz  l2
pop ax dx
ret

Putmsg:
               pusha
               mov ax,0a000h
               mov es,ax
               xor di,di
               xor ax,ax
               xor bx,bx
     mov al,[offset eye+bx]
     inc bx
               add ax,starty
               mov height,ax
     mov al,[offset eye+bx]
     inc bx
               add ax,startx
               mov bwidth,ax
               
               xor cx,cx
               xor ax,ax
               ;xor dx,dx
               ;xor bx,bx
               ;xor di,di
               mov dx,0ffffh
               mov cx,dx
               ;cx=current x coord
               ;dx=current y coord
               ;al=color
               ;bx=data counter
               ;mov dx,0ffffh
               xor di,di ; reset di
               mov ax,320
               imul starty
               add ax,startx
               mov di,ax
               mov cx,startx ; reset x coord
               mov dx,starty
donextline:
               inc dx
               
doline:        inc cx
     mov al,[offset eye+bx]
     inc bx
               stosb
               ;mov es:[DI],AL
               ;inc di
yahoo2:
               ; check if we've gone to the end of the line
               cmp cx,bwidth
               jnae doline
               xor di,di ; reset di
               mov ax,320
               push dx
               imul dx
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
Getnextpic:
     mov al,[offset eye+bx]
     inc bx
     ret
olpos dw ?
orpos dw ?
drawpaddles:
          mov ax,0a000h
          mov es,ax
          xor di,di
          ;jmp dorpad
          ; get rid of old paddle - 
          ; ---- left ----
          cmp ldont,1
          je dorpad
          mov ax,olpos
          cmp lpos,ax
          je dorpad
          ja labove               
          ; lpos<olpos          
          mov bx,lpos
          add bx,20
          mov ax, 320
          imul bx
          mov di,ax
          ;di = start pos
          xor cx,cx
          sub cx,ldir
          ;and cx,0000000000000111b
          ;cx = number to blank
          xor ax,ax ; color
          jmp eraseleft
labove:
          ; lpos>olpos          
          mov bx,olpos
          mov ax, 320
          imul bx
          mov di,ax
          ;di = start pos
          ;xor cx,cx
          mov cx,ldir
          ;and cx,0000000000000111b
          ;cx = number to blank
          xor ax,ax ; color
eraseleft:
          stosb
          add di,319
          loop eraseleft
          ;dec cx
          ;jnz eraseleft
dorpad:          
          cmp rdont,1
          je okspad
          ; ---- right ----
          mov ax,orpos
          cmp rpos,ax
          je okspad
          ja rabove               
          ; rpos<orpos          
          mov bx,rpos
          add bx,20
          mov ax, 320
          imul bx
          add ax,318
          mov di,ax
          ;di = start pos
          xor cx,cx
          sub cx,rdir
          ;and cx,0000000000000111b
          ;cx = number to blank
          xor ax,ax ; color
          jmp eraseright
rabove:
          ; rpos>orpos          
          mov bx,orpos
          mov ax, 320
          imul bx
          add ax,318
          mov di,ax
          ;di = start pos
          ;xor cx,cx
          mov cx,rdir
          ;and cx,0000000000000111b
          ;cx = number to blank
          xor ax,ax ; color
eraseright:
          stosb
          add di,319
          loop eraseright
okspad:          
          ; draw the paddles
          mov ax,320 ; bytes across
          imul lpos
          mov di,ax
          mov al,250 ; byte for paddle
          mov cx,20
lpadloop:          
          ;mov [offset sbuffer+BX],ah
          stosb
          add di,319
          loop lpadloop
          ;dec cx
          ;cmp cx,0 
          ;jnz lpadloop
          ; right paddle
          mov ax,320 ; bytes across
          imul rpos
          mov di,ax
          add di,318
          mov al,250 ; byte for paddle
          mov cx,20
rpadloop:          
          stosb
          add di,319
          ;dec cx
          ;cmp cx,0 
          ;jnz rpadloop
          loop rpadloop
          mov ax,lpos
          mov olpos,ax
          mov ax,rpos
          mov orpos,ax
ret

SOUND:  ;        PROC   NEAR  ; makes a sound - frequency in BX
                            ; higher BX = lower sound.
               mov     soundon,1
               push ax
               MOV     AL,10110110b         ; channel 2, wite LSB/MSB
               OUT     43h,AL   ; I don't understand this routine
               MOV     AX, BX   ; cuz I didn't write it.
               OUT     42h, AL  ; it's from a book.
               MOV     Al,Ah    ; so don't ask me what
               OUT     42h, AL  ; the hell
               IN      AL,61h   ; is going on.
               OR      AL,00000011b     ; enable speaker
               OUT     61h, AL
               pop ax
               RET
;SOUND          ENDP

spkoff:
               mov soundon,0
               mov     ax, port_off
               out     61h, al
ret


PRINT:
               MOV     AH,9
               INT     21h
               RET
Byte2Str:
           mov DI,AX                ; Duplicate byte in DI
           and DI,000FH             ; Mask out high 12 bits of DI
           mov BX,OFFSET Digits     ; Load offset of Digits into DI
           mov AH,BYTE PTR [BX+DI]  ; Load digit from table into AH
           mov [SI+1],AH            ;   and store digit into string
           xor AH,AH                ; Zero out AH
           mov DI,AX                ; And move byte into DI
           shr DI,4                 ; Shift high nybble of byte to
           ;shr DI,1                 ;   low nybble
           ;shr DI,1
           ;shr DI,1
           mov AH,BYTE PTR [BX+DI]  ; Load digit from table into AH
           mov [SI],AH              ;   and store digit into string
           ret                      ; We're done--go home!
;Byte2Str 


; Data Bytes - the D_s (Or, Pseudo-ops, as the book calls them)
Outmessage    db  crlf,crlf,'(c) 1995 Tom Murphy. (Frood)',crlf
db 'Freely distribute! Comments: IMightBeTM@aol.com',crlf,'$'
digits db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
xspeed dw 1
yspeed dw 1
include eye.inc
leftwords db 'Left score:  '
lscorestring db '**h$'
rightwords db crlf,'Right score: '
rscorestring db '**h$'
speed dw 200
port_off dw ?
rdont db ?
ldont db ?
height dw ?
bwidth  dw ?
putit dw ?

_TEXT          ENDS
               END     START
