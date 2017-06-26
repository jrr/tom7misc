; Shooter... the generic Arcade Shooter Game!
_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME CS:_TEXT               
               ASSUME DS:_TEXT
;               assume fs:@dataseg2
org 100h
.386
;.model compact
;.stack 100h

;.code
START:         JMP     MAIN

; Constants - the EQUs (The book calls them "Equates")
CR             EQU     13
LF             EQU     10
crlf           equ     13,10

;              CODE AREA
;              ---------
MAIN           PROC    NEAR

; Allocate da memory we need...
xor ax,ax               
mov  ah, 48h ; put the function code in AH
mov  bx, 4000  ;block size in paragraphs - I think I did this right but you
               ;should definitely check
int 21h    ; call the function
;jc error

;AX now contains the segment address of the allocated block.

;mov  screenptr,ax  ; ptr now contains the segment address.

; Speaker setup...              
               in     al, 61h
               mov     port_off,al

               ; init the screen buffer (to ALL BLACK)

 ;              mov ax,screenptr
 ;              mov es,ax
 ;              xor di,di
 ;              xor ax,ax
  ;             mov cx,32
  ;             rep stosw

xor ax,ax
xor bx,bx
xor cx,cx
xor dx,dx
xor di,di

; go into screen 13               
               
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
               mov dx,3c9h
               ;call getnextal
               mov al,[pal+bx]
               out dx,al
               mov al,[pal+bx+1]
               out dx,al
               mov al,[pal+bx+2]
               out dx,al
               inc cx
               add bx,3
               cmp cx,0ffh
               jna toppal               

  ; *****
               xor di,di
               xor ax,ax
               xor bx,bx
               xor cx,cx  
               xor dx,dx
     
floop:               
               
               ; key was pressed            
               mov ax,startx
               add ax,speedx
               cmp ax, 4242h
               jb leftok
               ; went over on the left.
               xor ax,ax
               mov speedx,0
leftok:        cmp ax,275
               jb rightok
               mov ax,275
               mov speedx,0
rightok:       mov startx,ax
               mov ax,starty
               add ax,speedy
               mov starty,ax
               cmp ax, 4242h
               jb Topok
               ; went over on the Top
               xor ax,ax
               mov speedy,0
Topok:         cmp ax,185
               jb bottomok
               mov ax,185
               mov speedy,0
bottomok:      mov starty,ax

; Put the shot...
               mov cx,maxshots
               mov ax,offset shotg
               mov picture,ax
               xor si,si
               mov ax,0a000h
               mov es,ax
topshots:               
               mov ax,[shots.x+si]
               cmp ax,0
               je noshot
               
               add ax,3
               cmp ax,308
               jb putit
          ;went off, remove shot...
          mov word ptr [shots.x+si],0
          dec numshots
          jmp noshot
putit:
               mov [shots.x+si],ax
               mov picx,ax
               mov ax,[shots.y+si]
               mov picy,ax
               call putmsg
noshot:
               add si,shotstrucsize
               dec cx
               jnz topshots
               ;loop topshots

; do stars
               mov cx,maxstars
               xor si,si
topstars:               
               mov ax,[stars.x+si]
               cmp ax,0
               je nostar
               
               mov bx,[stars.speed+si]
               sub ax,bx
               
               cmp ax,2
               ja putitt 
          ;went off, remove star...
          mov word ptr [stars.x+si],0
          dec numstars
          jmp nostar
putitt:
               mov [stars.x+si],ax
               ;xor di,di
               mov bx,[stars.y+si]
               mov ax,320
               imul bx
               add ax,[stars.x+si]
               mov di,ax
               mov [stars.olddi+si],di
               ;mov al,15
               mov al,[stars.color+si]
               stosb
nostar:
               add si,starstrucsize
               dec cx
               jnz topstars
               ;loop topstars


               ; put ship...
               mov ax,offset ship
               mov picture,ax
               mov ax,startx
               mov picx,ax
               mov ax,starty
               mov picy,ax
               call putmsg
               
               ; Wait for the VRT... just a timing function, really...
               call waitretrace
               ; remove ship; no need to reset args, they're already
               ; set from above.
               call remmsg
               
               mov cx,maxshots
               mov ax,offset shotg
               mov picture,ax
               mov ax,0a000h
               mov es,ax
               xor si,si
topsloop:      
               mov ax,[shots.x+si]
               cmp ax,0
               je noshot2
        
               mov picx,ax
               mov ax,[shots.y+si]
               mov picy,ax
               call remmsg
noshot2:
add si,shotstrucsize
dec cx
jnz topsloop
;loop topsloop

               mov cx,maxstars
               xor si,si
topstloop:      
               mov ax,[stars.x+si]
               cmp ax,0
               je nostar2
               
               mov di,[stars.olddi+si]
               ;mov di,ax
               xor al,al
               stosb
nostar2:
add si,starstrucsize
dec cx
jnz topstloop
;loop topstloop

               cmp soundon,0
               je soundoffy

               cmp soundcycles,0
               jne soundonny
               ; shut the goddamned sound OFF
               mov     al,port_off
               out     61h, al
               mov     soundon,0
soundonny:
               dec soundcycles
soundoffy:
               call makestar
               jmp keystuff
               
terminate:
               mov     al,port_off
               out     61h, al
               
; deallocate mem
;mov ax,screenptr
;mov es,ax
;xor ax,ax
;mov  ah, 49h ; put the function code in AH
;int 21h

;error:
               
xor bx,bx
mov ah,59h
int 21h
cmp ax,7
jne endo

               MOV     AX,0003
               INT     10H
               

               mov dx, offset outmessage
;               call print
endo:
    mov     ax,4c00h        ;return control to DOS
    int     21h

MAIN           ENDP
; ----------===== SUBS ======----------
PRINT:
               xor ax,ax
               MOV     AH,9
               INT     21h
               RET
keystuff:               
               XOR     AX,AX
               MOV     ES,AX
               MOV     AX,ES:[41Ah] ; loop if no key waiting.
               CMP     AX,ES:[41Ch] 
               JZ      fLoop
               ; key pressed...
               
foloop:

               XOR     AX,AX
               INT     16h
               
               ; do key stuff
               
               ; check quit
               cmp al,27 ; ESC
               je terminate
               cmp ah,72 ; up
               jne n1
               dec speedy
n1:            cmp ah,75
               jne n2
               dec speedx
n2:            cmp ah,77
               jne n3
               inc speedx
n3:            cmp ah, 80
               jne n4
               inc speedy
n4:            cmp ah, 57
               jne keydone
               jmp makeshot
keydone:       ; done parsing keys, go back
               xor ax,ax ; just for safety
             jmp floop

makeshot:
    mov     ax,cs
    mov     es,ax
    mov     ds,ax

    cmp     [NumShots],MaxShots    ;is there room for another shot?
    jae     Floop

    ;search for 1st available slot

    xor     si,si
TryAgain:
    cmp     word ptr [Shots.X+si],0     ;is this slot empty?
    je      GotOne                      ;yes, go fill it

    add     si,ShotStrucSize
    cmp     si,MaxShots*ShotStrucSize
    jb      TryAgain
    ; All shot spots are full...
    jmp     Floop

GotOne:         
; higher BX = lower sound.
               mov bx,01500
               MOV     AL,10110110b         ; channel 2, wite LSB/MSB
               OUT     43h,AL   ; I don't understand this routine
               MOV     AX, BX   ; cuz I didn't write it.
               OUT     42h, AL  ; it's from a book.
               MOV     Al,Ah
               OUT     42h, AL
               IN      AL,61h
               OR      AL,00000011b     ; enable speaker
               OUT     61h, AL
               mov soundcycles,3     
               mov soundon,1
     
     mov bx,startx
     add bx,32
     mov [shots.x+si],bx
     mov bx,starty
     add bx,4
     mov [shots.y+si],bx
     ;mov word ptr [shots.olddi+si],0 ; it'll be set the first time around.
     inc numshots

noemptyspace:
               jmp floop

makestar:
    mov     ax,cs
    mov     es,ax
    mov     ds,ax

    cmp     [NumStars],MaxStars    ;is there room for another star?
    jae     NoEmptySpace2

    ; search for 1st available slot

    xor     si,si
TryAgain2:
    cmp     word ptr [Stars.X+si],0     ;is this slot empty?
    je      GotOne2                     ;yes, go fill it

    add     si,StarStrucSize
    cmp     si,MaxStars*StarStrucSize
    jb      TryAgain2
    ; All shot spots are full...
    jmp     NoEmptySpace2

GotOne2:         ;si points to the record for the star to fill
     
     mov word ptr [stars.x+si],318
     call getrandomnumber
     and ax,0000000011111111b    
     cmp ax, 198 ; max 198...
     jb rrnd
     sub ax, 198 ; most it can be is 56
     shl ax, 1 ; most it can be is 106, which means that the top will have
               ; a few more stars, but it will be ok.
rrnd:      
     mov [stars.y+si], ax
     shr bx,3 ; div by *8*... no particular reason...
     and bx,0000000000000011b ; 0-3
     inc bl ; (so that it can't be 0) ;1-4
     mov [stars.speed+si], bx
     mov al,28
     shl bl,1 ; 2-8
     sub al,bl ; get color index
     mov [stars.color+si],al
     ;mov word ptr [stars.olddi+si],0 ; it'll be set...
     inc numstars

noemptyspace2:
               ret

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

Remmsg:
               ; passed vars same as putmsg...
               pusha
               mov ax,0a000h
               mov es,ax
               xor di,di
               xor ax,ax
               mov bx, picture
     mov al,[bx]
     inc bx
               add ax,picy
               mov height,ax
     mov al,[bx]
     inc bx
               add ax,picx
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
               imul picy
               add ax,picx
               mov di,ax
               mov cx,picx ; reset x coord
               mov dx,picy

donextline2:
               inc dx
               
doline2:        inc cx
     mov al,[bx]
     inc bx
               cmp al,0
               je skip
               xor al,al
               mov es:[DI],al
skip:     inc di
               ; check if we've gone to the end of the line
               cmp cx,bwidth
               jnae doline2
               xor di,di ; reset di
               mov ax,320
               push dx
               imul dx
               add ax,picx
               mov di,ax
               pop dx
               mov cx,picx ; reset x coord
               xor ax,ax ; reset ah to 0
               ; check if it was the last line
               cmp dx,height
               jnae donextline2

               ; done... I hope
               popa
ret


Putmsg:
               ; vars passed:
               ; picture = OFFSET of picture to be loaded
               ; picx    = Upper left hand corner
               ; picy    = coordinates.
               pusha
               mov ax,0a000h
               mov es,ax
               xor di,di
               xor ax,ax
               mov bx, picture
     mov al,[bx]
     inc bx
               add ax,picy
               mov height,ax
     mov al,[bx]
     inc bx
               add ax,picx
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
               imul picy
               add ax,picx
               mov di,ax
               mov cx,picx ; reset x coord
               mov dx,picy
donextline:
               inc dx
               
doline:        inc cx
     mov al,[bx]
     inc bx
               cmp al,0
               je skippy
               ;stosb
               mov es:[DI],AL
               ;inc di
skippy: inc di

               ; check if we've gone to the end of the line
               cmp cx,bwidth
               jnae doline
               xor di,di ; reset di
               mov ax,320
               push dx
               imul dx
               add ax,picx
               mov di,ax
               pop dx
               mov cx,picx ; reset x coord
               xor ax,ax ; reset ah to 0
               ; check if it was the last line
               cmp dx,height
               jnae donextline

               ; done... I hope
               popa
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

; -------------- DATA --------------
include palette.inc
include sgfx.inc
include shot.inc

maxshots equ 14
maxstars equ 140

NumShots   dw  0       ;number of shots active
NumStars   dw  0

yesmsg db 'Y',7,'e',7,'S',7,'.',7,0
nomsg db 'N',7,'o',7,'.',7,0
startx dw 50
starty dw 50
speedx dw 0
speedy dw 0
shotx  dw 0
shoty  dw 0
shoton db 0
soundon db 0
soundcycles db 0
RandSeed     DW 348Bh
RandSeed2    DW 7F34h
RandSeed3    DW 32bfh
outmessage db "Generic shooter game, (c) 1995 Tom Murphy. Alpha version.",crlf,"(DO NOT DISTRIBUTE!)",crlf,"$"
bwidth dw ?
height dw ?
port_off db ?    
picx dw ?    
picy dw ?
picture dw ?
screenptr dw ?


    Shot_Struc      STRUC
        X       dw  0
        Y       dw  0
        OldDi   dw  0       ;where to erase last dot
    Shot_Struc      ENDS

    ShotStrucSize = 8       ;number of bytes per entry

    Star_Struc      STRUC
        X       dw  0
        Y       dw  0
        OldDi   dw  0       ;where to erase last dot
        speed   dw  0
        color   db  0
    Star_Struc      ENDS

    StarStrucSize = 11      ;number of bytes per entry

Shots       Shot_Struc MaxShots DUP (<0,0,0>) ;where all the data is held
filler db maxshots*shotstrucsize dup (0) ; because the strucs don't work
Stars       Star_Struc maxstars dup (<0,0,0,0,0>)
filler2 db maxstars*starstrucsize dup (0) ; for some reason... I ought to
                                        ; get a book!

; Da Virtual Screen...
_TEXT          ENDS

               END     START
