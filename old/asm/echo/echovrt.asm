; Tom's ECHO B&W flashing intro. 
; Copyright (c) 1996 Tom Murphy of EùCùHùO productions.
;
; ImightbeTM@aol.com - http://members.aol.com/imightbetm
;
; This code is licensed under the GNU Public License.
;
; I wrote this program on a dare. I told a friend that I could reproduce
; some other lame 8k intro in 2k. The main trick was to not store the
; full-resolution bitmap, but to generate it by anti-aliasing a smaller
; picture. Other tricks, like actual compression, could have made it
; tons smaller, I'm sure.
;
; The program first takes a 2 color bitmap array and displays it to the
; 320x200x256 screen (13). The reason for using the 2 color bitmap was
; to save space. Then it does some 2 color anti-aliasing, removing any
; especially sharp corners. The program then does 256 color anti-aliasing
; to soften the edges and give it a hint of 3D. Finally, it cycles color
; 0 between black and white while waiting for the user to press a key.
;
; For a neat effect, change the VRT delay or remove it entirely. On
; faster computers, you can get some really cool interference in the
; background while the ECHO logo stays. It looks like it's more complicated
; than it is, too. =)
;
; Please send questions, comments, interesting modifications, etc. to:
; ImightbeTM@aol.com
; And visit my web page:
; http://members.aol.com/imightbetm
;
; Have fun!

_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME  CS:_TEXT,DS:_TEXT
               ASSUME  ES:_TEXT,SS:_TEXT
               ORG     100H
.386
START:         JMP     MAIN

CR             EQU     13
LF             EQU     10
CRLF           EQU     13,10
CTRL_Z         EQU     26
SPACE          EQU     32


;              CoDE AREA
;              ---------
MAIN           PRoC    FAR
               
               Mov     ax,0013h               
               int     10h

               mov     ax,0a000h
               mov     es,ax
               
; set the palette
               xor ax,ax
               xor bx,bx
               xor cx,cx  
               xor dx,dx
               
               mov ax,001
               
paltop:                           ;And wow, look at all this wasteful
                                  ;code!!
               mov bx, 5
anotherpal:               
               MOV DX,3c8h
               xchg ax,cx
               out DX,al
               xchg ax,cx
               MOV dX,3c9h
               out dx,Al ;red
               out dX,Al ;green
               out dX,Al ;blue
               inc cx
               dec bx
               jnz anotherpal
               inc ax
               mov bx,ax
               ;putpixel bx,bx,cl

               cmp cx,0feh
               jnae paltop

               ; pallette is grey scale now
               
outpal:
               xor cx,cx
               xor ax,ax
               xor dx,dx
               xor bx,bx

loadpic:
     xor bx,bx
     mov cx,160
     mov di, 16000
tloadpic:     

     mov al,[echie+bx]
     ; al holds 8 bit bitmap. need to convert it to a bytemap.
     mov holdme,al
     push bx
     xor bx,bx
funloop:
     mov al,holdme
     and al,[masktable+bx]
     jz alzero
     mov al, 200
alzero:
     mov es:[di+320],al
     mov es:[di+321],al
     stosb
     stosb
     inc bx
     cmp bx,8
     jne funloop
     dec cx
     jnz nevermind
     add di, 320
     mov cx, 160
nevermind:
     pop bx
     inc bx
     cmp bx, 1350
     jnae tloadpic


; Bitmap anti-alias. Removes:

; [][][]                      [][][]
; []ÛÛÛÛ   type corners; to:  [][]ÛÛ
; []ÛÛÛÛ                      []ÛÛÛÛ

antialias:
mov bx, 25640
mov di, 15360
topaa:
               mov al,es:[di-1]
               cmp al,0
               jne aaskipittl
               mov ax,es:[di-320]
               cmp ax,0
               jne aaskipittl
               mov al,es:[di+319]
               cmp al,0
               jne aaskipittl
               mov byte ptr es:[di],0
aaskipittl:
               mov al,es:[di+1]
               cmp al,0
               jne aaskipittr
               mov ax,es:[di-320]
               cmp ax,0
               jne aaskipittr
               mov al,es:[di+321]
               cmp al,0
               jne aaskipittr
               mov byte ptr es:[di],0
aaskipittr:
               mov al,es:[di-1]
               cmp al,0
               jne aaskipitbl
               mov ax,es:[di+320]
               cmp ax,0
               jne aaskipitbl
               mov al,es:[di-321]
               cmp al,0
               jne aaskipitbl
               mov byte ptr es:[di],0
aaskipitbl:
               mov al,es:[di+1]
               cmp al,0
               jne aaskipitbr
               mov ax,es:[di+319]
               cmp ax,0
               jne aaskipitbr
               mov al,es:[di-319]
               cmp al,0
               jne aaskipitbr
               mov byte ptr es:[di],0
aaskipitbr:

               inc di
               dec bx
               jnz topaa

xor ax,ax
xor cx,cx


; And now do rad ass ditherin' on it:


; two dithers
mov dx,2
doadither:
mov bx, 25640
mov di, 15360
topdither:
               xor ax,ax
               xor cx,cx
               mov cl,es:[di-1]
               mov al,es:[di]
               add cx,ax
               mov al,es:[di+1]
               add cx,ax
               mov al,es:[di+320]
               add ax,cx
               shr ax,2  ; divide by 4   (average)
               mov es:[di],al
               inc di
               dec bx
               jnz topdither

dec dx
jnz doadither

xor ax,ax
xor cx,cx
waituntilover:     
     
     ; crappy palette rotation - 
               not ax
               MOV DX,3c8h
               xchg ax,cx
; let's do FOUR retraces cuz it's SO DAMN FAST!
mov bx,4
     push ax dx
aretrace:
     mov dx,3DAh
l1:
    in al,dx
    and al,08h
    jnz l1
l2:
    in al,dx
    and al,08h
    jz  l2
          dec bx               
          jnz aretrace
pop dx ax          
               out DX,al
               xchg ax,cx
               MOV dX,3c9h
               out dx,Al ;red
               out dX,Al ;green
               out dX,Al ;blue

     call checkkey
     cmp dx,0
     je waituntilover
     
; -----------------END-------------------------------------------------
TERMINATE:     MOV     AX,0003
               INT     10H
               
               MOV     DX, OFFSET Outmessage
               CALL    PRINT
               
               MOV     AX, 0008                ;Return to DOS.
               INT     21H

MAIN           ENDP
; ----------===== SUBS ======----------
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



; Data 
Outmessage    db  "Thanks for staring at this.",crlf,crlf,crlf
db "This was a EùCùHù0 (Tom Murphy) Production.",crlf,"$"

masktable db 10000000b
          db 01000000b
          db 00100000b
          db 00010000b
          db 00001000b
          db 00000100b
          db 00000010b
          db 00000001b

include echobw.inc
holdme db ?

_TEXT          ENDS
               END     START
