[org 0x0100]
jmp start
;cx holds end of area

oldisr:dd 0;keyboard
oldisr_timer:dd 0
location_ball: dw 0

scorepos: dw 30*2*132
score:dw 0
score_string: db 'Score: ',0
score_string_length: dw 7
scoreupdate:db 1
width:dw 132
height: dw 43
area:dw 0
screenmax:dw 11352

minutes: dw 0
seconds: dw 0
tickcount:dw 0
timerpos: dw 31*2*132-8

gameover:dw 0

brickstartpos: dw 0
brickamount: dw 0
brickwidth: dw 0
brickhalf: dw 0
brickdirection:dw 0,2,0,-4;preserveorder
tile_height:dw 0

bricktimer: dw -1
carotstartpos:dw 0

instruction:db '22L.6741  22L.6654-Game Instructions:-Use Up Key to Jump.-Collect Carrots for points, 1 point for each.-Orange Tiles move left and right.-Blue Tiles break after 3 seconds.-Tiles and Carrots are generated randomly.-Press Any Key to Start Game',0
quit_info: db '-     Quit     -Yes         No',0
game_over_msg:db 'Gameover!-',0
quit:dw 0
quitmenu:dw 0

RANDGEN:         ; generate a rand no using the system time
push ax
push cx
RANDSTART:
       
	rdtsc
   xor  dx, dx
   mov  cx, 12   
   div  cx       ; here dx contains the remainder of the division - from 0 to 9  
pop cx
pop ax
RET  

instruction_screen:
		push bp
		mov bp,sp
		push es
		push ax
		push cx
		push si

		
		mov ax, 0xb800
		mov es, ax ; point es to video base

		
		mov bx, [bp+4] ; point si to string
		mov ah,0x17 ; load attribute in ah

mov cx,0
mov si,di
next_instruction_char: 
			mov al,[bx] ; load next char in al
			inc bx
			
			cmp al,0
			jz exit_print_instruction
			
			cmp al,'-'
			jnz no_nextline
			
			cmp cx,0
			jnz nextfont
			mov ah,0x46
			jmp normal_nextline
			
			nextfont:
			cmp cx,1
			jnz normal_nextline
			mov ah,0x07
			normal_nextline:
			add si,[width]
			add si,[width]
			mov di,si
			inc ah
			inc cx
			jmp next_instruction_char
			no_nextline:
			mov [es:di],ax ; print char/attribute pair
			add di,2
			jmp next_instruction_char
exit_print_instruction:
		pop si
		pop cx
		pop ax
		pop es
		pop bp
RET 2

printscore:;assistivefunc
			push ax
			push cx
			push si

									; es:di --> b800:0000
			mov si, score_string			; point si to string
			mov cx, [score_string_length]			; load length of string in cx
			mov ah, 0x27			; normal attribute fixed in al
			
nextchar:	mov al, [si]			; load next char of string
			mov [es:di], ax			; show this char on screen
			add di, 2				; move to next screen location
			add si, 1				; move to next char in string			
			loop nextchar			; repeat the operation cx times
			mov byte[scoreupdate],0
			pop si
			pop cx
			pop ax
			ret 
			
printnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	
	mov di, [timerpos] ; point di to 70th column
	mov ax, [minutes] ; load number in ax
	cmp ax,0
	jz endmin

	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit_min: mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit_min ; if no divide it again
	nextpos_min: pop dx ; remove a digit from the stack
	mov dh, 0x27 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos_min ; repeat for all digits on stack
	mov dl,':'
	mov [es:di],dx
	add di,2
	jmp startsec
	
	endmin:
	mov dl,'0'
	mov dh,0x27
	mov [es:di],dx
	mov dl,':'
	mov [es:di+2],dx
	add di,4
	
	startsec:
	mov ax, [seconds] ; load number in ax
	cmp ax,0
	jz endseconds

	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit_sec: mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit_sec ; if no divide it again
	nextpos_sec: pop dx ; remove a digit from the stack
	mov dh, 0x27 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos_sec ; repeat for all digits on stack
	jmp endprint_time
	endseconds:
	mov dl,'0'
	mov dh,0x27
	mov [es:di],dx
	
	endprint_time:
	
	cmp byte[scoreupdate],0
	jz skipscoreupdate
	; mov di,500
	; mov ax,0x720
	; mov [es:di],ax
	mov di,[scorepos]
	call printscore;increments di
	
	mov ax, [score] ; load number in ax

	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit_score: 
	mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit_score ; if no divide it again
	nextpos_score:
	pop dx ; remove a digit from the stack
	mov dh, 0x27 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos_score ; repeat for all digits on stack
	skipscoreupdate:
	
	pop di
	pop dx
	pop cx
	pop bx
	pop ax 
	pop es
	pop bp
	ret 
;------------------------------------------------------
; timer interrupt service routine
;------------------------------------------------------
timer:		push ax

			cmp word[cs:quitmenu],1
			jz exit_timer
			
			inc word [cs:tickcount]; increment tick count
			cmp word [cs:tickcount],18
			jle print_time
			
			cmp word[cs:bricktimer],-1
			jz normaltimeinc
			dec word[cs:bricktimer]
			jnz normaltimeinc
			mov word[gameover],1
			
			normaltimeinc:
			inc word [cs:seconds]
			mov word [cs:tickcount],0
			
			cmp word[cs:seconds],60
			jnz print_time
			inc word [cs:minutes]
			mov word[cs:seconds],0
			
			print_time:
			call printnum ; print tick count
			
			
			exit_timer:
			mov al, 0x20
			out 0x20, al ; end of interrupt
			pop ax
			iret ; return from interrupt
kbisr: 
	push di
	push ax
	push es
	mov ax, 0xb800
	mov es, ax ; point es to video memory
	
	in al, 0x60 ; read a char from keyboard port
	
	cmp word[cs:quitmenu],1
	jz nomatch
	
	cmp al, 0x48 ; upkey
	jz exit1 ; leave interrupt routine

	cmp al,1
	jz exit2

nomatch:
	pop es
	pop ax
	pop di
	jmp far [cs:oldisr] ; call the original ISR

; exit3:

	; mov al, 0x20
	; out 0x20, al
	; jmp exit
	
exit1:
	call bounce_decider
	mov al, 0x20
	out 0x20, al
	jmp exit
exit2:

	mov al, 0x20
	out 0x20, al
	call quitscreen_buffer
	cmp word[quit],1
	jnz exit
	mov word[gameover],1

exit:
	pop es
	pop ax
	pop di
	iret ; return from interrupt
	
moverow_left_right:
push bp 
mov bp,sp
pusha

mov ax,0xb800
mov es,ax
mov di,[brickstartpos]
mov cx,[bp+4]
cmp cx,0
jg rightmovefirst_tile
jl leftmovefirst_tile
jmp endrowmovement
rightmovefirst_tile:

		add di,[width]
		add di,[width]
		sub di,2
		
		mov si,di
		
		rowmovementsize_right_push:
		push word[es:si]
		sub si,2
		loop rowmovementsize_right_push
		mov cx,[bp+4]
		
		mov dx,cx
		rightshifter:
		
		mov ax,[es:si]
		mov [es:di],ax
		
		sub si,2
		sub di,2
		inc dx
		cmp dx,[width]
		jnz rightshifter

		rowmovementsize_right_pop:
		pop word[es:di]
		sub di,2
		loop rowmovementsize_right_pop
		jmp endrowmovement
leftmovefirst_tile:
	mov ax,-1
	xor [bp+4],ax
	inc word[bp+4]
	mov cx,[bp+4]
	
	mov [bp+4],cx
	
	mov si,di
	rowmovementsize_left_push:
		push word[es:si]
		add si,2
		loop rowmovementsize_left_push
	mov cx,[bp+4]
	mov dx,cx
	leftshifter:
	
		mov ax,[es:si]
		mov [es:di],ax
		
		add si,2
		add di,2
		inc dx
		cmp dx,[width]
		jnz leftshifter
		
		rowmovementsize_left_pop:
		pop word[es:di]
		add di,2
		loop rowmovementsize_left_pop
		
	endrowmovement:
popa
pop bp
ret 2


bounce_decider:

mov ax,0xb800
mov es,ax


mov ax,[screenmax]
sub ax,264

cmp word[location_ball],ax
jg bounce_first

jmp bounce_normal

bounce_first:
push ax
push bx
push cx
push dx
push si
push di


mov di,[location_ball]
mov si,di
sub di,[width]
sub di,[width]
mov bx,[tile_height]
	uploop:
		call bigdelay
		mov ax,[es:di]
		cmp al,0;checkfortile
		jnz normal
		mov cx,[es:si]
		mov [es:di],cx
		mov [es:di+2],cx
		
		mov dl,32
		mov dh,00100000b
		
		mov [es:si],dx
		mov [es:si+2],dx
		call bigdelay
		
		sub di,[width]
		sub di,[width]
		sub si,[width]
		sub si,[width]
		
		mov dx,[es:di]
		cmp dl,'*'
		jz carrotfnd
		mov dx,[es:di+2]
		cmp dl,'*'
		jnz nocarrot
		carrotfnd:
		;mov word[carrot+6],0
		inc word[score]
		mov byte[scoreupdate],1
	
		nocarrot:
		mov [es:di],cx
		mov [es:di+2],cx
		
		mov [es:si],ax
		mov [es:si+2],ax
		cmp ah,0x10
		jnz endbounce_first
		;mov [es:si+2],cx
		mov word[bricktimer],3
		jmp endbounce_first
		normal:
		mov cx,[es:si]
		mov [es:di],cx
		mov [es:di+2],cx
		
		mov [es:si],ax
		mov [es:si+2],ax
		sub di,[width]
		sub di,[width]
		sub si,[width]
		sub si,[width]
		sub bx,1
		cmp bx,0
		jz game_over
		jmp uploop
game_over:
	mov word[gameover],1
endbounce_first:
		mov [location_ball],di
		
pop di
pop si
pop dx
pop cx
pop bx
pop ax
ret

bounce_normal:
mov word[bricktimer],-1
push word[location_ball]
call bounce_first
pop word[location_ball]

call bigdelay
call bigdelay
call scrollball

ret


scrollball:
push bp
push 0
mov bp,sp
push ax
push bx
push cx
push dx
push si
push di

xor di,di
push word[height]
call incrementer
mov bx,di

			mov ax, [width] ; load chars per row in ax
			mul word[tile_height] ; calculate source position
			mov dx,ax
			sub dx,[width];tile-1
			mov [bp],dx
			;mov dx,[width]
			shl ax, 1 ; convert to byte offset
			mov si, bx
			sub si,2; last location on the screen
			sub si, ax ; load source position in si
			
			
			push ax
			mov ax,[area]
			sub ax,[tile_height]
			dec ax
			mul word[width]
			mov cx,ax
			pop ax
			
			mov di,si
			mov ax,[bp]
			add di,2
			store:
			push word[es:di]
			add di,2
			dec ax
			jnz store
			
			mov di, bx
			sub di, 2 ; point di to lower right column
			mov dx,0
			mov_:
				mov ax,[es:si]
				cmp ah,0x27
				jz skipcopy
				cmp al,'*'
				jnz normal_mov_
				mov word[es:si],0x2000
				normal_mov_:
				mov [es:di],ax
				skipcopy:
				sub di,2
				sub si,2
				inc dx
				cmp dx,[width];worksonlyonce
				jnz nodelayanimate
		
				call bigdelay
				nodelayanimate:
				dec cx
				jnz mov_
			
			; mov ax,[area]
			; add ax,[area]
			; add ax,2
			; push ax
			; call incrementer
			
			
			mov cx,[bp]
			
			cmp word[tickcount],9;randomizer
			jge nextr1
			mov byte[bp],01100000b
			mov byte[bp+1],1
			jmp restore
			
			nextr1:
			cmp word[tickcount],14
			jge nextr2
			mov byte[bp],01110000b
			mov byte[bp+1],0
			jmp restore
		
			nextr2:
			cmp word[tickcount],18
			mov byte[bp],00010000b
			mov byte[bp+1],1
			restore:
			pop ax
			cmp al,0
			jnz normalrestore
			mov ah,[bp]
			normalrestore:
			mov word[es:di],ax
			sub di,2
			dec cx
			jnz restore
			
			
			mov byte[scoreupdate],1;reprint
			
			mov ax,[brickdirection+2]
			mov bx,[brickdirection]
				mov [brickdirection+2],bx
			mov bx,[brickdirection+4]
			mov [brickdirection+4],ax
				mov [brickdirection+6],bx
			
			
			
			repeatuntilnonzero:
				call RANDGEN
				cmp dx,0
				jz repeatuntilnonzero
			mov bx,0
			mov ax,[tickcount]
			cmp ax,0
			jpe positivedirection
			mov ax,0
			sub ax,dx
			push ax
			call moverow_left_right
			jmp setdirection
			positivedirection:
			mov ax,0
			mov ax,dx
			push ax
			call moverow_left_right
			
			
			setdirection:
	
			mov bx,[brickwidth]
			add bx,[brickwidth]
			add bx,[brickwidth]
			shr bx,1
		
			
			cmp byte[bp+1],0
			jz zeroset
			
			call RANDGEN
			cmp dx,0
			jpe negmovement
			
			cmp ax,0
			jl pos_neg
			pos_pos:
			
			sub bx,ax
			mov ax,bx
			jmp set_zero
			pos_neg:
			sub bx,ax
			mov ax,bx
			jmp set_zero
			negmovement:
			mov dx,0
			sub dx,bx
			mov bx,dx
			
			cmp ax,0
			jl neg_neg
			neg_pos:
			
			sub bx,ax
			mov ax,bx
			jmp set_zero
			
			neg_neg:
			sub bx,ax
			mov ax,bx
			jmp set_zero
			zeroset:
				mov ax,0
			set_zero:
			mov [brickdirection],ax
	
			;----
			test word[tickcount],1
			jnz skipcarrot
			cmp word[tickcount],0
			jnp skipcarrot
			
			call RANDGEN
			shl dx,1
			mov di,[carotstartpos]
			cmp word[brickdirection],0
			jg carrot_right
			sub di,dx
			sub di,dx
			jmp carrotskip
			carrot_right:
			add di,dx
			add di,dx
			
			carrotskip:
			mov al,'*'
			mov ah,00101110b
			mov [es:di],ax
			;mov [carrot],di
			skipcarrot:
			
pop di
pop si
pop dx
pop cx
pop bx
pop ax
add sp,2
pop bp
ret 

incrementer:;oneparameter for incrementing di
push bp
mov bp,sp
push ax
push dx

mov ax,[width]
shl ax,1
mul word[bp+4]
add di,ax

pop dx
pop ax
pop bp
ret 2
;---------------
bigdelay:
push dx
push cx
				mov dx,4
				mov cx,0xFFFF
				delay1:
					loop delay1
					dec dx
					jnz delay1
pop cx
pop dx
					ret
					
;------------------
building:
		push bp
		push 0
		push 0
		mov bp,sp
		push bx
		push ax
		push di
		push si
		push dx
		cmp di,[bp+8] ;willnotworkwithoutincrementer bcz di>4*width in rows>2
		jge endbuilding
		push word[bp+10]
		call incrementer
		;mov bx,264mov dx,0mov ax,didiv axsub ax,14shr ax,1mov dx,ax ;always 1byte answer
		
		mov bl,0x4F;0 code
		mov bh,01110110b
		mov ah,01110000b
		mov al,32
		;-------cofigure
			mov word[bp+2],6;building width
		;--------------
			
			mov dx,[bp+2]
			shr dx,1
			mov [bp],dx;midofbuilding
			mov dx,0
			test word[bp+2],1
			jz column
			add word[bp],1
column:
			mov si,[bp+2]

			push di
			buildingwidth:
				mov word [es:di], AX
							cmp si,[bp]
							jnz nowindow
							test dx,1 
							jz window	;windows on every even occurence
							jmp skipwindow
							window:
								mov word [es:di], BX
							skipwindow:
							inc dx
				nowindow:
				add di,2
				dec si
				jnz buildingwidth
			pop di
			
			add di, [width]
			add di,[width]
			
			cmp di, cx				; 132x14x2
			jb column					; if no clear next position
			
	endbuilding:
		pop dx
		pop si
		pop di
		pop ax
		pop bx
		
		add di,[bp+2]
		add di,[bp+2]
		
		add sp,2
		add sp,2
		pop bp
		ret 4
;--------------------------
background:
	push bp
	push 0
	push 0
	push 0
	mov bp,sp
	push di
	
	mov ah,00110000b
	mov al, 32
sky:
	mov di,0
	push cx
	shr cx,1
	rep stosw
	pop cx
	
	
	;shl cx,1;for building
backgroundscenary:
	mov di,[width];1row top indent
	add di,[width]
	
	mov word[bp+4],di
	add word[bp+4],di
	sub word[bp+4],8 ;edge check
;;;;-----cofigure
	mov word[bp+2],2;depth of side building
	mov word[bp],4;depth of small building
;;;;;;-----
	
	add di,8;some indentation
	tallbuildings:
		
		push 0
		push word[bp+4]
		call building
		
		sidebuilding:
		cmp di,0
		jp skipside
		
		push word[bp+2]
		push word[bp+4]
		call building
			
		skipside:
		push 0
		push word[bp+4]
		call building
		cmp di,0
		
		jp diffspacing
		add di,4;building spacing
		jmp smallbuilding
		diffspacing:
		add di,8
		
		smallbuilding:
		cmp di,0
		jnp skipsmallbuildings
		
		push word[bp]
		push word[bp+4]
		call building
		
		cmp di,0
		jp skipsmallbuildings
		add di,4
		
		skipsmallbuildings:
	
			cmp di,[bp+4] ;2rows-limit

			jle tallbuildings
		
		pop di
		add sp,2
		add sp,2
		add sp,2
		pop bp
		ret 

;--------------------------------
foreground:

	push bp
	mov bp,sp
	mov ax,[width]
	mov dx,[bp+4]
	add dx,4
	mul dx
	shl ax,1
	mov di,ax
	
	mov ax,[width]
	mov dx,[bp+4]
	sub dx,2
	mul dx
	shl ax,1
	add ax,di;add section area to prev area for cx
	mov cx,ax
	

	mov bx,[width]
	add bx,[width]
	
	sub cx,bx
	mov si,cx
	
	mov dx,[width]
	
	mov al,'-'
	mov ah,00001111b
firstlastline:	
			mov word [es:di], AX	; clear next char on screen
			mov word [es:si], AX
			add di, 2					
			add si, 2					; move to next screen location
			dec dx
			jne firstlastline	
	
	mov al,32
	mov ah,01110000b
	mov dx,0
	mov word[bp],0

	road_body:
		
		mov word [es:di], AX	; clear next char on screen
		add di, 2		; move to next screen location
		add dx,2
		cmp dx,bx;rowendcount
		jnz skiprowcount
		mov dx,0
		inc word[bp];row count
		skiprowcount:
		cmp di, cx			; 132x14x2
		jnz road_body		; if no clear next position
	
	
	
	shr word[bp],1
	mov ax,[width]
	mov si,[bp+4]
	add si,4;backgroundarea add
	add si,word[bp];will not overflow
	mul si
	shl ax,1
	mov di,ax
	
	mov cx,ax
	add cx,bx 
	
	mov dx,[bp];

	mov ah,00001111b
	mov al,'-'
	road_lines:
		
		roadloop: 
			mov word [es:di], AX	; clear next char on screen
			mov si,di
			add si,bx
			mov word [es:si], AX ;for thickness
			add di, 2					; move to next screen location
			
			cmp di, cx
			jge roadmarking
			
			dec dx
			jnz roadloop
			
			mov dx,[bp+4]
			shr dx,1
			
			
			mov dx,[bp]
			jmp road_lines		; inner loop will break loop
	
	roadmarking:
	
	
	shr word[bp],1
	
	mov ax,[width]
	mov si,[bp+4]
	add si,4;backgroundarea add
	
	add si,word[bp+4];will not overflow
	sub si,[bp]
	sub si,1
	mul si
	shl ax,1
	mov si,ax;firstmarking
	
	push si
	mov ax,[width]
	mov si,[bp+4]
	add si,4;backgroundarea add
	
	add si,word[bp];will not overflow
	mul si
	shl ax,1
	mov di,ax;2nd marking
	
	pop si
	;---------cofigure
	mov word[bp],4;road line length
	;----------
	mov dx,[bp];
	mov cx,bx;holdswidth
	add cx,di
	
	mov ah,01111111b
	mov al,'-'
	
	add di,2
	add si,2;indentation
	jmp endforeground;skiplinemarking
	
	road_marking:
		
		roadmarkloop: 
		
			mov word [es:di], AX	; clear next char on screen
			mov word [es:si], AX ;for thickness
			add di, 2					; move to next screen location
			add si, 2
			
			cmp di, cx
			jge endforeground
			
			dec dx
			jnz roadmarkloop
			
			mov dx,[bp]
			;---------cofigure
			mov bx,8;gapmultipleof2
			;---------
			add di,bx
			add si,bx
			
			cmp di, cx
			jge endforeground
			jmp road_marking		; inner loop will break loop
			
	endforeground:
	
	mov al,' '
	mov ah,01100000b
	mov bl,' '
	mov bh,01010000b
	
	mov bx,[width]
	add bx,[width]

	add di,[width]
	test di,1
	je safe
	add di,1
	safe:
	;----------cofigure
	mov cx,1;carheight
	mov word[bp],6;carwidth
	;-----------
	
;	sub di,bx
;	sub di,bx
	
	carheight:
	push di
	mov dx,[bp]
		cardraw:
			mov [es:di],ax
			add di,2
			dec dx
			jnz cardraw
			cmp cx,1
			jnz mainbody
			mov [es:di],bx
			mov [es:di+2],bx
			sub di,[bp]
			sub di,[bp]
			mov [es:di-2],bx
			mov [es:di-4],bx
			mainbody:
			pop di
			add di,bx
			dec cx
			jnz carheight
			
	mov ah,0
	
	mov [es:di],ax
	mov [es:di-2],ax
	
	add di,[bp]
	add di,[bp]
	mov [es:di],ax
	mov [es:di-2],ax
	
pop bp
ret 2
;---------------------------------------
reserved:
	push bp
	push 0
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	
	mov ax,0xb800
	mov es,ax
	
	;push di
call bigdelay
call bigdelay
	mov ax,[width]
	mov cx,[bp+6]
	add cx,[bp+6]
	add cx,2
	mul cx
	shl ax,1
	mov di,ax
	
	mov cx,[height]
	sub cx,[bp+6]
	sub cx,[bp+6]
	sub cx,2
	
	mov al,[width]
	mov ah,cl
	mul ah
	mov cx,ax
	
	push di;storeforlater
	mov al,32
	mov ah,00100000b
	rep stosw
	
	pop di
	
	mov ax,[width]
	mul word[height]
	mov cx,ax
	shl ax,1
	
	mov cx,ax
	push cx;storeforlater
	
	sub ax,di
	add ax,[width]
	;inc ax;rounds up 1 value
	
	mov dx,0;fordiv
	
	mov bx,[width]
	add bx,[width]
	
	add di,bx

call bigdelay
call bigdelay
	div word[width]
	shr ax,3;div4
	mov [tile_height],ax
	;-----
	mov dl,0
	mov dh,00010000b
	mov bl,0
	mov bh,01110000b
	mov byte[bp],11;tilewidthoddalways
	mov byte[bp+1],4;tileamount
	mov word[brickamount],4
	mov word[brickwidth],11;+1
	mov word[brickhalf],6
	;------------
	
	mov [brickstartpos],di
	mov ch,[bp+1]
	add di,[width];screenmid
	test di,1
	je evendi1
	inc di
	
	evendi1:
	
	push di
	sub di,[width]
	sub di,[width]
	mov [carotstartpos],di
	pop di
	evendi:
		
		push di
		mov si,di
		mov cl,[bp]
	call bigdelay
	call bigdelay
		test ch,1
		je changecolorcentretile
		mov [es:di],dx
		add di,2
		dec cl
		jmp tilewidth
		changecolorcentretile:
		mov [es:di],bx
		add di,2
		dec cl
		tilewidth:
			sub si,2
			test ch,1
			je changecolor
			
			mov [es:di],dx
			mov [es:si],dx
			jmp nochangecolor
			changecolor:
			mov [es:di],bx
			mov [es:si],bx
			
			nochangecolor:
			add di,2
			sub cl,2
			jnz tilewidth
			mov si,[es:di-2]
			mov [es:di],si;tilewidthwillbeeven oneextratileonright
			pop di
			push ax
			call incrementer
			
			dec ch
			jnz evendi
		
	pop di
	ballplace:
			sub di,[width];screenmid
			test di,1
			je ball
			inc di
	
	
	ball:
	mov dh,01010000b
	mov [es:di+2],dx
	mov [es:di],dx
	mov [location_ball],di
	;call scrollball
	
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	add sp,2
	pop bp
	ret 2
	
;--------------------------------------------------------------------
; subroutine to clear the screen
;--------------------------------------------------------------------
clrscr:		push es
			push ax
			push di

			mov ax, 0xb800
			mov es, ax				; point es to video base
			mov di, 0					; point di to top left column
			mov ah, 0x07
			mov al, 0

nextloc:	mov word [es:di], AX	; clear next char on screen
			add di, 2					; move to next screen location
			cmp di, 11352				; 132x43x2
			jne nextloc					; if no clear next position

			pop di
			pop ax
			pop es
			ret
;---------------------------


animate:	
			push bp
			
			push 0
			mov bp,sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			mov ax, 0xb800
			mov es, ax	
			
			mov dx,0
			mov ax,[height]
			mov cx,3
			div cx
			mov [area],ax
			
						
			add ax,4;more space for background
			mul word[width]
			mov cx,ax
			shl cx,1
			mov di, 0
			

call background


push word[area]
call foreground

		mov ax,[area]
		add ax,ax
		add ax,2
		mul word[width]
		shl ax,1
		mov di,ax ; point di to required location
mov ax,instruction
push ax
call instruction_screen;instructionmenu
mov ah,0
int 0x16


push word[area]
call reserved

	mov al,'*'
	mov ah,00101110b
	mov [es:9638],ax

mov bx,[brickwidth]
add bx,[brickwidth]
add bx,[brickwidth]
shr bx,1
push word[brickstartpos]

call bigdelay
call bigdelay
push -2
call moverow_left_right

call bigdelay
call bigdelay
add word[brickstartpos],792
push word[brickdirection+2]
call moverow_left_right
mov ax,bx
sub ax,[brickdirection+2]
mov word[brickdirection+2],ax

call bigdelay
call bigdelay
add word[brickstartpos],792
push 5
call moverow_left_right

call bigdelay
call bigdelay
add word[brickstartpos],792
push word[brickdirection+6]
call moverow_left_right
mov ax,0
sub ax,bx
sub ax,[brickdirection+6]
mov word[brickdirection+6],ax

pop word[brickstartpos]
			pop si
			pop di
			pop cx
			pop cx
			pop bx
			pop ax
			pop es
			add sp,2
			pop bp
			ret

move:
			push bp
			push 0
			push 0
			mov bp,sp
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			
			mov ax,0xb800
			mov es,ax
			
			
			mov dx,0
			mov ax,[height]
			mov cx,3
			div cx
			mov [bp],ax
			
			;mov [bp+2],ax
			;add word[bp+2],4;fixed addition
			
			add ax,4;more space for background
			;mul word[width]
			;mov cx,ax
			;shl cx,1
			
			call backgroundmovfunc
			call foregroundmovefunc
			jmp brickmovefunc
			
			backgroundmovfunc:
			mov cx,ax
			mov di,0
			backgroundloop:
					push cx
					
					add di,[width] ;last element
					add di,[width]
					sub di,2
					
					mov cx,[es:di]
					mov [bp+2],cx
					
		
					mov si,di
					sub si,2
					
					mov dx,[width]
					dec dx
					
					backgroundinnerloop:
						
						mov cx,[es:si]
						mov [es:di],cx
						
						sub di,2
						sub si,2
						dec dx
						jnz backgroundinnerloop
						
					mov cx,[bp+2]	
					mov [es:di],cx

					add di,[width]
					add di,[width]
					
					pop cx
					
					;cmp cx,di
				loop backgroundloop
				ret
;---------------------road
			
			foregroundmovefunc:
			mov cx,[bp]
			sub cx,2
			foregroundloop:
					push cx
					
					mov cx,[es:di]
					mov [bp+2],cx
					
		
					mov si,di
					add si,2
					
					mov dx,[width]
					dec dx
					
					foregroundinnerloop:
						
						mov cx,[es:si]
						mov [es:di],cx
						
						add di,2
						add si,2
						dec dx
						jnz foregroundinnerloop
					
					mov cx,[bp+2]	
					mov [es:di],cx

					add di,2
					;add di,[width]
					;add di,[width]
					
					pop cx
					
					;cmp cx,di
				loop foregroundloop
				ret
brickmovefunc:
		;call carrotmover
		mov di,0
		mov dx,0
		mov bx,brickdirection
		mov cx,4;[brickamount]
		mov di,[brickstartpos]
movebricks:
			cmp cx,0
			jnz normalmovement
			
			
			cmp word[bp],0
			jz endtilemov;skipsmovement
		
			mov si,[screenmax]
			sub	si,264
			cmp word[location_ball],si
			jge endtilemov
			
			mov di,[bp+2]
			sub di,[width]
			sub di,[width]
			
			mov si,[location_ball]
			
			cmp word[bp],0
			jg rightmovcheckcarrot
			
			mov ax,[es:si-2]
			cmp al,'*'
			jnz normalballanimateleft
			mov ax,[es:si]
			mov [es:si-2],ax
			mov word[es:si+2],0x2000
			sub word[location_ball],2
			
			jmp skipballanimate
			
			rightmovcheckcarrot:
			mov dx,[es:si+4]
			cmp dl,'*'
			jnz normalballanimateright
		
			mov ax,[es:si]
			mov word[es:si],0x2000
			mov [es:si+4],ax
			
			add word[location_ball],2
			jmp skipballanimate
			
			normalballanimateleft:
			mov ax,[es:si]
			mov [es:si-2],ax
			mov word[es:si+2],0x2000
			sub word[location_ball],2
			jmp endtilemov
			
			normalballanimateright:
			mov ax,[es:si]
			mov [es:si+4],ax
			mov word[es:si],0x2000
			add word[location_ball],2
			jmp endtilemov
			; normalballanimate:
			; add word[location_ball],2
			; cmp word[bp],0
			; jg rightmov
			; sub word[location_ball],4
			; jmp loopleftmov
			
			skipballanimate:
				 inc word[score]
				 mov byte[scoreupdate],1
				
				jmp endtilemov
				
		normalmovement:
		
			cmp cx,1
			jnz movdecider
			mov [bp+2],di
			mov ax,[bx]
			mov [bp],ax
			
			movdecider:
			cmp word[bx],0;+veforright
			jz skipleftreset;skipsmovement
			jg rightmov

	
	loopleftmov:
	
	; mov si,[location_ball]
	; mov ax,[es:si]
	; cmp al,'*'
	; jz gotcarrotleft
	
	push di
	
	mov si,di
	add si,2
	push word[es:di]
	mov dx,1
	leftshift:
	
		normalleft:
		mov ax,[es:si]
		mov [es:di],ax
		skipleftshift:
		add si,2
		add di,2
		inc dx
		cmp dx,[width]
		jnz leftshift
		pop word[es:di]
		pop di
		
		cmp cx,0
		jz endtilemov
			
		add word[bx],1
		cmp word[bx],0
		jnz skipleftreset
		mov ax,[brickwidth]
		add ax,[brickwidth]
		add ax,[brickwidth]
		mov word[bx],ax
		skipleftreset:
		add bx,2
		dec cx
		add di,792;+tileheight
		jmp movebricks
rightmov:
		push di
	
		add di,[width]
		add di,[width]
		
		sub di,2
		mov si,di
		sub si,2
		push word[es:di]
		mov dx,1
		mov word[es:di],0x720
		rightshift:
		
		normalright:
		mov ax,[es:si]
		mov [es:di],ax
		skiprightshift:
		sub si,2
		sub di,2
		inc dx
		cmp dx,[width]
		jnz rightshift
			
		pop word[es:di]
		pop di
		
		cmp cx,0
		jz endtilemov
		
		sub word[bx],1
		cmp word[bx],0
		jnz skiprightreset
		mov ax,0
		sub ax,[brickwidth]
		sub ax,[brickwidth]
		sub ax,[brickwidth]
		mov word[bx],ax
		skiprightreset:
		
		add bx,2
		dec cx
		add di,792;+tileheight
		
		jmp movebricks
		
; gotcarrotleft:
		; ;inc word[score]
		; ;mov word[scoreupdate],1
		
		; mov ax,[es:si+2]
		; mov word[es:si],ax
		; mov word[es:si+4],0x2020
		
		
endtilemov:

;--------------------------------------
				call bigdelay
				
			pop si
			pop di
			pop cx
			pop cx
			pop bx
			pop ax
			add sp,4
			pop bp

	ret
	
setisr:
push ax
		xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:9*4]
		mov [oldisr], ax ; save offset of old routine
		mov ax, [es:9*4+2]
		mov [oldisr+2], ax ; save segment of old routine
		cli ; disable interrupts
		mov word [es:9*4], kbisr ; store offset at n*4
		mov [es:9*4+2], cs ; store segment at n*4+2
		sti ; enable interrupts
		
		xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:8*4]
		mov [oldisr_timer], ax ; save offset of old routine
		mov ax, [es:8*4+2]
		mov [oldisr_timer+2], ax ; save segment of old routine
		cli ; disable interrupts
		mov word [es:8*4], timer; store offset at n*4
		mov [es:8*4+2], cs ; store segment at n*4+2
		sti ; enable interrupts
		
pop ax
ret

removeisr:
push ax
xor ax, ax

		mov es, ax ; point es to IVT base
		cli ; disable interrupts
		mov ax,[oldisr]
		mov word [es:9*4], ax ; store offset at n*4
		mov ax,[oldisr+2]
		mov [es:9*4+2], ax; store segment at n*4+2
		sti ; enable interrupts
		
		cli ; disable interrupts
		mov ax,[oldisr_timer]
		mov word [es:8*4], ax ; store offset at n*4
		mov ax,[oldisr_timer+2]
		mov [es:8*4+2], ax; store segment at n*4+2
		sti ; enable interrupts
		
pop ax
ret

quitscreen_buffer:
pusha
push 0
push 0
mov bp,sp
	mov word[quitmenu],1
		mov ax,0xb800
		mov es,ax
		
		mov ax,[area]
		add ax,ax
		add ax,2
		mul word[width]
		shl ax,1
		mov di,ax ; point di to required location
		mov [bp+2],ax 
		mov ax,[screenmax];screenlocations
		sub ax,di
		mov [bp],ax
sub sp,ax

		push bp
		mov cx,[bp]
		mov si,[bp+2]
		
		sub bp,2
		bufferstore:
		mov ax,[es:si]
		mov [bp],ax
		add si,2
		sub bp,2
		cmp si,[screenmax]
		jnz bufferstore
		pop bp
		

mov cx,[bp]
mov ax,0x720
cld
rep stosw
mov di,[bp+2]
mov ax,[bp]

		shr ax,1
		add di,ax
		test di,1
		jz di_even
		dec di
		di_even:
		sub di,8
		sub di,[width]
		sub di,[width]
		sub di,[width]
		sub di,[width]
		mov ax,quit_info
		push ax
		call instruction_screen
	mov si,di
	sub si,28
	
	makebold:
	cmp word[quit],1
	jnz bold_no
	or word[es:si],0x8000
	or word[es:si+2],0x8000
	or word[es:si+4],0x8000

	and word[es:di-2],0x7FFF
	and word[es:di-4],0x7FFF
	jmp looptillenter
	bold_no:
	
	or word[es:di-2],0x8000
	or word[es:di-4],0x8000
	
	and word[es:si],0x7FFF
	and word[es:si+2],0x7FFF
	and word[es:si+4],0x7FFF


	looptillenter:
		mov ah,0
		int 0x16
			leftrightloop:
				cmp ah,0x4B;left
				jnz nextkey1
				xor word[quit],1
				jmp makebold
				nextkey1:
				cmp ah,0x4D;right
				jnz nextkey2
				xor word[quit],1
				jmp makebold
		nextkey2:
		cmp ah,0x1c;enter
		jnz nextkey3
		jmp exitkeycheck
		nextkey3:
		cmp ah,1
		jnz looptillenter
		mov word[quit],0
		
		exitkeycheck:
	
		mov di,[bp+2]
		push bp
		sub bp,2
		bufferrestore:
		mov ax,[bp]
		mov [es:di],ax
		add di,2
		sub bp,2
		cmp di,[screenmax]
		jnz bufferrestore
		pop bp
		mov word[quitmenu],0
		
add sp,[bp]
add sp,4
popa
ret

gameover_screen:
pusha
mov ax,0xb800
mov es,ax
mov ax,[height]
mov bx,[area]
shr bx,1
sub ax,bx

mul word[width]
mov si,ax
shl si,1

mov ax,[width]
mul word[area]
mov cx,ax

mov ax,[area]
add ax,ax
add ax,2
mul word[width]
mov di,ax
shl di,1

mov ax,0x720
rep stosw

mov di,si
mov ax,game_over_msg
push ax
call instruction_screen
mov ax,score_string
push ax
call instruction_screen

	mov ax, [score] ; load number in ax

	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit_score2: 
	mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit_score2 ; if no divide it again
	nextpos_score2:
	pop dx ; remove a digit from the stack
	mov dh, 0x17 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos_score2 ; repeat for all digits on stack
	

popa
ret
;--------------------------------------------------------------------
start:	
; following code just changes your screen resolution to 43x132 Mode
mov ax, 0x54
int 0x10


;call clrscr
call animate
call setisr

looper:
call move
cmp word[gameover],1
jz endgame
jmp looper

endgame:
call removeisr

call gameover_screen

mov ah,0
int 0x16

mov ah,0
mov al,3
int 0x10

; ;looper:
; ;jmp looper


mov ax, 0x4c00 ; terminate program
int 0x21