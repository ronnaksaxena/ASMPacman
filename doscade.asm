TITLE PacMan Game
;INCLUDE CS240.inc
;rsaxena@hamilton.edu
DOS = 21h
TERMINATE=4C00h
.MODEL small, stdcall
.stack 200h

.8086

.code
;VARIABLES!!!



Board LABEL BYTE
            BYTE "Score: 00000                                                                    "
            BYTE "--------------------------------------------------------------------------------"
            BYTE "|                   | |                                   | |                  |"
            BYTE "|  . . . . . . .    | |  . . . . . . . . . . . . . . . .  | |   . . . . . . .  |"
            BYTE "|  .           .    | |  .                             .  | |   .           .  |"
            BYTE "|  .   +----+  .    | |  .  |=======================|  .  | |   .  +----+   .  |"
            BYTE "|  .   |    |  .    |_|  .  |=======================|  .  |_|   .  |    |   .  |"
            BYTE "|  .   |    |  .         .                             .        .  |    |   .  |"
            BYTE "|  .   |    |  . . . . . . . . . . . . . . . . . . . . . . . . .   |    |   .  |"
            BYTE "|  .   |    |  .  |=====|  .                         .  |=====| .  |    |   .  |"
            BYTE "|  .   |    |  .  |=====|  .  |===             ===|  .  |=====| .  |    |   .  |"
            BYTE "|  .   +----+  .           .  |                   |  .          .  +----+   .  |"
            BYTE "|  . . . . . . . . . . . . .  |                   |  . . . . . . . . . . .  .  |"
            BYTE "|  .           .           .  |                   |  .          .           .  |"
            BYTE "|  .   +----+  .  |=====|  .  |                   |  .  |=====| .  +----+   .  |"
            BYTE "|  .   |    |  .  |=====|  .  |===================|  .  |=====| .  |    |   .  |"
            BYTE "|  .   |    |  .           .            M            .          .  |    |   .  |"
            BYTE "|  .   |    |  . . . . . . . . . . . . . . . . . . . . . . . . .   |    |   .  |"
            BYTE "|  .   |    |  .         .                             .        .  |    |   .  |"
            BYTE "|  .   |    |  .    |=|  .  |=======================|  .  |=|   .  |    |   .  |"
            BYTE "|  .   +----+  .    | |  .  |=======================|  .  | |   .  +----+   .  |"
            BYTE "|  .           .    | |  .                             .  | |   .           .  |"
            BYTE "|  . . . . . . .    | |  . . . . . . . . . . . . . . . .  | |   . . . . . . .  |"
            BYTE "|                   | |                                   | |                  |"
            BYTE "-------------------------------------------------------------------------------", 0

Results LABEL BYTE
            BYTE "                               Good Playing Kid.                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                             Score:                                             "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                "
			BYTE "                                                                                ", 0


;223 dots on board
ticks WORD 0

lastKeyPressed BYTE 'x'
pacmanDir BYTE 0 ;0 is up, 1 is right, 2 is down, 3 is left, 4 is still
pacmanLocX BYTE 40 ;row, 
pacmanLocY BYTE 16 ; col
gameRunning BYTE 1 ;1 is running, 0 is game stopped running

;ghost 1 args
ghost1Timer WORD 12
ghost1dir BYTE 0 ;0 is up, 1 is right, 2 is down, 3 is left, 4 is still
ghost1LocX BYTE 35
ghost1LocY BYTE 12
ghost1DotFlag BYTE 0

;ghost 2 args
ghost2Timer WORD 12
ghost2dir BYTE 0 ;0 is up, 1 is right, 2 is down, 3 is left, 4 is still
ghost2LocX BYTE 45
ghost2LocY BYTE 12
ghost2DotFlag BYTE 0

;ghost 3 args
ghost3Timer WORD 12
ghost3dir BYTE 0 ;0 is up, 1 is right, 2 is down, 3 is left, 4 is still
ghost3LocX BYTE 40
ghost3LocY BYTE 14
ghost3DotFlag BYTE 0

score WORD 0 ;starts at 14 index
resultTimer WORD 50 ;how long we want results to display for

KBD_INTERRUPT = 09h
CLK_INTERRUPT = 1Ch
BIOS_CLK_INTERRUPT = 1Ch
PROG_END_INTERRUPT = 22h
spaceASCII = 32
	
INTERRUPT = BIOS_CLK_INTERRUPT
VideoMemory = 0B800h

ClockVector LABEL DWORD
ClockOffset WORD 0
ClockSegment WORD 0

OldVector WORD 0, 0


Timer_Data_Port = 42h
Timer_Control_Port = 43h
Speaker_Port = 61h
Ready_Timer = 0B6h

GetScaleFrequency PROC
	; note in dl
	; output in si
	push dx
	push bx
	mov bx, offset scale 
	mov si, offset scalefreq
top:
	cmp [bx], dl
	je done
	inc bx
	add si, 2
	loop top
done:
	pop bx
	pop dx
	ret
GetScaleFrequency ENDP

SpeakerMuted BYTE 0

SetInterruptVector PROC
	;; AL - interrupt number
	;; DS:DX - new handler
	;; AL - interrupt number
	;; DS:DX - new handler

	push	ax
	
	mov	ah, 25h
	int	DOS
	
	pop	ax
	ret
SetInterruptVector ENDP


GetInterruptVector PROC
	;; AL - interrupt number
	;; returns:
	;; ES:BX - interrupt
	push ax
	mov	ah, 35h
	int	DOS
	pop	ax
	ret
GetInterruptVector ENDP

SaveVector PROC
	;; AL - handler number
	;; DX - offset of DWORD to store vector
	push	bx
	push	si
	push	es

	call	GetInterruptVector
	mov	si, dx
	mov	[si], bx
	mov	[si + 2], es
	
	pop	es
	pop	si
	pop	bx
	ret
SaveVector ENDP



InstallHandler PROC
	;; AL - interrupt number
	;; CS:DX - new handler
	push	bx
	push	ds
	
	mov	bx, cs
	mov	ds, bx
	call	SetInterruptVector
	
	pop	ds
	pop	bx
	ret
InstallHandler ENDP


RestoreVector PROC
	;; AL - handler number
	;; DX - offset of DWORD containing vector
	push	dx
	push	si
	push	ds
	
	mov	si, dx
	mov	dx, [si]
	mov	ds, [si + 2]
	call	SetInterruptVector
	
	pop	ds
	pop	si
	pop	dx
	ret
RestoreVector ENDP




updateInput PROC
	;takes in offset of lastKeyPressed in dx
	pushf
	push ax
	push bx
	mov	ah, 01h ;checks for keystroke
	int	16h
	jz cap

	mov	ah, 00h ;reads in keystroke in al
	int	16h
	mov lastKeyPressed, al

	cap:
		pop bx
		pop ax
		popf
		ret
updateInput ENDP

musictick byte 0

gamenotes   BYTE    "bB12B12cCGECGEbB12B122EFF1GG3AB", 0

scale BYTE "bB12cCGEFA30", 0
scalefreq WORD 2415 ; b 	lower
		  WORD 1207 ; B 	upper
		  Word 1612 ; 1 	F#
		  Word 1917 ; 2 	D# or 2 or Eb
		  Word 2280 ; c 	lower
		  Word 1140 ; C 	higher
		  Word 1521 ; G
		  Word 1809 ; E
		  Word 1715 ; F
		  Word 1355  ;A
		  Word 2873 ; 3 	G# or 3 or Ab
		  Word 0 

tempoffset Word 0

Tick PROC
	inc	ticks
	inc musicTick
	dec ghost1Timer
	dec ghost2Timer
	dec ghost3Timer
	sti
	push ax
	push bx
	push cx
	push dx
	push si

	;check if game ended
	cmp gameRunning, 0
	je endCredits

	cmp ghost1Timer, 0
	jne dontChangeGhost
	call ChangeGhost1Direction
	mov ghost1Timer, 12

	cmp ghost2Timer, 0
	jne dontChangeGhost
	call ChangeGhost2Direction
	mov ghost2Timer, 12

	cmp ghost3Timer, 0
	jne dontChangeGhost
	call ChangeGhost3Direction
	mov ghost3Timer, 12

	dontChangeGhost:

	cmp musicTick, 3
	jne gamemode
	mov bx, tempOffset
	mov al, 0
	cmp [bx], al
	je resetnotes
	jmp musictime
resetnotes:
	mov tempOffset, offset gamenotes
	mov bx, offset gamenotes
musictime:
	mov musicTick, 0
	mov dl, [bx]
	call  getscalefrequency
    mov dx, [si]
	call PlayFrequency
	inc tempOffset
done22:
	jmp gamemode
gamemode:
	

	mov cl,pacmanLocX
	mov ch,pacmanLocY

	;;moves pacman

	movPac:
		cmp pacmanDir, 0
		je up
		cmp pacmanDir, 1
		je right
		cmp pacmanDir, 2
		je down
		cmp pacmanDir, 3
		je left
		jmp donedir
		up:
			;;check if row-1 , col is '_'
			push cx
			dec ch
			call rowcol2index2 ;checks board for borders
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stop
			cmp Board[bx], '|'
			je stop
			cmp Board[bx], '+'
			je stop
			cmp Board[bx], '='
			je stop
			cmp Board[bx], '-'
			je stop
			;check ghost contact
			push cx
			dec ch
			call rowcol2index ;checks for ghost contact
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, 'G'
			je endGame
			cmp al, '.' ;check for dot on board
			jne noScore1
			inc score
			
			noScore1:
				call RemoveChar
				sub ch, 1
				jmp donedir
			
			endGame:
				mov gameRunning, 0
		right:
			;;check if row, col+1 is '|'
			push cx
			inc cl
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stop
			cmp Board[bx], '|'
			je stop
			cmp Board[bx], '+'
			je stop
			cmp Board[bx], '='
			je stop
			cmp Board[bx], '-'
			je stop
			;check ghost contact
			push cx
			inc cl
			call rowcol2index
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, 'G'
			je endGame2
			cmp al, '.' ;checks for dot on board
			jne noScore2
			inc score
			
			noScore2:
				call RemoveChar
				add cl, 1
				jmp donedir
			
			endGame2:
				mov gameRunning, 0
		down:
			;;check if row+1 , col is '_'
			push cx
			inc ch
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stop
			cmp Board[bx], '|'
			je stop
			cmp Board[bx], '+'
			je stop
			cmp Board[bx], '='
			je stop
			cmp Board[bx], '-'
			je stop
			;check ghost contact
			push cx
			inc ch
			call rowcol2index
			pop cx 
			mov si, ax
			mov ax, es:[si]
			cmp al, 'G'
			je endGame3
			cmp al, '.' ;checks for dot
			jne noScore3
			inc score
			
			noScore3:
				call RemoveChar
				add ch, 1
				jmp donedir
			
			endGame3:
				mov gameRunning, 0
		left:
			;;check if row, col-1 is '|'
			push cx
			dec cl
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stop
			cmp Board[bx], '|'
			je stop
			cmp Board[bx], '+'
			je stop
			cmp Board[bx], '='
			je stop
			cmp Board[bx], '-'
			je stop

			push cx
			dec cl
			call rowcol2index
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, 'G'
			je endGame4
			cmp al, '.' ;checks for dot
			jne noScore4
			inc score
			
			noScore4:
				call RemoveChar
				sub cl, 1
				jmp donedir
			
			endGame4:
				mov gameRunning, 0
		
		
	donedir:
		mov pacmanLocX, cl
		mov pacmanLocY, ch
		
		call DisplayChar


	stop:
			

	;;moves ghost1
	
	call movG1

	

	;;moves ghost2
	
	call movG2

	


	;;moves ghost3 !!!!

	call movG3
	
	
	
	mov ax, score
	call displayScore
	jmp returninBaby

	endCredits:
		call speakeroff
		dec resultTimer
		;;displays results
		mov dx, OFFSET Results
		mov ax, 0B800h
		mov es, ax
		call DisplayBoard
		mov ax, score
		call DisplayFinalResults
		cmp resultTimer, 0
		jne returninBaby
		call gameOver
		




	returninBaby:
		pop si
		pop	dx
		pop cx
		pop bx
		pop	ax
		iret
Tick ENDP

movG1 PROC
	pushf
	push ax
	push bx
	push cx

	mov cl, ghost1LocX
	mov ch, ghost1LocY

	movGhost1: ;0 is up, 1 is right, 2 is down, 3 is left, 4 is still
		cmp ghost1dir, 0
		je upG1
		cmp ghost1dir, 1
		je rightG1
		cmp ghost1dir, 2
		je downG1
		cmp ghost1dir, 3
		je leftG1
		jmp donedirG1
		upG1:
			;;check if row-1 , col is '_'
			push cx
			dec ch
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG1
			cmp Board[bx], '|'
			je stopG1
			cmp Board[bx], '+'
			je stopG1
			cmp Board[bx], '='
			je stopG1
			cmp Board[bx], '-'
			je stopG1
			
			push cx
			dec ch
			call rowcol2index ;checks for ghost contact
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot1
			cmp al, 'G'
			je stopG1

			cmp ghost1DotFlag, 0 ;checks if dot needs to be added
			je erase1
			jne placeDot1

			ateDot1:
				mov ghost1DotFlag, 1
			
			erase1:
				call RemoveG1Char
				sub ch, 1
				jmp donedirG1
			
			placeDot1:
				call placeGhost1Dot
				mov ghost1DotFlag, 0
				sub ch, 1
				jmp donedirG1

		rightG1:
			;;check if row, col+1 is '|'
			push cx
			inc cl
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG1
			cmp Board[bx], '|'
			je stopG1
			cmp Board[bx], '+'
			je stopG1
			cmp Board[bx], '='
			je stopG1
			cmp Board[bx], '-' ;add cl, 1
			je stopG1
			

			push cx
			inc cl
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot2
			cmp al, 'G' ;stops at ghost
			je stopG1

			cmp ghost1DotFlag, 0 ;checks if dot needs to be added
			je erase2
			jne placeDot2

			ateDot2:
				mov ghost1DotFlag, 1
			
			erase2:
				call RemoveG1Char
				add cl, 1
				jmp donedirG1
			
			placeDot2:
				call placeGhost1Dot
				mov ghost1DotFlag, 0
				add cl, 1
				jmp donedirG1
		downG1:
			;;check if row+1 , col is '_'
			push cx
			inc ch
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG1
			cmp Board[bx], '|'
			je stopG1
			cmp Board[bx], '+'
			je stopG1
			cmp Board[bx], '='
			je stopG1
			cmp Board[bx], '-' ;add ch, 1
			je stopG1
			
			
			push cx
			inc ch
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot3
			cmp al, 'G' ;stops at ghost
			je stopG1

			cmp ghost1DotFlag, 0 ;checks if dot needs to be added
			je erase3
			jne placeDot3

			ateDot3:
				mov ghost1DotFlag, 1
			
			erase3:
				call RemoveG1Char
				add ch, 1
				jmp donedirG1
			
			placeDot3:
				call placeGhost1Dot
				mov ghost1DotFlag, 0
				add ch, 1
				jmp donedirG1
		leftG1:
			;;check if row, col-1 is '|'
			push cx
			dec cl
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG1
			cmp Board[bx], '|'
			je stopG1
			cmp Board[bx], '+'
			je stopG1
			cmp Board[bx], '='
			je stopG1
			cmp Board[bx], '-' ;sub cl, 1
			je stopG1

			push cx
			dec cl
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot4
			cmp al, 'G'
			je stopG1

			cmp ghost1DotFlag, 0 ;checks if dot needs to be added
			je erase4
			jne placeDot4

			ateDot4:
				mov ghost1DotFlag, 1
			
			erase4:
				call RemoveG1Char
				sub cl, 1
				jmp donedirG1
			
			placeDot4:
				call placeGhost1Dot
				mov ghost1DotFlag, 0
				sub cl, 1
				jmp donedirG1
		
		
	donedirG1:
		mov ghost1LocX, cl
		mov ghost1LocY, ch
		
		call DisplayG1Char

	stopG1:

	pop cx
	pop bx
	pop ax
	popf
	ret

movG1 ENDP

movG2 PROC
	
	pushf
	push ax
	push bx
	push cx

	mov cl, ghost2LocX
	mov ch, ghost2LocY

	movGhost2: ;0 is up, 1 is right, 2 is down, 3 is left, 4 is still
		cmp ghost2dir, 0
		je upG2
		cmp ghost2dir, 1
		je rightG2
		cmp ghost2dir, 2
		je downG2
		cmp ghost2dir, 3
		je leftG2
		jmp donedirG2
		upG2:
			;;check if row-1 , col is '_'
			push cx
			dec ch
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG2
			cmp Board[bx], '|'
			je stopG2
			cmp Board[bx], '+'
			je stopG2
			cmp Board[bx], '='
			je stopG2
			cmp Board[bx], '-'
			je stopG2

			push cx
			dec ch
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot1G2
			cmp al, 'G'
			je stopG2

			cmp ghost2DotFlag, 0 ;checks if dot needs to be added
			je erase1G2
			jne placeDot1G2

			ateDot1G2:
				mov ghost2DotFlag, 1
			
			erase1G2:
				call RemoveG2Char
				sub ch, 1
				jmp donedirG2
			
			placeDot1G2:
				call placeGhost2Dot
				mov ghost2DotFlag, 0
				sub ch, 1
				jmp donedirG2

		rightG2:
			;;check if row, col+1 is '|'
			push cx
			inc cl
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG2
			cmp Board[bx], '|'
			je stopG2
			cmp Board[bx], '+'
			je stopG2
			cmp Board[bx], '='
			je stopG2
			cmp Board[bx], '-' ;add cl, 1
			je stopG2

			push cx
			inc cl
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot2G2
			cmp al, 'G'
			je stopG2

			cmp ghost2DotFlag, 0 ;checks if dot needs to be added
			je erase2G2
			jne placeDot2G2

			ateDot2G2:
				mov ghost2DotFlag, 1
			
			erase2G2:
				call RemoveG2Char
				add cl, 1
				jmp donedirG2
			
			placeDot2G2:
				call placeGhost2Dot
				mov ghost2DotFlag, 0
				add cl, 1
				jmp donedirG2
		downG2:
			;;check if row+1 , col is '_'
			push cx
			inc ch
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG2
			cmp Board[bx], '|'
			je stopG2
			cmp Board[bx], '+'
			je stopG2
			cmp Board[bx], '='
			je stopG2
			cmp Board[bx], '-' ;add ch, 1
			je stopG2
			
			push cx
			inc ch
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot3G2
			cmp al, 'G'
			je stopG2

			cmp ghost2DotFlag, 0 ;checks if dot needs to be added
			je erase3G2
			jne placeDot3G2

			ateDot3G2:
				mov ghost2DotFlag, 1
			
			erase3G2:
				call RemoveG2Char
				add ch, 1
				jmp donedirG2
			
			placeDot3G2:
				call placeGhost2Dot
				mov ghost2DotFlag, 0
				add ch, 1
				jmp donedirG2
		leftG2:
			;;check if row, col-1 is '|'
			push cx
			dec cl
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG2
			cmp Board[bx], '|'
			je stopG2
			cmp Board[bx], '+'
			je stopG2
			cmp Board[bx], '='
			je stopG2
			cmp Board[bx], '-' ;sub cl, 1
			je stopG2

			push cx
			dec cl
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot4G2
			cmp al, 'G'
			je stopG2

			cmp ghost2DotFlag, 0 ;checks if dot needs to be added
			je erase4G2
			jne placeDot4G2

			ateDot4G2:
				mov ghost2DotFlag, 1
			
			erase4G2:
				call RemoveG2Char
				sub cl, 1
				jmp donedirG2
			
			placeDot4G2:
				call placeGhost2Dot
				mov ghost2DotFlag, 0
				sub cl, 1
				jmp donedirG2
		
		
	donedirG2:
		mov ghost2LocX, cl
		mov ghost2LocY, ch
		
		call DisplayG2Char

	stopG2:
		pop cx
		pop bx
		pop ax
		popf
		ret

movG2 ENDP

movG3 PROC
	pushf
	push ax
	push bx
	push cx


	mov cl, ghost3LocX
	mov ch, ghost3LocY

	movGhost3: ;0 is up, 1 is right, 2 is down, 3 is left, 4 is still
		cmp ghost3dir, 0
		je upG3
		cmp ghost3dir, 1
		je rightG3
		cmp ghost3dir, 2
		je downG3
		cmp ghost3dir, 3
		je leftG3
		jmp donedirG3
		upG3:
			;;check if row-1 , col is '_'
			push cx
			dec ch
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG3
			cmp Board[bx], '|'
			je stopG3
			cmp Board[bx], '+'
			je stopG3
			cmp Board[bx], '='
			je stopG3
			cmp Board[bx], '-'
			je stopG3

			push cx
			dec ch
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot1G3
			cmp al, 'G'
			je stopG3

			cmp ghost3DotFlag, 0 ;checks if dot needs to be added
			je erase1G3
			jne placeDot1G3

			ateDot1G3:
				mov ghost3DotFlag, 1
			
			erase1G3:
				call RemoveG3Char
				sub ch, 1
				jmp donedirG3
			
			placeDot1G3:
				call placeGhost3Dot
				mov ghost3DotFlag, 0
				sub ch, 1
				jmp donedirG3

		rightG3:
			;;check if row, col+1 is '|'
			push cx
			inc cl
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG3
			cmp Board[bx], '|'
			je stopG3
			cmp Board[bx], '+'
			je stopG3
			cmp Board[bx], '='
			je stopG3
			
			push cx
			inc cl
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot2G3
			cmp al, 'G'
			je stopG3

			cmp ghost3DotFlag, 0 ;checks if dot needs to be added
			je erase2G3
			jne placeDot2G3

			ateDot2G3:
				mov ghost3DotFlag, 1
			
			erase2G3:
				call RemoveG3Char
				add cl, 1
				jmp donedirG3
			
			placeDot2G3:
				call placeGhost3Dot
				mov ghost3DotFlag, 0
				add cl, 1
				jmp donedirG3
		downG3:
			;;check if row+1 , col is '_'
			push cx
			inc ch
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG3
			cmp Board[bx], '|'
			je stopG3
			cmp Board[bx], '+'
			je stopG3
			cmp Board[bx], '='
			je stopG3
			cmp Board[bx], '-' ;add ch, 1
			je stopG3
			
			push cx
			inc ch
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot3G3
			cmp al, 'G'
			je stopG3

			cmp ghost3DotFlag, 0 ;checks if dot needs to be added
			je erase3G3
			jne placeDot3G3

			ateDot3G3:
				mov ghost3DotFlag, 1
			
			erase3G3:
				call RemoveG3Char
				add ch, 1
				jmp donedirG3
			
			placeDot3G3:
				call placeGhost3Dot
				mov ghost3DotFlag, 0
				add ch, 1
				jmp donedirG3
		leftG3:
			;;check if row, col-1 is '|'
			push cx
			dec cl
			call rowcol2index2
			pop cx
			mov bx, ax
			cmp Board[bx], '_'
			je stopG3
			cmp Board[bx], '|'
			je stopG3
			cmp Board[bx], '+'
			je stopG3
			cmp Board[bx], '='
			je stopG3
			cmp Board[bx], '-' ;sub cl, 1
			je stopG3

			push cx
			dec cl
			call rowcol2index ;checks for dot picked up
			pop cx
			mov si, ax
			mov ax, es:[si]
			cmp al, '.'
			je ateDot4G3
			cmp al, 'G'
			je stopG3

			cmp ghost3DotFlag, 0 ;checks if dot needs to be added
			je erase4G3
			jne placeDot4G3

			ateDot4G3:
				mov ghost3DotFlag, 1
			
			erase4G3:
				call RemoveG3Char
				sub cl, 1
				jmp donedirG3
			
			placeDot4G3:
				call placeGhost3Dot
				mov ghost3DotFlag, 0
				sub cl, 1
				jmp donedirG3
		
		
	donedirG3:
		mov ghost3LocX, cl
		mov ghost3LocY, ch
		
		call DisplayG3Char

	stopG3:
		pop cx
		pop bx
		pop ax
		popf
		ret

movG3 ENDP


checkPacGhost PROC
	;cl = ghost1LocX
	;ch = ghost1LocY
	;dl = pacmanLocX
	;dh = pacmanLocY

	cmp dl, cl
	jne cap
	cmp dh, ch
	jne cap
	mov gameRunning, 0

	cap:
		ret

checkPacGhost ENDP

checkGhostGhost PROC ;checks if ghosts make contact
	;cl = ghostALocX
	;ch = ghostALocY
	;dl = ghostBLocX
	;dh = ghostBLocY
	;ret ah=1 if True, ah=0

	cmp dl, cl
	jne cap
	cmp dh, ch
	jne cap
	mov ah, 1

	cap:
		mov ah, 0
		ret
checkGhostGhost ENDP



FIRST_SEED = 0100110001110000b

Random16Seed WORD FIRST_SEED
	
Random16 PROC
	;; returns:
	;; ax - a 16-bit random number
.386
	pushf
	push	edx
	push	eax
	
	cmp	Random16Seed, FIRST_SEED
	jne	good
	call	Randomize
good:	
	add	Random16Seed, 0FC15h 
	movzx	eax, Random16Seed
	mov	edx, 02ABh
	mul	edx
	mov	edx, eax
	shr	edx, 16
	xor	eax, edx
	and	eax, 0FFFFh
	mov	edx, eax
	
	pop	eax
	mov	ax, dx
	pop	edx
	popf
	ret
.8086
Random16 ENDP
	
Randomize PROC
	;; sets seed to current hundreths of seconds
	pushf
	push	ax
	push	bx
	push	cx
	push	dx
	
	mov	ah,2Ch
	int	21h		; ch (hrs), cl (mins), dh (sec), dl (hsec)
	
	mov	bh, 0
	mov	bl, dl

	mov	dh, 0
	mov	dl, dh
	mov	ax, 100
	mul	dx
	add	bx, ax

	mov	dh, 0
	mov	dl, cl
	mov	ax, 6000
	mul	dx
	add	bx, ax
	
	mov	Random16Seed, bx
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	popf	
	ret
Randomize ENDP

RandRange PROC
	;; ax - maximum value + 1
	;; returns:
	;; ax - a value between 0 - (ax - 1)
	pushf
	push	bx
	push	dx
	
	mov	bx, ax
	call	Random16
	mov	dx, 0
	div	bx
	mov	ax, dx
	
	pop	dx
	pop	bx
	popf
	
	ret
RandRange ENDP

;0 is up, 1 is right, 2 is down, 3 is left, 4 is still

ChangeGhost1Direction PROC
	push ax
	mov ax, 4
	call RandRange
	cmp ax, 3
	je left
	cmp ax, 0
	je up
	cmp ax, 1
	je right
	cmp ax, 2
	jne done
	down:
		mov ghost1dir, 2
		jmp done
	left: 
		mov ghost1dir, 3
		jmp done
	right: 
		mov ghost1dir, 1
		jmp done
	up: 
		mov ghost1dir, 0
		jmp done

	done:
		pop ax
		ret
ChangeGhost1Direction ENDP

ChangeGhost2Direction PROC
	push ax
	mov ax, 4
	call RandRange
	cmp ax, 3
	je left
	cmp ax, 0
	je up
	cmp ax, 1
	je right
	cmp ax, 2
	jne done
	down:
		mov ghost2dir, 2
		jmp done
	left: 
		mov ghost2dir, 3
		jmp done
	right: 
		mov ghost2dir, 1
		jmp done
	up: 
		mov ghost2dir, 0
		jmp done

	done:
		pop ax
		ret
ChangeGhost2Direction ENDP

ChangeGhost3Direction PROC
	push ax
	mov ax, 4
	call RandRange
	cmp ax, 3
	je left
	cmp ax, 0
	je up
	cmp ax, 1
	je right
	cmp ax, 2
	jne done
	down:
		mov ghost3dir, 2
		jmp done
	left: 
		mov ghost3dir, 3
		jmp done
	right: 
		mov ghost3dir, 1
		jmp done
	up: 
		mov ghost3dir, 0
		jmp done

	done:
		pop ax
		ret
ChangeGhost3Direction ENDP

DisplayChar PROC
	push ax
	push cx
	push dx

	mov cl,pacmanLocX
	mov ch,pacmanLocY
	mov al, 'M'
	mov dl, 0
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret	
DisplayChar ENDP

RemoveChar PROC
	push ax
	push cx
	push dx

	mov cl,pacmanLocX
	mov ch,pacmanLocY
	mov dl, 0
	mov al, ' '
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
RemoveChar ENDP

DisplayG1Char PROC
	push ax
	push cx
	push dx

	mov cl,ghost1LocX
	mov ch,ghost1LocY
	mov al, 'G'
	mov dl, 1
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret	
DisplayG1Char ENDP

placeGhost1Dot PROC
	push ax
	push cx
	push dx

	mov cl,ghost1LocX
	mov ch,ghost1LocY
	mov al, '.'
	mov dl, 0
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
placeGhost1Dot ENDP

RemoveG1Char PROC
	push ax
	push cx
	push dx

	mov cl,ghost1LocX
	mov ch,ghost1LocY
	mov al, ' '
	mov dl, 0
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
RemoveG1Char ENDP

DisplayG2Char PROC
	push ax
	push cx
	push dx

	mov cl,ghost2LocX
	mov ch,ghost2LocY
	mov al, 'G'
	mov dl, 2
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
DisplayG2Char ENDP

placeGhost2Dot PROC
	push ax
	push cx
	push dx

	mov cl,ghost2LocX
	mov ch,ghost2LocY
	mov al, '.'
	mov dl, 0
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
placeGhost2Dot ENDP

RemoveG2Char PROC
	push ax
	push cx
	push dx

	mov cl,ghost2LocX
	mov ch,ghost2LocY
	mov al, ' '
	mov dl, 0
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
RemoveG2Char ENDP

DisplayG3Char PROC
	push ax
	push cx
	push dx

	mov cl,ghost3LocX
	mov ch,ghost3LocY
	mov al, 'G'
	mov dl, 3
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
DisplayG3Char ENDP

placeGhost3Dot PROC
	push ax
	push cx
	push dx

	mov cl,ghost3LocX
	mov ch,ghost3LocY
	mov al, '.'
	mov dl, 0
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
placeGhost3Dot ENDP

RemoveG3Char PROC
	push ax
	push cx
	push dx

	mov cl,ghost3LocX
	mov ch,ghost3LocY
	mov al, ' '
	mov dl, 0
	call ScreenChar

	pop dx
	pop cx
	pop ax
	ret
RemoveG3Char ENDP



rowcol2index2 PROC
	;; ch - row
	;; cl - col
	;; return
	;; ax - index

	pushf
	push	cx
	
	mov	ax, 80
	mul	ch
	mov	ch, 0
	add	ax, cx
	
	pop	cx
	popf

	ret
rowcol2index2 ENDP

rowcol2index PROC
	;; ch - row
	;; cl - col
	;; return
	;; ax - index

	pushf
	push	cx
	
	mov	ax, 80
	mul	ch
	mov	ch, 0
	add	ax, cx
	shl	ax, 1
	
	pop	cx
	popf

	ret
rowcol2index ENDP

ScreenChar PROC
	;; ch - row
	;; cl - col
	;; al - character
	;; dl - which ghost? 1, 2, or 3

	push	ax
	push	di
	push	es
	
	mov	di, 0B800h
	mov	es, di
	
	push	ax
	call	rowcol2index
	mov	di, ax
	pop	ax
	
	cmp al, 'M' ;prints pacman as yellow
	je PacMan

	cmp dl, 1 ;prints ghost1 as red
	je Ghost1

	cmp dl, 2 ;prints ghost2 as blue
	je Ghost2

	cmp dl, 3 ;prints ghost3 as green
	je Ghost3

	jmp Other

	PacMan:
		mov ah, 00001110b
		jmp printMem
	
	Ghost1:
		mov ah, 00001100b ;red
		jmp printMem
	
	Ghost2:
		mov ah, 00001001b
		jmp printMem
	
	Ghost3:
		mov ah, 00001010b
		jmp printMem
	
	Other:
		mov	ah, 00001111b

	printMem:
		mov	es:[di], ax
	
	pop	es
	pop	di
	pop	ax
	ret
ScreenChar ENDP

DisplayBoard PROC
    ;takes in Board OFFSET in BX
    ;takes in 0B800h in es
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    pushf

    mov si, 0
    mov di, 0
    mov cx, 2000
    mov ah, 00001111b ;map colors
	mov bx, dx  ; change back to offset board if fucked
    Top:
        mov al, [bx+di]
		mov ah, 00001111b ;map colors
		cmp al, 'M'
		jne notPac
		mov ah, 00001110b ;pacman colors
		notPac:
			mov es:[si], ax
			inc si
			inc si
			inc di
    loop Top

    popf
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

DisplayBoard ENDP

gameOver PROC
	;; Restore the saved vector
	mov ax, 03h
	int 10h
	

	mov	al, INTERRUPT
	mov	dx, offset ClockVector
	call	RestoreVector
	mov ax, TERMINATE ; DOS function to exit with termination code
	int DOS ; exit
gameOver ENDP

ChangeDirection PROC
	mov	ah, 10h
	int	16h
	cmp al, 'a'
	je left
	cmp al, 'w'
	je up
	cmp al, 'd'
	je right
	cmp al, 'p' ;click p to end program
	je exit
	cmp al, 's'
	jne done
	down:
		mov pacmanDir, 2
		jmp done
	left: 
		mov pacmanDir, 3
		jmp done
	right: 
		mov pacmanDir, 1
		jmp done
	up: 
		mov pacmanDir, 0
		jmp done
	exit:
		call speakeroff
		call gameOver

	done:
		ret
ChangeDirection ENDP



displayScore PROC 
	;takes in arguments in ax and prints it out
    pushf
    push bx
    push cx
    push dx
    push es
    push si


    mov cx, 0B800h
    mov es, cx
	;initilize count
    mov cx,5
    mov dx,0
    mov si, 22 ;index of last score digit
    top:
		cmp ax, 0
		je donezo ;breaks loop

		mov dx, 0    ; dividend high half = 0.  prefer  xor edx,edx

		mov bx, 10            ; divisor can be any register or memory

		div bx       ; Divides 1234 by 10.
		add dl, 48 ;get ascii value of number
		mov dh, 00001111b ;map colors
		
		

		mov es:[si], dx ;puts digit on right index in screen
		dec si
		dec si

		
	loop top

	donezo:
		pop si
		pop es
		pop dx
		pop cx
		pop bx 
		popf
		ret
displayScore ENDP

DisplayFinalResults PROC 
	;takes in arguments in ax and prints it out
    pushf
    push bx
    push cx
    push dx
    push es
    push si


    mov cx, 0B800h
    mov es, cx
	;initilize count
    mov cx,5
    mov dx,0
    mov si, 886 ;index of last score digit
    top:
		cmp ax, 0
		je donezo ;breaks loop

		mov dx, 0    ; dividend high half = 0.  prefer  xor edx,edx

		mov bx, 10            ; divisor can be any register or memory

		div bx       ; Divides 1234 by 10.
		add dl, 48 ;get ascii value of number
		mov dh, 00001111b ;map colors
		
		

		mov es:[si], dx ;puts digit on right index in screen
		dec si
		dec si

		
	loop top

	donezo:
		pop si
		pop es
		pop dx
		pop cx
		pop bx 
		popf
		ret
DisplayFinalResults ENDP

PrintChar PROC ;prints value in dl
	mov ah, 02h
	int 21h
	ret
PrintChar ENDP

PlayNoteString PROC
	mov bx, offset gamenotes
top:
	mov al, 0
	cmp [bx], al
	je done

	mov dl, [bx]
	call  getscalefrequency
    mov dx, [si]
	cmp dx, 0
	je stalljmp
	call PlayFrequency
stalljmp:
	call stall1
	call stall1
	call SpeakerOff

	inc bx
	loop top
done:

	call stall1
	call stall1

	ret
PlayNoteString ENDP


SpeakerOff PROC

	pushf
	push	ax
	
	in	al, SPEAKER_PORT		; Read the speaker register
	and	al, 0FCh			; Clear the two low bits high
	out	SPEAKER_PORT, al		; Write the speaker register

	pop	ax
	popf
	ret
SpeakerOff ENDP

SpeakerOn PROC
	pushf
	push	ax

	test	cs:SpeakerMuted, 1
	jnz	muted
	
	in	al, SPEAKER_PORT		; Read the speaker register
	or	al, 03h				; Set the two low bits high
	out	SPEAKER_PORT, al		; Write the speaker register
	jmp	done
muted:	
	push	dx
	mov	dl, 02h
	mov ah, 02h
	int 21h
	pop	dx
done:	
	pop	ax
	popf
	ret
SpeakerOn ENDP

stall1 PROC
	mov	cx, 65535
top:
	loop	top
	ret
stall1 ENDP

PlayFrequency PROC
	;; Frequency is found in DX

	pushf
	push	ax
	
	mov	al, READY_TIMER			; Get the timer ready
	out	TIMER_CONTROL_PORT, al

	mov	al, dl
	out	TIMER_DATA_PORT, al		; Send the count low byte
	
	mov	al, dh
	out	TIMER_DATA_PORT, al		; Send the count high byte
	;call	InterNoteDelay
	
	; call	NoteDelay
	call	SpeakerOff
	call stall1
	call stall1
	call stall1
	call stall1
	call stall1
	call	SpeakerOn

	pop	ax
	popf
	ret
PlayFrequency ENDP





main PROC
    mov ax, @data
    mov	ax, cs ;sets up data segment
	mov	ds, ax
	


;; Display the active vector
	mov al, INTERRUPT
	mov bx, 0
	mov es, bx
	call GetInterruptVector

;; Save the vector
	mov al, INTERRUPT
	mov dx, offset ClockVector
	call SaveVector

;; Display the saved vector
	mov dx, offset ClockVector

	mov al, interrupt
	mov dx, tick
	call installhandler


    ;;displays board
    mov dx, OFFSET Board
    mov ax, 0B800h
    mov es, ax
    call DisplayBoard

	mov tempOffset, offset gamenotes
	inputs:
		call ChangeDirection

		mov dl, pacmanLocX
		mov dh, pacmanLocY

		;checks if bumps into G1
		mov cl, ghost1LocX
		mov ch, ghost1LocY
		call checkPacGhost

		;checks if bumps into G2
		mov cl, ghost2LocX
		mov ch, ghost2LocY
		call checkPacGhost

		
		;checks if bumps into G3
		mov cl, ghost3LocX
		mov ch, ghost3LocY
		call checkPacGhost
		
		cmp score, 217
		je won

		jmp inputs
	
	won:
		mov gameRunning, 0


  
main ENDP
END main