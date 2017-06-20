.386
dane segment
;algorytm Bresenhama !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    cmd_count   db 0              ;ilosc argumentow
    cmd_len  dw 150 dup(?)     ;dl argumentow
    cmd_offset  dw 150 dup(?)     ;offsety
    cmd_arg     db 300 dup(0)   ;wartosci

    handler1 dw	0

    buffer db 500 dup (0) ;file reading buffer
    inbuffer dw 0 ;how many in buffer
    bufferpos dw 0 ;current position in buffer
    finishedreading db 0



	penActivityFlag		db	1	; 1 - opuszczony, 0 - podniesiony (domy�lnie opuszczony, gotowy do pisania)


  file_open_error db "Nie udalo sie otworzyc pliku",13,10,"$"
  file_close_error	db	"Nie udalo sie zamknac pliku",13,10,"$"
  wrong_command_error db "Nieznana komenda",13,10,"$"
  wrong_arguments db "Podano nieprawidlowe argumenty",13,10,"$"

	angle		dw	270	; aktualny kierunek ��wia
	_180_		dw	180	; warto�� 180 potrzebna przy zamianie stopni na radiany
	_2_		dw	2	; warto�� 2 potrzebna do mno�enia przez dwa
	_0_		dw	0	; warto�� 2 potrzebna do mno�enia przez dwa
	x_beg		dw	160	; sta�a, pocz�tkowa wsp. x pozycji ��wia
	y_beg		dw	100	; sta�a, pocz�tkowa wsp. y pozycji ��wia
	len		dw	80	; d�ugo�� odcinka
	xp		dd	160	; wsp. X pocz�tku odcinka
	yp		dd	100	; wsp. Y pocz�tku odcinka
	xk		dd	?	; wsp. X ko�ca odcinka
	yk		dd	?	; wsp. Y ko�ca odcinka
	tmp_integer	dw	?	; tymczasowe miejsce do konwersji float -> int
	D_Di		dd	?
	D_y		dd	?
	D_x		dd	?
	m_y		dd	?
	m_x		dd	?

    t dw ?,"$"



dane ends

code segment use16
start:

mov ax , seg wstosu
mov ss , ax
mov sp , offset wstosu

	call	parse
  mov	ax, dane
  mov	ds, ax
  call check_command_line
  call open_file

	call	draw_image
  call close_file
  call kill_program





	draw_image proc
		push	cx

		finit
		fild	word ptr ds:[x_beg]
		fstp	dword ptr ds:[xp]
		fild	word ptr ds:[y_beg]
		fstp	dword ptr ds:[yp]

		mov	ax, 13h
		int	10h		;  tryb graficzny

		mov	ax, 0A000h
		mov	es, ax	; ustawiam segment na odpowiedni dla trybu graf. 13h


		drawing_loop:
			call	get_char
      cmp ds:[finishedreading] , 1
      je koniec_rysowania

			cmp	al, 'r'
			jne	not_r
				call	rotate_command
				jmp	drawing_loop
			not_r:
			cmp	al, 'm'
			jne	not_m
				call	move_command
				jmp	drawing_loop
			not_m:
			cmp	al, 'u'
			jne	not_u
				call	up_command
				jmp	drawing_loop
			not_u:
      cmp	al, 'd'
			jne	not_d
				call	down_command
				jmp	drawing_loop
      not_d:
      call error_commands
koniec_rysowania:
		xor ah, ah
		int 16h		; czekanie na nacisniecie przycisku

		mov ax, 3
		int 10h		; powrot do trybu tekstowego

		pop	cx
		ret
	draw_image endp


	rotate_command proc
popa
    call get_char

		call	get_integer_from_file ;wynik w dx
		mov	ax, dx
		xor	dx, dx
		add	ax, word ptr ds:[angle]	; dodaje do obecnego kata
		mov	bx, 360
		div	bx				; modulo 360
		mov	word ptr ds:[angle], dx	; uaktualniam ds:angle

		call	get_char	; pobieram line feed
pusha
		ret
	rotate_command endp

	move_command proc
  pusha
    call get_char ;pobieram spacje
		call	get_integer_from_file
		mov	word ptr ds:[len] , dx	; zapisuje dlugosc
		call	line		; rysowanie linii
		call	get_char	; pobieram line feed
  popa
		ret
	move_command endp

  up_command proc
  pusha
          mov	byte ptr ds:[penActivityFlag], 0
          call	get_char
  popa
          ret
  up_command endp

  down_command proc
  pusha
      mov	byte ptr ds:[penActivityFlag], 1
      call	get_char
  popa
      ret
  down_command endp


	get_integer_from_file proc
		push	ax
		push	bx
		push	di

		xor	dx, dx
		xor	ax, ax
		xor	bx, bx
		get_integer_loop:
			call	get_char
			cmp al , 10
	    je	end_of_get_integer_loop
			cmp	al, 13
			je	end_of_get_integer_loop
			sub	al, '0' ; konwersja ascii -> int
			push	ax		; odkladam na stos
			inc	bx		; licze ile cyfr ma liczba
		jmp	get_integer_loop
		end_of_get_integer_loop:

		; obliczanie liczby wtciagajac ze stosu w odwrotnej kolejnosci
		xchg	cx, bx	; swap bx , cx bo w cx potrzebuje licznika pętli
		mov	di, 1	; kolejne potegi 10
		eval_loop:
			pop	ax
			imul	ax, di
			add	dx, ax
			imul	di, 10	; kolejna potega 10
		loop	eval_loop

		xchg	cx, bx	; przywracam stary cx

		pop	di
		pop	bx
		pop	ax
		ret
	get_integer_from_file endp




	line proc
		push	eax
		push	cx
		push	si
		push	di
		push	bp
		push	es

		call	find_end_coordinates
		call	check_case

		fld	dword ptr ds:[D_y]
		fimul	word ptr ds:[_2_]
		fsub	dword ptr ds:[D_x]
		fstp	dword ptr ds:[D_Di]	; w ds:[D_Di] mam poczatkowa wartosc D_Di di=2(dy-dx)


		fld	dword ptr ds:[yp]
		fistp	word ptr ds:[tmp_integer] ; konwertuje i wrzuca
		mov	di, word ptr ds:[tmp_integer]
		imul	di, 320 ; bo w dół
		fld	dword ptr ds:[xp]
		fist	word ptr ds:[tmp_integer]  ; nie wymaga mnozenia
		add	di, word ptr ds:[tmp_integer]		;w prawo
;mamy w ten sposob wprowadzone wspolrzedne poczatkowe do rejestrów na podstawie ktrorych bedziemy potem rysowac


		fld	dword ptr ds:[D_x]
		fistp	word ptr ds:[tmp_integer]
		mov	cx, word ptr ds:[tmp_integer]	; licznik <- D_X ; wykonamy tyle krokow ile trzeba by przejsc po x'ach od poczatku do konca lini

		main_loop:
			fld	dword ptr ds:[D_Di]
			fistp	word ptr ds:[tmp_integer] ; wrzucamy obcięte DDi
			mov	ax, word ptr ds:[tmp_integer]
			cmp	ax, 0
			jl	less_than_zero
      ;zgodnie z algorytmem bergsenhama jak DDi >0 to wybieramy (x+1,y+1) bo to oznacza ze on jest blizej idealnej prostej
				add	di, si
				add	di, bp	; w DI mam jednowymiarowe wsporzedne piksela do zapalenia

				fld	dword ptr ds:[D_y]
				fsub	dword ptr ds:[D_x]
				fimul	word ptr ds:[_2_]
				fadd	dword ptr ds:[D_Di]
				fstp	dword ptr ds:[D_Di] ; ddi = 2(dy-dx)+ddi_stare

				jmp	goto_drawing_pixel
			less_than_zero:
        ;zgodnie z algorytmem bergsenhama jak DDi <0 to wybieramy (x+1,y) bo to oznacza ze on jest blizej idealnej prostej
				add	di, si	; w DI mam jednowymiarowe wsporzedne piksela do zapalenia

				fld	dword ptr ds:[D_y]
				fimul	word ptr ds:[_2_]
				fadd	dword ptr ds:[D_Di]
				fstp	dword ptr ds:[D_Di] ; ddi= 2dy+ddi_stare

			goto_drawing_pixel:

			cmp	byte ptr ds:[penActivityFlag], 1	; sprawdzam flage pisaka
			jne	PenUP_DoNotPaint
				mov	byte ptr es:ds:[di], 15	; koloruj piksel na bialo
			PenUP_DoNotPaint:
		loop main_loop

		mov	eax, dword ptr ds:[xk]		; |
		mov	dword ptr ds:[xp], eax		; |
		mov	eax, dword ptr ds:[yk]		; | => punkt koncowy staje sie nowym punktem poczatkowym
		mov	dword ptr ds:[yp], eax		; |

		pop	es
		pop	bp
		pop	di
		pop	si
		pop	cx
		pop	eax
		ret
	line endp




	find_end_coordinates proc
		push	eax
		push	edx

		cmp	word ptr ds:[angle], 90
		jne	not_90_deg
			mov	eax, dword ptr ds:[xp]
			mov	dword ptr ds:[xk], eax

			fld	dword ptr ds:[yp]
			fiadd	word ptr ds:[len]
			fstp	dword  ptr ds:[yk] ;yk = yp+len w dword mogę przechować liczbę ułamkową

			jmp	coordinates_found
		not_90_deg:
		cmp	word ptr ds:[angle], 270
		jne	not_270_deg
			mov	eax, dword ptr ds:[xp]
			mov	dword ptr ds:[xk], eax

			fld	dword ptr ds:[yp]
			fisub	word ptr ds:[len]
			fstp	dword  ptr ds:[yk] ; yk=yp-len

			jmp	coordinates_found
		not_270_deg:

		fldpi				; laduj pi
		fidiv	word ptr ds:[_180_]	; w st(0) mam pi/180
		fimul	word ptr ds:[angle]	; w st(0) mam pi/180 * angle czyli k�t w radianach

		fldz			; st(0) = 0.0, st(1) = alpha
		fadd	st(0), st(1); st(0) = alpha, st(1) = alpha, bo alpha przyda si� p�niej

		; licze xk
		fcos		; w st(0) mam cos(alpha)
		fimul word ptr ds:[len]
		fadd	dword ptr ds:[xp]
		fst	dword ptr ds:[xk]	; zapisuje xk= cos(alpha)*len + xp

		; licze yk
		fsub	dword ptr ds:[xp]	; st(0) = dx = xk-xp, st(1) = alpha
		fxch	st(1)			; swap -> st(0) = alpha, st(1) = dx
		fptan				; licze tan(alpha), st(0) = 1.0, st(1) = tangens, st(2)= dx ; nie wiem dlaczego ta 1 tam jest ale doświadczalnie sprawdzilem ze jest
		fxch	st(2)			; niepotrzebna 1 idzie do st(2)
		fmul	st(0), st(1)
		fadd	dword ptr ds:[yp]	; w st(0) mam obliczony yk
		fstp	dword ptr ds:[yk]	; zapisuje yk = tan(alpha)*dx +yp
    fstp st(0)
    fstp st(0)
		coordinates_found:

		pop	edx
		pop	eax
		ret
	find_end_coordinates endp


	check_case proc
		push	ax
;podział na kąty
;według algorytmu. tu nie ma nic ciekawego

		mov	ax, word ptr ds:[angle]
		cmp	ax, 45
		jb	first_case
		cmp	ax, 90
		jb	second_case
		cmp	ax, 135
		jb	third_case
		cmp	ax, 180
		jb	fourth_case
		cmp	ax, 225
		jb	fifth_case
		cmp	ax, 270
		jb	sixth_case
		cmp	ax, 315

		jb	seventh_case
		jmp	eight_case

		first_case:

			fld	dword ptr ds:[xk]
			fsub	dword ptr ds:[xp]
			fstp	dword ptr ds:[D_x] ;d_x=xk-xp         ; obracamy sobie osie tak żeby si poruszalo sie po dx a bp po dy i zeby dx i dy  byly dodatnie (szkoda czasu na implementowanie wartosci bezwzglednej)
			fld	dword ptr ds:[yk]
			fsub	dword ptr ds:[yp]
			fstp	dword ptr ds:[D_y] ; d_y=yk-yp
			mov	si, 1
			mov	bp, 320   ; w prawo i do dołu ; w tej czesci ustalamy miedzy jakimi 2 punktami bedziemy wybierac bo w daleszej czesci albo birzemy si albo (si+bp)
			jmp	case_checked
		second_case:

			fld	dword ptr ds:[yk]
			fsub	dword ptr ds:[yp]
			fstp	dword ptr ds:[D_x]
			fld	dword ptr ds:[xk]
			fsub	dword ptr ds:[xp]
			fstp	dword ptr ds:[D_y]
			mov	si, 320
			mov	bp, 1
			jmp	case_checked
		third_case:

			fld	dword ptr ds:[yk]
			fsub	dword ptr ds:[yp]
			fstp	dword ptr ds:[D_x]
			fld	dword ptr ds:[xp]
			fsub	dword ptr ds:[xk]
			fstp	dword ptr ds:[D_y]
			mov	si, 320
			mov	bp, (-1)
			jmp	case_checked
		fourth_case:

			fld	dword ptr ds:[xp]
			fsub	dword ptr ds:[xk]
			fstp	dword ptr ds:[D_x]
			fld	dword ptr ds:[yk]
			fsub	dword ptr ds:[yp]
			fstp	dword ptr ds:[D_y]
			mov	si, (-1)
			mov	bp, 320
			jmp	case_checked
		fifth_case:

			fld	dword ptr ds:[xp]
			fsub	dword ptr ds:[xk]
			fstp	dword ptr ds:[D_x]
			fld	dword ptr ds:[yp]
			fsub	dword ptr ds:[yk]
			fstp	dword ptr ds:[D_y]
			mov	si, (-1)
			mov	bp, (-320)
			jmp	case_checked
		sixth_case:

			fld	dword ptr ds:[yp]
			fsub	dword ptr ds:[yk]
			fstp	dword ptr ds:[D_x]
			fld	dword ptr ds:[xp]
			fsub	dword ptr ds:[xk]
			fstp	dword ptr ds:[D_y]
			mov	si, (-320)
			mov	bp, (-1)
			jmp	case_checked
		seventh_case:

			fld	dword ptr ds:[yp]
			fsub	dword ptr ds:[yk]
			fstp	dword ptr ds:[D_x]
			fld	dword ptr ds:[xk]
			fsub	dword ptr ds:[xp]
			fstp	dword ptr ds:[D_y]
			mov	si, (-320)
			mov	bp, 1
			jmp	case_checked
		eight_case:

			fld	dword ptr ds:[xk]
			fsub	dword ptr ds:[xp]
			fstp	dword ptr ds:[D_x]
			fld	dword ptr ds:[yp]
			fsub	dword ptr ds:[yk]
			fstp	dword ptr ds:[D_y]
			mov	si, 1
			mov	bp, (-320)

		case_checked:

		pop	ax
		ret
	check_case endp



    ;--------------------------------------------------------------------
    check_command_line proc
    cmp ds:[cmd_count] , 1
    jne error_command_line
    ret
    check_command_line endp

    ;----------------------------------------------------------------
    ;operacje na pliku:

    open_file proc
    pusha
        mov dx , ds:[cmd_offset]
        xor al , al
        mov ah,3dh
        int 21h
        jc error_while_opening
        mov ds:[handler1],ax				;handler->identyfikator pliku
    popa
        ret
    open_file endp

    close_file proc
    pusha
        	mov bx , ds:[handler1]
        	mov ah,3eh
        	int 21h
        	jc error_while_closing_file			;CF==0 -> ok, CF==1 not ok
    popa
          ret
    close_file endp

    ;--------------------------------------
    ;odczyt z pliku:
    get_char proc
      push dx
      push cx
      push bx
      xor ax ,ax
            mov dx,ds:[inbuffer] ;how many characters are already in buffer?
            cmp dx,0
            jg returnchar
            call loadbuffer
        returnchar:
            cmp cx,1
            jne r2 ;if eof
            mov dx,ds:[inbuffer]
            cmp dx,0
            jne r2 ;there was none and none more loaded
            mov ds:[finishedreading], 1
            jmp charreturned
        r2:
            xor bx,bx
            mov bx,ds:[bufferpos]
            mov al,ds:[buffer+bx] ;get char from buffer
            inc bx
            mov ds:[bufferpos],bx ;save new position in buffer
            mov bx,ds:[inbuffer]
            dec bx
            mov ds:[inbuffer],bx ;save new number of chars still in buffer
        charreturned:
      pop bx
      pop cx
      pop dx
          ret
    get_char endp

    loadbuffer proc
      push bx
      push dx
      push ax
          mov bx,ds:[handler1]
        	mov cx,500 ;500 bytes to load
        	mov ah,3Fh
        	mov dx,offset buffer
        	int 21h

        	mov cx,0
        	mov ds:[bufferpos],cx ;initial bufferpos is 0
        	cmp ax,0
        	jne more_to_load ;if none loaded return eof
        	mov cl,1
        	more_to_load:
        	mov ds:[inbuffer],ax ;number of bytes loaded to buffer
      pop ax
      pop dx
      pop bx
        	ret
    loadbuffer endp
    ;-----------------------------------------------------------------------
    ;errory:
    error_command_line proc
      mov dx, offset wrong_arguments
      mov ah,9
      int 21h
      call kill_program
    error_command_line endp

    error_while_opening proc
      mov dx, offset file_open_error
      mov ah,9
      int 21h
      call kill_program
    error_while_opening endp

    error_while_closing_file proc
      mov dx, offset file_close_error
      mov ah,9
      int 21h
      call kill_program
    error_while_closing_file endp

    error_commands proc
      mov dx, offset wrong_command_error
      mov ah,9
      int 21h
      call kill_program
    error_commands endp

    ;----------------------------------------------------------------------
    ;killer:
    kill_program proc
          mov   ah, 4ch
          int   21h
          ret
    kill_program endp
    ;--------------------------------------------


  ;--------------------------------------------
  ;parser:
  parse proc
        xor cx , cx
        mov cl , byte ptr ds:[80h]   ;długość stringa z argumentami
        mov si , 82h                ; ds:ds:[si] początek buffora
        mov ax , seg cmd_arg
        mov es , ax                 ;es -> segment z wartosciami
        mov di , offset cmd_arg     ;es:ds:[di] na początek cmd_arg
    l1:
        cmp cx , 0
        je return1
        call EraseSPACE
        cmp cx , 0
        je return1
        ;nowy argument
        xor bx , bx
        mov bl , byte ptr es:ds:[cmd_count]
        shl bx , 1                              ;adresujemy dwu bajtowy word więc powiększamy ilosc pamięci o 2
        mov word ptr es:[cmd_offset + bx] , di  ; wrzucamy pod es:[...] nasz parametr
        inc byte ptr es:[cmd_count]            ;mamy jeden więcej argument

        call CopyToMem

        jmp l1
    return1:
        ret
  parse endp

  ;--------------------------
  EraseSPACE proc
    l2:                 ;do pętli
        cmp cx , 0
        je return2
        dec cx
        lodsb          ;al=ds:[si]  si++
        ;szukamy białych znaków
        cmp al , 20h  ;spacja
        je l2
        cmp al , 09h  ;tab
        je l2
        cmp al , 13  ;carriage-return
        je l2
        ;else
        inc cx
        dec si
    return2:
        ret
  EraseSPACE endp

  ;------------------------
  CopyToMem proc
    l3:
        cmp cx , 0
        je return3
        dec cx
        lodsb         ;al= ds:[si] si++
        inc byte ptr es:[cmd_len + bx]
        cmp al , 20h    ;spacja
        je done
        cmp al , 09h    ;tab
        je done
        cmp al , 13    ;form-feed
        je done
        cmp al , '$'
        je done
        ;else
        stosb       ; es:[di] = al, di++
        jmp l3
    done:
        inc cx
        dec si
        inc di      ;zostawia $ jako przerwe
        dec byte ptr es:[cmd_len + bx]
    return3:
        ret
  CopyToMem endp



code ends

_Stos segment stack
	dw 256 dup(?)
  wstosu	dw ?
_Stos ends

END start
