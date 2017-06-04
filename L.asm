.386

data segment

    cmd_count   db 0              ;ilosc argumentow
    cmd_len  dw 150 dup(?)     ;dl argumentow
    cmd_offset  dw 150 dup(?)     ;offsety
    cmd_arg     db 300 dup(0)   ;wartosci

    handler1 dw	0

    buffer db 500 dup (0) ;file reading buffer
    inbuffer dw 0 ;how many in buffer
    bufferpos dw 0 ;current position in buffer
    finishedreading db 0

    file_open_error db "Nie udalo sie otworzyc pliku",13,10,"$"
    file_close_error	db	"Nie udalo sie zamknac pliku",13,10,"$"
    wrong_command_error db "Nieznana komenda",13,10,"$"
    not_a_number_error db "Podano nieprawidlowa liczbe",13,10,"$"
    wrong_arguments db "Podano nieprawidlowe argumenty",13,10,"$"

    is_drawing db 1


    yLonger db 0
    shortLen dw 0
    longLen dw 0
    incrementVal dw 0
    divDiff dq 0.0
    temp dw 0
    prawiePol dt 0.49999999

    l180 dt 180.0 ;180.0 - orzyda sie przy przeliczaniu katow ze stopni na radiany
  	tylkoOdczyt EQU 00010000b ;stala - plik tylko do odczytu, denyall
  	tempLiczba dw 0;j.w.

  	txtSpacja db " $" ;spacja i dosowy znak konca linii
  	txtEnter db 10, 13, "$" ;przejscie do nowej linii

  	x1 dw 320 ;wspolrzedne punktu poczatkowego i koncowego
  	x2 dw 0
  	y1 dw 240
  	y2 dw 0
  	d  dw 100 ;dlugosc odcinka
  	kat dt 0.0
  	kolor equ 1

data ends


code segment use16
start:
    mov ax , seg wstosu
    mov ss , ax
    mov sp , offset wstosu


    call parse
    ;wczytywanie
    mov ax , data   ;nie ma co powtarzać tej czynnosci za kazdym razem
    mov ds , ax


    call check_command_line
    call open_file



    mov ah, 0
		mov al, 12h
		int 10h ;przechodzimy do trybu graficznego 12h - rozdzielczosc 640x480 16 kolorow

		call wczytywanie ;wczytyujemy plik i wykonujemy zawarte w nim polecenia

		mov ax, 0;oczekiwanie na wcisniecie klawisza - zeby dalo sie zobaczyc dzielo ;]
		int 16h

		mov ah, 00h ;wracamy do tekstowego trybu graficznego
		mov ax, 03h;
		int 10h;

    call close_file
    call kill_program

;--------------------------------------------------------------


fisttp macro arg1
    ;makro zastepujace instrukcje fisttp. Instrukcja ta zdejmuje ze stosu (st0)
    ;wartosc i kopiuje ja do zmiennej calkowitej zaokraglajac w dol. Makro robi dokladnie to samo tylko ze w kilku isntrukcjach.
    ;arg1 - zmienna do ktorej chcemy skopiowac zaokraglona wartosc
    fld ds:[prawiePol] ;st0 = 0.499999, st1= stare st0
    fsub st(1), st(0) ;st1 = st0 i st1 (czyli odejmujy od liczby 0.499999)
    fstp st(0); wywalamy st0
    fistp arg1 ;zdejmuje st0 ze stosu i po konwersji(zaokragleniu) kopiujemy do zmiennej.
endm

get_integer_from_file proc
  ;daje liczbe w dx
  push	ax
  push	bx
  push	di

  xor	dx, dx
  xor	ax, ax
  xor	bx, bx
  get_integer_loop:
    call	getchar
    cmp al , 10
    je	end_of_get_integer_loop
    cmp	al, 13
    je	end_of_get_integer_loop
  ;  cmp	al, '0'
  ;  jae	possibly_number ;above or equal
  ;  call error_not_a_number
  ;  possibly_number:
  ;  cmp	al, '9'
  ;  jbe	certainly_number
  ;  call error_not_a_number
  ;  certainly_number:
    sub	al, '0' ; konwersja ascii -> int
    push	ax		; odkladam na stos zeby odwrocic potem kolejnosc
    inc	bx		; licze ile cyfr ma liczba
  jmp	get_integer_loop
  end_of_get_integer_loop:
  ; obliczanie liczby wyciagajac ze stosu w odwrotnej kolejnosci
  mov cx ,bx
  mov	di, 1	; to bedzie czynnik z kolejnymi potegami 10
  evaluate_loop:
    pop	ax
    imul	ax , di	; dx += ax*(10)^(di)
    add	dx , ax
    imul di , 10	; kolejna potega 10
  loop	evaluate_loop ;while cx != 0
  mov ds:[tempLiczba] , dx
  pop	di
  pop	bx
  pop	ax
  ret
get_integer_from_file endp




wartoscBezwzgledna proc
  cmp ax, 0
  jge wieksza ;jezeli ax jest >= 0 to skaczemy na koniec procedury, jezeli nie to zmieniamy jego znak
  neg ax ; ax = -ax
  wieksza:
  ret
wartoscBezwzgledna endp




rysujLinie proc
      ;procedura rysujaca linie, parametry: x, y, x2, y2
      mov al, 0
      mov ds:[yLonger], al ;zerujemy yLonger

      mov ax, ds:[y2] ; ax = y2
      sub ax, ds:[y1] ;ax = ax - y
      mov ds:[shortLen], ax ; shortLen = y2-y

      mov ax, ds:[x2] ;ax = x2
      sub ax, ds:[x1] ;ax = ax - x
      mov ds:[longLen], ax ; lognLen =ax  //x2-


      mov ax, ds:[shortLen]
      call wartoscBezwzgledna ;abs(shortLen), wynik w ax
      push ax ;odkladamy na pozniej


      mov ax, ds:[longLen]
      call wartoscBezwzgledna
      pop bx ; bx = odlozona na pozniej wartosc z ax, czyli abs(shortLen)
      ;ax = abs(longLen), bx = abs(shortLen)

      cmp bx, ax
      jle lessEqual ;sprawdzenie czy abs(shortLen)>abs(longLen)
      ;tak - abs(shortLen)>abs(longLen)
      push ds:[longLen]
      push ds:[shortLen]
      pop ds:[longLen]
      pop ds:[shortLen]
      ;zamieniamy wartosc shortLen z longLen (swap)
      mov al, 1
      mov ds:[yLonger], al ; i ustawiamy yLonger na true

      lessEqual: ;nie - abs(shortLen)<=abs(longLen)
      mov ax, ds:[longLen]
      cmp ax, 0
      jge AbEq; longLen>=0
      ;tak
      mov ax, -1
      mov ds:[incrementVal], ax ;incrementVal=-1
      jmp next

      AbEq: ;nie - longLen<0
      mov ax, 1
      mov ds:[incrementVal], ax ;incrementVal=1

      next:
      finit;inicjujemy fpu
      mov ax, ds:[shortLen]
      cmp ax, 0; sprawdzenie czy shortLen==0
      jne NotEqual
      ;tak
      fild ds:[longLen] ;st0=(double)longLen
      jmp qweasd

      NotEqual: ;nie - shortLen != 0
      fild ds:[longLen] ;st0 = (double) longLen
      fild ds:[shortLen] ;st0 = (double) shortLen, stara wartosc st0 przesunela sie na st1, czyli teraz st0 to (double) shortLen, a st1 to (double) longLen
      fdiv st(1), st(0) ;st1 = st1/st0
      fstp st(0) ;wywalamy st0
      qweasd:
      fstp ds:[divDiff] ;divDiff = st0 i zdejmuje st0 ze stosu


      mov bx, 0 ; licznik(bedzie potrzebny przy adresowaniu wspolrzednych punktow)
      mov al, ds:[yLonger]
      cmp al, 0; sprawdzenie czy yLonger == 0
      je yLongerEqualZero
      ;nie - yLonger != 0
      mov cx, 0
      cmp ds:[longLen], cx ; cx==0 ?
      je end_e ;tak - skaczemy do end
      ;nie - cx!=0




      loopBeginning:
      fld ds:[divDiff] ;st0 = divDiff
      mov ds:[temp], cx
      fild ds:[temp] ;st0 = (double) temp, stare st0 przesunelo sie na st1
      fdiv st(0), st(1) ;st0 = st0/st1 => cx/divDiff

      fisttp ds:[temp]
      fstp st(0) ;wywlamy st0

      mov dx, ds:[y1]
      add dx, cx ;dx = y+cx //cx sluzy za licznik
      mov ax, ds:[temp]
      add ax, ds:[x1] ;ax = temp + x, czyli cx/divDiff + x



      push cx ;odkladamy bx i cx na stos - na potem

        mov cx, ax
        mov dx, dx
        mov al, ds:[kolor]
        mov ah, 0Ch
        mov bx, 0
        int 10h
      pop cx
      add cx, ds:[incrementVal] ;cx = cx+ incrementVal
      cmp cx, ds:[longLen]
      je end_e ;jezeli cx == longLen to skaczemy do atykiety end
      jmp loopBeginning

      yLongerEqualZero: ;tak - yLonger ==0 //sprawdzenie czy yLonger == 0
      mov cx, 0
      cmp ds:[longLen], cx ;cx==0 ?
      je end_e ;tak - skaczemy do end
      ;nie - cx!=0


      loop2Beginning:
      fld ds:[divDiff] ;st0 = divDiff
      mov ds:[temp], cx
      fild ds:[temp] ;st0 = (double) temp, stare st0 przesunelo sie na st1
      fdiv st(0), st(1) ;st0 = st0/st1 => cx/divDiff

      fisttp ds:[temp] ;temp = st0(obcinajac koncowke sto, czyli np z 1,7 wyjdzie 1), zdejmuje rowniez st0 ze stosu fpu
      fstp st(0)  ;wywalamy st0

      mov dx, ds:[y1]
      add dx, ds:[temp] ;bx = y+cx/divDiff
      mov ax, ds:[x1]
      add ax, cx ;ax = x+i


      push cx
        mov cx, ax
        mov dx, dx
        mov al, ds:[kolor]
        mov ah, 0Ch
        mov bx, 0
        int 10h
      pop cx
      add cx, ds:[incrementVal] ;cx = cx+ incrementVal
      cmp cx, ds:[longLen] ;jezeli cx == longLen to skaczemy do atykiety end
      je end_e
      jmp loop2Beginning

      end_e:
      ret
rysujLinie endp

wczytywanie proc
 ; wczytuje kolejne znaki, interpretuje je jako komendy i nastepne wykonuje te komendy.
 pusha
wczytywanie_petla:
		call getchar ;wczytujemy kolejny znak
    cmp ds:[finishedreading], 1
    je wczytywanie_koniec
    cmp al, 10
    je wczytywanie_petla
    cmp al , 13
    je wczytywanie_petla
    cmp al, 0
    je wczytywanie_petla

    cmp	al, 'u'
    je	up
    cmp al , 'd'
    je down

		cmp al, 'r'
		je rotate
		cmp al, 'm'
		je move
up:
    mov ds:[is_drawing] , 0
    jmp wczytywanie_petla
down:
    mov ds:[is_drawing] , 1
    jmp wczytywanie_petla

rotate:

    call getchar  ; bo jest spacja
    call get_integer_from_file

		fild ds:[tempLiczba] ;templiczba leci do st0
    fld ds:[l180];ladujemy 180.0 na stos, czyli teraz st0 = 180.0, st1 =  w stopniach
    fdiv st(1), st(0) ;st1 = st1 * st0
    fstp st(0);0 ;wywalamy st0; st0 =  w stopniach * 180
    fldpi ;ladujemy na stos pi
    fmul st(1), st(0) ;st1 = st1 * st0
    fstp st(0);wywalamy st0
		fld ds:[kat] ;st0 = kat, st1 = templiczba
		fadd st(0), st(1) ;dodajemy do aktualnego kata wczytana przed chwila wartosc
		fstp ds:[kat] ;i zdejmuje nowy kat ze stosu i wrzucamy go do zmiennej kat
		fstp st(0) ;wywalamy st0 - sprzatamy stos
		jmp wczytywanie_petla



move:

    call getchar  ; bo jest spacja
		call get_integer_from_file

    pusha
    mov dx ,  ds:[tempLiczba]
    mov ds:[d] , dx
    popa


    fild ds:[tempLiczba]
    fld ds:[kat] ;st0 = kat, st1 = d
    fsin
    fmul st(0), st(1)
    fild ds:[x1] ;st0 = x1, st1 = sin(kat)*d, st2 = d
    fadd st(0), st(1)
    fistp ds:[x2]
    fstp st(0) ;st0 = d
    fld ds:[kat] ;st0 = kat, st1 = d
    fcos
    fmul st(0), st(1)
    fild ds:[y1] ;st0 = y1, st1 = cos(kat), st2 = d
    fsub st(0), st(1)
    fistp ds:[y2]
    ;czyscimy stos
    fstp st(0)
    fstp st(0)


		cmp ds:[is_drawing], 0
		je nie_rysuj ;jezeli pisak nie jest aktywny to pomijamy rysowanie
		call rysujLinie

nie_rysuj:
		;ustawiamy punkty koncowe lini jako punkty poczatkowe(dzieki temu bedziemy dalej rysowac od miejsca w ktorym skonczylismy)
		mov ax, ds:[x2]
		mov ds:[x1], ax
		mov ax, ds:[y2]
		mov ds:[y1], ax
		jmp wczytywanie_petla

wczytywanie_koniec:
  popa
    ret
wczytywanie endp


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
getchar proc
  push dx
  push cx
  push bx
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
getchar endp

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

error_not_a_number proc
  mov dx, offset not_a_number_error
  mov ah,9
  int 21h
  call kill_program
error_not_a_number endp
;----------------------------------------------------------------------
;killer:
kill_program proc
      mov   ah, 4ch
      int   21h
      ret
kill_program endp
;--------------------------------------------
;parser:
parse proc
      xor cx , cx
      mov cl , byte ptr ds:[80h]   ;długość stringa z argumentami
      mov si , 82h                ; ds:[si] początek buffora
      mov ax , seg cmd_arg
      mov es , ax                 ;es -> segment z wartosciami
      mov di , offset cmd_arg     ;es:[di] na początek cmd_arg
  l1:
      cmp cx , 0
      je return1
      call EraseSPACE
      cmp cx , 0
      je return1
      ;nowy argument
      xor bx , bx
      mov bl , byte ptr es:[cmd_count]
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

end start
