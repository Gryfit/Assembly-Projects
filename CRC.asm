.186

data segment
    cmd_count   db 0              ;ilosc argumentow
    cmd_len  dw 150 dup(?)     ;dl argumentow
    cmd_offset  dw 150 dup(?)     ;offsety
    cmd_arg     db 300 dup(0)   ;wartosci
    newline     db 13,10,"$"
    temp db 0,"$"


     crc16 dw 0,"$"
	   working_case	db 1
     buffor	db 	16 dup(0)
     buffor_size db 0
	   handler1 dw	0
      no_args      db  "Brak argumentow",13,10,"$"
    wrong_args 	db	"Argumenty nie spelniaja zalozen zadania",13,10,"$"
    wrong_prefix db "Pierwszy argument nie spelnia zalozen, rozny od -v",13,10,"$"
    file_open_error db "Nie udalo sie otworzyc pliku",13,10,"$"
    file_save_error db "Nie udalo sie zapisac do pliku",13,10,"$"
    file_close_error	db	"Nie udalo sie zamknac pliku",13,10,"$"
    zgodnosc_str db "Dwa pliki maja zgodne CRC16",13,10,"$"
    brak_godnosci_str db "Dwa pliki nie maja zgodnego CRC16",13,10,"$"

    crc16_lo db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
    	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
    crc16_hi db 000h, 0c0h, 0c1h, 001h, 0c3h, 003h, 002h, 0c2h
    	db 0c6h, 006h, 007h, 0c7h, 005h, 0c5h, 0c4h, 004h
    	db 0cch, 00ch, 00dh, 0cdh, 00fh, 0cfh, 0ceh, 00eh
    	db 00ah, 0cah, 0cbh, 00bh, 0c9h, 009h, 008h, 0c8h
    	db 0d8h, 018h, 019h, 0d9h, 01bh, 0dbh, 0dah, 01ah
    	db 01eh, 0deh, 0dfh, 01fh, 0ddh, 01dh, 01ch, 0dch
    	db 014h, 0d4h, 0d5h, 015h, 0d7h, 017h, 016h, 0d6h
    	db 0d2h, 012h, 013h, 0d3h, 011h, 0d1h, 0d0h, 010h
    	db 0f0h, 030h, 031h, 0f1h, 033h, 0f3h, 0f2h, 032h
    	db 036h, 0f6h, 0f7h, 037h, 0f5h, 035h, 034h, 0f4h
    	db 03ch, 0fch, 0fdh, 03dh, 0ffh, 03fh, 03eh, 0feh
    	db 0fah, 03ah, 03bh, 0fbh, 039h, 0f9h, 0f8h, 038h
    	db 028h, 0e8h, 0e9h, 029h, 0ebh, 02bh, 02ah, 0eah
    	db 0eeh, 02eh, 02fh, 0efh, 02dh, 0edh, 0ech, 02ch
    	db 0e4h, 024h, 025h, 0e5h, 027h, 0e7h, 0e6h, 026h
    	db 022h, 0e2h, 0e3h, 023h, 0e1h, 021h, 020h, 0e0h
    	db 0a0h, 060h, 061h, 0a1h, 063h, 0a3h, 0a2h, 062h
    	db 066h, 0a6h, 0a7h, 067h, 0a5h, 065h, 064h, 0a4h
    	db 06ch, 0ach, 0adh, 06dh, 0afh, 06fh, 06eh, 0aeh
    	db 0aah, 06ah, 06bh, 0abh, 069h, 0a9h, 0a8h, 068h
    	db 078h, 0b8h, 0b9h, 079h, 0bbh, 07bh, 07ah, 0bah
    	db 0beh, 07eh, 07fh, 0bfh, 07dh, 0bdh, 0bch, 07ch
    	db 0b4h, 074h, 075h, 0b5h, 077h, 0b7h, 0b6h, 076h
    	db 072h, 0b2h, 0b3h, 073h, 0b1h, 071h, 070h, 0b0h
    	db 050h, 090h, 091h, 051h, 093h, 053h, 052h, 092h
    	db 096h, 056h, 057h, 097h, 055h, 095h, 094h, 054h
    	db 09ch, 05ch, 05dh, 09dh, 05fh, 09fh, 09eh, 05eh
    	db 05ah, 09ah, 09bh, 05bh, 099h, 059h, 058h, 098h
    	db 088h, 048h, 049h, 089h, 04bh, 08bh, 08ah, 04ah
    	db 04eh, 08eh, 08fh, 04fh, 08dh, 04dh, 04ch, 08ch
    	db 044h, 084h, 085h, 045h, 087h, 047h, 046h, 086h
    	db 082h, 042h, 043h, 083h, 041h, 081h, 080h, 040h

    hi db 0,"$" ; high byte of CRC
    lo db 0 ,"$"; low byte of CRC


    outbuffer db 0,0,0,0,0;for printing integers as chars

    buffer db 500 dup (0) ;file reading buffer
    inbuffer dw 0 ;how many in buffer
    bufferpos dw 0 ;current position in buffer
    finishedreading db 0d ;eof?

data ends

code segment
start:
    ;stos
    mov ax , seg wstosu
    mov ss , ax
    mov sp , offset wstosu
    ;wczytywanie
    call parse

    mov ax , data   ;nie ma co powtarzać tej czynnosci za kazdym razem
    mov ds , ax
    xor ax ,ax

    call check_input
    call open_file

    call make_crc


    call close_file
    call open_2_file

    cmp ds:[working_case] , 1
    je first_case

    second_case:
     call get_second_crc
     call compare
     call close_file
     call kill_program
    first_case:
    call save_crc
    call close_file
    call kill_program

;----------------------------------------------------
check_input proc
      cmp ds:[cmd_count] , 2
      je first_imput
      cmp ds:[cmd_count] , 3
      je second_imput
      jmp error_args

      second_imput:
      inc ds:[working_case]
      mov bh , ds:[cmd_arg]
      mov bl , ds:[cmd_arg + 1]
      cmp bx , "-v"
      jne error_prefix
      ret
      first_imput:
      ret
check_input endp

    ;--------------------------
    ;Przerwania na plikach:
    ;3Ch ->utworzenie
    ;3Eh ->zamkniecie
    ;3Dh-> otworzenie (0->odczyt, 1->zapis, 2 -> both)
open_file proc
    mov cx,0
    mov ax , data
    mov ds , ax

    cmp ds:[working_case] , 2
    je open_2_flag
    mov dx , ds:[cmd_offset]
    jmp back_form_open_2_problem
    open_2_flag:
    mov dx , ds:[cmd_offset+2]
    back_form_open_2_problem:
    xor al , al
    mov ah,3dh
    int 21h

    jc error_while_opening
    mov ds:[handler1],ax				;handler->identyfikator pliku
    ret
open_file endp

close_file proc
    	mov bx , ds:[handler1]
    	mov ah,3eh
    	int 21h
    	jc error_while_closing_file			;CF==0 -> ok, CF==1 not ok
      ret
close_file endp

open_2_file proc
      mov cx,0
      mov ax , data
      mov ds , ax

      cmp ds:[working_case] , 2
      je read_open
      write_open:
      mov dx , ds:[cmd_offset+2]
      mov al , 1
      jmp back_form_open_2_problem
      read_open:
      mov dx , ds:[cmd_offset+4]
      mov al , 0
      back_form_open_2_problem:
      mov ah,3dh
      int 21h

      jc error_while_opening
      mov ds:[handler1],ax				;handler->identyfikator pliku
      ret
open_2_file endp


save_crc proc
pusha
    xor ax ,ax
    mov al,ds:[lo]
    mov dx, 0
    mov bx, 16
    div bx    	; divide DX:AX by 16. AX=quotient, DX=remainder
    cmp dx,10
    jl number3
    add dx,'A'
    sub dx ,10
    mov ds:[outbuffer + 3] , dl
    jmp end3
number3:
    add dx,'0'
    mov ds:[outbuffer + 3] , dl
end3:
    cmp ax,10
    jl number2
    add ax,'A'
    sub ax ,10
    mov ds:[outbuffer + 2] , al
    jmp end2
number2:
    add ax,'0'
    mov ds:[outbuffer + 2] , al
end2:
    xor ax ,ax
    mov al, ds:[hi]
    mov dx, 0
    mov bx, 16
    div bx    	; divide DX:AX by 16. AX=quotient, DX=remainder
    cmp dx,10
    jl number1
    add dx,'A'
    sub dx ,10
    mov ds:[outbuffer + 1] , dl
    jmp end1
number1:
    add dx,'0'
    mov ds:[outbuffer + 1] , dl
end1:
    cmp ax,10
    jl number0
    add ax,'A'
    sub dx ,10
    mov ds:[outbuffer] , al
    jmp end0
number0:
    add ax,'0'
    mov ds:[outbuffer] , al
end0:
     mov dx , offset outbuffer
     mov bx , ds:[handler1]
     mov cx, 5
     mov ah,40h
     int 21h
     popa
     ret
save_crc endp


;changecrc16 -> change crc16 values in [hi] and [lo] using lookup tables depending on input byte
;ax - input byte
;Lookup table method:
;I = ([lo]) XOR (input byte)
;new [hi] = crc16_hi[I]
;new [lo] = (crc16_lo[I]) XOR [hi]
make_crc proc
  crcloop:
  call getchar ; gets char into ax and sets finishedreading if required
  cmp ds:[finishedreading],1
  je crcdone
  pusha
  xor cx,cx
  xor dx,dx

  mov cl,ds:[lo]
  xor cx,ax	; create offset
  mov bx,cx	; save offset for tables
  mov dl,ds:[crc16_hi+bx] ;new hi

  mov al,ds:[crc16_lo+bx]
  xor cx,cx
  mov cl,ds:[hi]
  xor ax, cx ;new lo =(crc16_lo[I]) XOR [hi]

  mov ds:[hi],dl
  mov ds:[lo],al
  popa
  jmp crcloop
  crcdone:
  ret
make_crc endp



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
  	ret
loadbuffer endp


compare proc
    pusha
      mov dl,ds:[hi]
      mov dh,ds:[lo]
      pusha
      mov dx, offset hi
      mov ah ,9
      int 21h
      mov dx, offset lo
      mov ah ,9
      int 21h
      popa

      mov ax , ds:[crc16]
      cmp ax,dx	;compare
      jne nequal ; if equal
      mov ah ,9
      mov dx , offset zgodnosc_str
      int 21h
      jmp after_comp
      nequal:
      mov ah ,9
      mov dx , offset brak_godnosci_str
      int 21h
    after_comp:
    popa
    ret
compare endp

get_second_crc proc
    push dx
    push cx
    xor cx,cx

    call getchar ;wynik w al
    mov ah, al
    call HEXtoBIN
    shl ah , 4
    mov cl ,ah

    call getchar ;wynik w al
    mov ah, al
    call HEXtoBIN
    add cl , ah ; mamy pierwsze 2 liczby łiiiii

    call getchar ;wynik w al
    mov ah, al
    call HEXtoBIN
    shl ah , 4
    mov ch , ah

    call getchar ;wynik w al
    mov ah, al
    call HEXtoBIN
    add ch , ah ; mamy drugie 2 liczby łiiiii

    mov ds:[crc16], cx

    pop cx
    pop dx
    ret
get_second_crc endp

HEXtoBIN proc
    cmp	ah, '9'
    jna od0do9  ;jump if not above ---> daje nierówność 0<=ah<=9
    cmp ah , 'a'
    jnb odadof  ;jump if not bellow --> daje nierówność a<=ah<=f
od0do9:
    sub ah , '0'
    ret
odadof:
    sub ah , 'a'
    add ah , 10
    ret
HEXtoBIN endp



kill_program proc
      mov   ah, 4ch
      int   21h
      ret
kill_program endp
;------------------------------------------------

error_prefix proc
  mov dx,offset wrong_prefix
  mov ah,9h
  int 21h
  call kill_program
error_prefix endp
error_args proc
  mov dx, offset wrong_args
  mov ah,9h
  int 21h
  call kill_program
error_args endp
error_while_saving proc
  mov dx, offset file_save_error
  mov ah,9h
  int 21h
  call kill_program
error_while_saving endp
error_while_opening proc
  mov dx, offset file_open_error
  mov ah,9h
  int 21h
  call kill_program
error_while_opening endp
error_while_closing_file proc
  mov ax, seg file_close_error
  mov ds,ax
  mov dx, offset file_close_error
  mov ah,9h
  int 21h
  call kill_program
error_while_closing_file endp



;--------------------------------------------
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
