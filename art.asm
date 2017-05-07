.186

data segment
    cmd_count   db 0              ;ilosc argumentow
    cmd_len  dw 150 dup(?)     ;dl argumentow
    cmd_offset  dw 150 dup(?)     ;offsety
    cmd_arg     db 300 dup('$')   ;wartosci
    newline     db 13,10,"$"

    ART_TABLE   db 153 dup(0)    ;ilosc odwiedzin
    ART_code    dw 16 dup(?)     ;rozbite na ruchy
    ART_char    db " ",".","o","+","=","*","B","O","X","@","%","&","#","/","^"
    line        db "+", 17 dup("-"), "+", 13 , 10, "$" ;linia start/koniec

    error_ilosc db "Blad ilosci argumentow",13,10,"$"
    error_typ   db "Blad typu argumentu",13,10,"$"

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

    mov cl, byte ptr ds:[cmd_count] ;ilosc argumentow
    cmp cl , 2
    jne Error_il

    call konwersja   ;przyjumje tablice cmd_arg daje wyniki w tablicy ART_code
    cmp byte ptr ds:[cmd_arg] , '1'
    je modyfikacja
FLAGA_powrotu_dla_modyfkiacji:
    call Dbishop     ;przyjmuje tablice ART_code daje wynik w tablicy ART_TABLE
    call wypisz      ;formatuje wyjście i wypisuje ART_TABLE
quit:                               ;UNIWERSALNE DLA CAŁEGO PROGRAMU
    mov ah , 4Ch
    int 21h
Error_il:
    mov dx , offset error_ilosc
    mov ah , 9
    int 21h
    jmp quit
modyfikacja:
    call modyf
    jmp FLAGA_powrotu_dla_modyfkiacji
    ;--------------------------------------
konwersja proc
    pusha
    mov cx , 16
    mov si , 0
    mov di , word ptr ds:[cmd_offset +2] ;drugi argument
Kopiowanie:
    mov ah , byte ptr ds:[di]            ; ah 1 literka w HEXie
    inc di
    mov al , byte ptr ds:[di] ; druga literka w HEXie
    inc di
    call HEXtoBIN     ;dostaje w ah jednego HEXa daje wynik w ah
    mov bl , ah
    shl bl , 4        ;przesuwamy na starszą 4 bitów
    mov ah , al
    call HEXtoBIN
    add bl , ah
    mov byte ptr ds:[ART_code+si] , bl
    inc si
    loop Kopiowanie
    popa
    ret
konwersja endp
; --------------------------
HEXtoBIN proc
    cmp ah , '0'
    jb  Error_t     ;jump if bellow
    cmp	ah, '9'
          jna od0do9  ;jump if not above ---> daje nierówność 0<=ah<=9
    cmp	ah, 'f'
    ja  Error_t     ;jump if above
    cmp ah , 'a'
          jnb odadof  ;jump if not bellow --> daje nierówność a<=ah<=f
od0do9:
    sub ah , '0'
    ret
odadof:
    sub ah , 'a'
    add ah , 10
    ret
Error_t:
    mov dx , offset error_typ
    mov ah , 9
    int 21h
    jmp quit
HEXtoBIN endp
;-----------------------------------------
modyf proc
    pusha
    mov cx , 15
    mov si , 15
    mov bx , 0
    mov ax , 0
petla_przesuwania_bitow:
    mov ax , bx
    mov bx , 0
    shl byte ptr ds:[ART_code+si] , 1
    jc jest_nadmiarowy_bit
FLAGA_powrotu_do_przesuwania_bitow:
    add byte ptr ds:[ART_code+si] , al
    dec si
    dec cx
    cmp cx , 0
    jne petla_przesuwania_bitow
    ;--koniec pętli ale dla 0 tez trzeba wykonac
    mov ax , bx
    mov bx , 0
    shl byte ptr ds:[ART_code+si] , 1
    jc jest_nadmiarowy_bit_END
FLAGA_powrotu_do_przesuwania_bitow_END:
    add byte ptr ds:[ART_code+si] , al
    ;-- ostatni nadmiarowy bit wrzucamy do 15 bajtu
    add byte ptr ds:[ART_code+15] , bl
    popa
    ret
jest_nadmiarowy_bit:
    mov bl , 1
    jmp FLAGA_powrotu_do_przesuwania_bitow
jest_nadmiarowy_bit_END:
    mov bl , 1
    jmp FLAGA_powrotu_do_przesuwania_bitow_END
modyf endp
;-----------------------------------------
Dbishop proc  ; w si zostaje położenie koncowe
    push di
    push cx
    push bx
    mov si , 76
    mov di , 0
    mov cl , 16
Argumenty_tablicy:
    mov bl , byte ptr ds:[ART_code + di] ;bl aktualna komorka tablicy ART_code
    mov ch , 4
Pary_bitow:
    shr bl , 1 ;ostatni bit jest wrzucany do flagi carry całość jest przesuwana o 1 w prawo
    jc Right   ;w srodku sprawdzamy gora dol
    jmp Left   ;-||-
FLAGA_powrotu_do_Pary_bitow:
    dec ch
    cmp ch , 0
    jne Pary_bitow
    ;-koniec pętli wew
    inc di
    dec cl
    cmp cl ,0
    jne Argumenty_tablicy
    ;-koniec pętli zew
    pop bx
    pop cx
    pop di
    ret
Right:
    push ax
    push dx
    ;-sprawdzamy czy mozna w prawo
    mov ax, si
    add ax , 1 ; jeżeli położenie = 17k -1
    mov dl , 17
    div dl
    cmp ah , 0
    je  NieWykonujRight
    inc si ; to jest przesunięcie w prawo
NieWykonujRight:
    pop dx
    pop ax ;żeby się potem z tym nie zgubić
    ;-sprawdzamy nastepny bit
    shr bl , 1
    jc Down
    jmp Up
Left:
    ;-sprawdzamy czy mozna w lewo
    push ax
    push dx
    mov ax , si
    mov dl , 17 ; jeżeli położenie = 17k
    div dl
    cmp ah , 0
    je  NieWykonujLeft
    dec si
NieWykonujLeft:
    pop dx
    pop ax ;żeby się potem z tym nie zgubić
    ;-sprawdzamy nastepny bit
    shr bl , 1
    jc Down
    jmp Up
Up:
    ;sprawdzamy czy mozna
    cmp si , 17
    jb NieWykonujUp
    sub si ,17
NieWykonujUp:
    inc byte ptr ds:[ART_TABLE + si]
    jmp FLAGA_powrotu_do_Pary_bitow
Down:
    ;sprawdzamy czy mozna
    cmp si , 135 ; jak index >135 to nie da się skoczyć w dół
    ja NieWykonujDown
    add si , 17
NieWykonujDown:
    inc byte ptr ds:[ART_TABLE + si]
    jmp FLAGA_powrotu_do_Pary_bitow
Dbishop endp
;----------------------------------------
wypisz proc
    ;mov bx , si
    call Pusta_linia
    mov ch , 9
    mov di , 0
    mov dx , 0
jedna_linia_z_ART_TABLE:
    mov dx , '|'
    mov ah , 2h
    int 21h
    mov cl , 17
jeden_znak_z_ART_TABLE:
    cmp di , si
    je koncowa_pozycja_skoczka
    cmp di , 76
    je poczatkowa_pozycja_skoczka
    cmp ds:[ART_TABLE + di] , 14
    ja wiecej_niz_14
    mov bl , byte ptr ds:[ART_TABLE + di]
    mov dl , byte ptr ds:[ART_char + bx] ;wybieramy znaczek w kolejności od początku tablicy
    jmp wypisz_znak
    wiecej_niz_14:
    mov dl , byte ptr ds:[ART_char + 14]
    wypisz_znak:
    mov ah , 2h
    int 21h
    inc di
    dec cl
    cmp cl , 0
    jne jeden_znak_z_ART_TABLE
;--koniec petli wewnetrznej
    mov dl , '|'
    mov ah , 2h
    int 21h
    mov dx , offset newline
    mov ah , 9h
    int 21h
    dec ch
    cmp ch , 0
    jne jedna_linia_z_ART_TABLE
;--koniec petli zewnetrznej

    call Pusta_linia
    ret
koncowa_pozycja_skoczka:
    mov dl , 'E'
    jmp wypisz_znak
poczatkowa_pozycja_skoczka:
    mov dl , 'S'
    jmp wypisz_znak
wypisz endp
;------------------------------------------
Pusta_linia proc
    mov dx , offset line
    mov ah , 9
    int 21h
    ret
Pusta_linia endp



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
