name "loader"
; directive to create boot file
#make_boot#

org 7c00h

; initialize the stack
mov     ax, 07c0h
mov     ss, ax
mov     sp, 03feh ; top of the stack

; set data segment:
xor     ax, ax
mov     ds, ax

; set default video text mode
mov     ah, 00h
mov     al, 03h
int     10h

; BIOS passes drive number in dl,
; so it's not changed:

mov     ah, 02h ;We need to read the sectors where our program is
mov     al, 5   ;Our program is about 2,2 KB, so it's stored on five sectors, each one being 512 bytes max
mov     ch, 0   ;The cylinder, our program is on cylinder 0
mov     cl, 2   ;The sector, our program is on sector 2
mov     dh, 0   ;The head, our program is on head 0
;We don't need to change dl, as the correct value of the drive number
;is already in it

;es:bx points to receiving
;data buffer
mov     bx, 0800h   
mov     es, bx
mov     bx, 0

;We read the corresponding sector where our main program is stored
int     13h

;We go to our program now that we read its sector
jmp     0800h:0000h  