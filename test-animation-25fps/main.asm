	org 0x8000
phase1b:
	include "fighter1.asm"
phase1a:
	include "fighter1.asm"
phase2b:
	include "fighter4.asm"
phase2a:
	include "fighter4.asm"
	
copy_to_vram:
	proc
	;; hl - source buffer
	;; de - destination address
	di
	ld (_restore_sp+1),sp
	ld sp, hl
	ex de,hl
	ld (_restore_hl+1),hl
	ld c,3
_loop2:	
	ld b,8
_loop:	
	pop de
	ld (hl),e
	inc l
	ld (hl),d
	dec l
	inc h
	djnz _loop
	ld a,32
	add a,l
	ld l,a
	ld a,h
	sub 8
	ld h,a
	dec c
	jr nz,_loop2
_restore_hl:
	ld hl,0
	;; copy attributes
	;; convert hl address to attributes
	ld a,h
	and 0x18
	rrca
	rrca
	rrca
	ld b,a
	ld a,h
	and 0xC0
	or b
	or 0x18
	ld h,a
	;; address of attributes
	ld b,3
_loop_attrib:	
	pop de
	ld (hl),e
	inc l
	ld (hl),d
	dec l
	ld a,32
	add a,l
	ld l,a
	djnz _loop_attrib
_restore_sp:
	ld sp,0
	ei
	ret
	
	endp

animate_shift:       
        ;; hl - source address
        ;; de - destination address
        proc
        ld b,24
_loop:  call _shift_loop
        inc de
        inc de
        inc hl
        inc hl
        djnz _loop
        ld bc,6
        ldir
        ret
_shift_loop:
        ex de,hl
        scf
        rr (hl)
        inc hl
        rr (hl)
        dec hl
        ex de,hl
        rr (hl)
        inc hl
        rr (hl)
        dec hl
        ret
        endp


	;; 5B5c

toggle_screen:
	proc
	ld a,(0x5B5C)
	xor 0x08
	ld (0x5B5C),a
	ld bc,0x7FFD
	out (c),a
	ret
	endp

flip:
	proc
	call _real
_real:
	ld c,24
_loop2: 
        ld d,(hl)
        inc hl
        call _swap
        ld d,(hl)
        ld (hl),a
        dec hl
        call _swap
        ld (hl),a
        inc hl
        inc hl
        dec c
        jr nz, _loop2
        call _attributes
        call _attributes
_attributes:    
        ld d,(hl)
        inc hl
        ld a,(hl)
        ld (hl),d
        dec hl
        ld (hl),a
        inc hl
        inc hl
        ret
_swap:  
        ld b,8
        xor a
_loop1: rl d
        rra 
        djnz _loop1
        ret
        endp

clear:	proc
	call _real
_real:	
	ld a,0xFF
	ld b,48
_loop:	
	ld (hl),a
	inc hl
	djnz _loop
	ld bc,6
	add hl,bc
	ret
	endp
	
	
start:
	ld a,0
	out (254),a
	;; flip sprites
	ld hl,phase1a
	call flip
	ld hl,phase1b
	call clear
	ld hl,phase2a
	call flip
	ld hl,phase2b
	call clear

	
	ld a,(0x5B5C)
	and 0xF8
	or 7
	ld (0x5B5C),a
	ld bc,0x7FFD
	out (c),a
	
	;; use bank7
	
	ld a,0xff
	ld hl,0x4000
	ld de,0x4001
	ld (hl),a
	ld bc,6 * 1024 - 1
	ldir
	ld hl,0xC000
	ld de,0xC001
	ld (hl),a
	ld bc,6 * 1024 - 1
	ldir

	ld hl,phase1b
	ld de,phase1a
	call animate_shift	; field1
	call animate_shift	; field2
	ld hl,phase2b
	ld de,phase2a
	call animate_shift	; field1
	call animate_shift	; field2

	
	ld c,16
_loop_32
	ld b,16
_loop_16:	
	push bc
	;;  copy phases
_phase_a:	
	ld hl,phase2a
_position_1a:	
	ld de,0x4040
	call copy_to_vram
	ld hl,(_phase_a+1)
	ld de,54
	add hl,de
_position_2a:	
	ld de,0xC040
	call copy_to_vram
_phase_b:	
	ld hl,phase2b
_position_1b:	
	ld de,0x4042
	call copy_to_vram
	ld hl,(_phase_b+1)
	ld de,54
	add hl,de
_position_2b:	
	ld de,0xC042
	call copy_to_vram

	;;  half frame
	;;  shift phase
	ld hl,phase1b
	ld de,phase1a
_update1:
	nop
	call animate_shift	; field1
	call animate_shift	; field2
	ld hl,phase2b
	ld de,phase2a
_update2:
	nop
	call animate_shift	; field1
	call animate_shift	; field2

	halt
	call toggle_screen
	halt
	call toggle_screen
	pop bc

	ld a,b
	and 0x4
	jr z,_toggle_phase1
	
	ld hl,phase2a
	ld (_phase_a+1),hl
	ld hl,phase2b
	ld (_phase_b+1),hl
	jr _common2

_toggle_phase1:	
	ld hl,phase1a
	ld (_phase_a+1),hl
	ld hl,phase1b
	ld (_phase_b+1),hl
_common2:	
	
	djnz _loop_16

	ld a,c
	and 1
	jr nz,_other_position
	
	ld a, 0xeb 		; ex de,hl
	ld (_update1),a
	ld (_update2),a
	ld hl,(_position_1a+1)
	ld de,4
	add hl,de
	ld a,l
	and 0x1F
	or 0x40
	ld l,a
	ld (_position_1a+1),hl
	ld a,0x80
	or h
	ld h,a
	ld (_position_2a+1),hl
	jr _common
_other_position:
	ld a, 0			; nop
	ld (_update1),a
	ld (_update2),a
	ld hl,(_position_1b+1)
	ld de,4
	add hl,de
	ld a,l
	and 0x1F
	or 0x40
	ld l,a
	ld (_position_1b+1),hl
	ld a,0x80
	or h
	ld h,a
	ld (_position_2b+1),hl
	

_common:
	;; test for switch
	
	dec c
	jp _loop_32

	end start
