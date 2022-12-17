; YOUR NAME(S)       Mon Nov 28 09:26:16 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no217

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at x
        call    ReadInt                 ; read int; value placed in eax
        mov     [I2],eax                ; store eax at a
        call    ReadInt                 ; read int; value placed in eax
        mov     [I1],eax                ; store eax at y
        call    ReadInt                 ; read int; value placed in eax
        mov     [I3],eax                ; store eax at b
        mov     eax,[I6]                ; AReg = 0
        mov     [I4],eax                ; c = AReg
        mov     eax,[I0]                ; AReg = x
        cmp     eax,[I2]                ; compare x and a
        jg      .L0                     ; if x > a then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L2                     ; if T0 is false then jump to end of if
        mov     eax,[I1]                ; AReg = y
        cmp     eax,[I3]                ; compare y and b
        jl      .L3                     ; if y < b then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L4                     ; unconditionally jump
.L3:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L4:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L5                     ; if T0 is false then jump to end of if
        mov     eax,[I2]                ; AReg = a
        add     eax,[I3]                ; AReg = a + b
        mov     [I4],eax                ; c = AReg
        mov     eax,[I1]                ; AReg = y
        add     eax,[I7]                ; AReg = y + 2
        mov     [I5],eax                ; d = AReg
.L5:                                    ; end if
        jmp     .L6                     ; jump to end if
.L2:                                    ; else
        mov     eax,[I8]                ; AReg = 1
        mov     [I4],eax                ; c = AReg
.L6:                                    ; end if
        mov     eax,[I1]                ; AReg = y
        add     eax,[I9]                ; AReg = y + 3
        mov     [I5],eax                ; d = AReg
        mov     eax,[I4]                ; load c in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I5]                ; load d in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I6      dd      0                       ; 0
I8      dd      1                       ; 1
I7      dd      2                       ; 2
I9      dd      3                       ; 3
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
I2      resd    1                       ; a
I3      resd    1                       ; b
I4      resd    1                       ; c
I5      resd    1                       ; d
I0      resd    1                       ; x
I1      resd    1                       ; y
