; YOUR NAME(S)       Mon Nov 28 09:26:16 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no204

_start:                                 
        mov     eax,[I7]                ; AReg = -1
        mov     [I4],eax                ; a = AReg
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at w
        call    ReadInt                 ; read int; value placed in eax
        mov     [I1],eax                ; store eax at x
        call    ReadInt                 ; read int; value placed in eax
        mov     [I2],eax                ; store eax at y
        call    ReadInt                 ; read int; value placed in eax
        mov     [I3],eax                ; store eax at z
        call    ReadInt                 ; read int; value placed in eax
        mov     [I5],eax                ; store eax at b
        call    ReadInt                 ; read int; value placed in eax
        mov     [I6],eax                ; store eax at c
        mov     eax,[I1]                ; AReg = x
        cmp     eax,[I2]                ; compare x and y
        jl      .L0                     ; if x < y then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        mov     [T0],eax                ; deassign AReg
        mov     eax,[I0]                ; AReg = w
        cmp     eax,[I3]                ; compare w and z
        je      .L2                     ; if w = z then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L3                     ; unconditionally jump
.L2:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L3:                                    
        and     eax,[T0]                ; AReg = T1 and T0
        cmp     eax,0                   ; compare eax to 0
        je      .L4                     ; if T0 is false then jump to end of if
        mov     eax,[I5]                ; AReg = b
        imul    dword [I6]              ; AReg = b * c
        mov     [I4],eax                ; a = AReg
.L4:                                    ; end if
        mov     eax,[I4]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I7      dd      -1                      ; -1
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
T0      resd    1                       ; T0
I4      resd    1                       ; a
I5      resd    1                       ; b
I6      resd    1                       ; c
I0      resd    1                       ; w
I1      resd    1                       ; x
I2      resd    1                       ; y
I3      resd    1                       ; z
