; YOUR NAME(S)       Mon Nov 28 09:26:17 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no224

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at a
        call    ReadInt                 ; read int; value placed in eax
        mov     [I1],eax                ; store eax at b
        mov     eax,[I0]                ; AReg = a
        cmp     eax,[I1]                ; compare a and b
        jl      .L0                     ; if a < b then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        mov     [B0],eax                ; f = AReg
        mov     eax,[I3]                ; AReg = 0
        mov     [I2],eax                ; c = AReg
        mov     eax,[B0]                ; AReg = f
        cmp     eax,0                   ; compare eax to 0
        je      .L2                     ; if f is false then jump to end of if
        mov     eax,[I0]                ; AReg = a
        mov     [I2],eax                ; c = AReg
        jmp     .L3                     ; jump to end if
.L2:                                    ; else
        mov     eax,[I1]                ; AReg = b
        mov     [I2],eax                ; c = AReg
.L3:                                    ; end if
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I1]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I2]                ; load c in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I3      dd      0                       ; 0
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
I0      resd    1                       ; a
I1      resd    1                       ; b
I2      resd    1                       ; c
B0      resd    1                       ; f
