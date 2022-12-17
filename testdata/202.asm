; YOUR NAME(S)       Mon Nov 28 09:26:16 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no202

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at y
.L0:                                    ; while
        mov     eax,[I0]                ; AReg = y
        cmp     eax,[I2]                ; compare y and 10
        jl      .L1                     ; if y < 10 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L2                     ; unconditionally jump
.L1:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L2:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L3                     ; if T0 is false then jump to end while
        mov     eax,[I0]                ; AReg = y
        add     eax,[I3]                ; AReg = y + 1
        mov     [I0],eax                ; y = AReg
        jmp     .L0                     ; end while
.L3:                                    
        mov     eax,[I0]                ; AReg = y
        add     eax,[I3]                ; AReg = y + 1
        mov     [I1],eax                ; z = AReg
        mov     eax,[I0]                ; load y in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I1]                ; load z in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I3      dd      1                       ; 1
I2      dd      10                      ; 10
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
I0      resd    1                       ; y
I1      resd    1                       ; z
