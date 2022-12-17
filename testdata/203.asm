; YOUR NAME(S)       Mon Nov 28 09:26:16 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no203

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at x
        call    ReadInt                 ; read int; value placed in eax
        mov     [I1],eax                ; store eax at y
        mov     eax,[I0]                ; AReg = x
        cmp     eax,[I1]                ; compare x and y
        jl      .L0                     ; if x < y then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L2                     ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = x
        add     eax,[I3]                ; AReg = x + 3
        mov     [I2],eax                ; a = AReg
        jmp     .L3                     ; jump to end if
.L2:                                    ; else
        mov     eax,[I1]                ; AReg = y
        add     eax,[I4]                ; AReg = y + 5
        mov     [I2],eax                ; a = AReg
.L3:                                    ; end if
        mov     eax,[I0]                ; load x in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I1]                ; load y in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I2]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I3      dd      3                       ; 3
I4      dd      5                       ; 5
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
I2      resd    1                       ; a
I0      resd    1                       ; x
I1      resd    1                       ; y
