; YOUR NAME(S)       Mon Nov 28 09:26:17 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no226

_start:                                 
        mov     eax,[I1]                ; AReg = 0
        mov     [I0],eax                ; a = AReg
        cmp     eax,[I2]                ; compare a and 10
        jl      .L0                     ; if a < 10 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        mov     [B0],eax                ; f = AReg
.L2:                                    ; while
        mov     eax,[B0]                ; AReg = f
        cmp     eax,0                   ; compare eax to 0
        je      .L3                     ; if f is false then jump to end while
        mov     eax,[I0]                ; AReg = a
        add     eax,[I3]                ; AReg = a + 1
        mov     [I0],eax                ; a = AReg
        cmp     eax,[I2]                ; compare a and 10
        jl      .L4                     ; if a < 10 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L5                     ; unconditionally jump
.L4:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L5:                                    
        mov     [B0],eax                ; f = AReg
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        jmp     .L2                     ; end while
.L3:                                    
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[B0]                ; load f in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I1      dd      0                       ; 0
I3      dd      1                       ; 1
I2      dd      10                      ; 10
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
I0      resd    1                       ; a
B0      resd    1                       ; f
