; YOUR NAME(S)       Mon Nov 28 09:26:16 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no206

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I1],eax                ; store eax at a
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at x
        mov     eax,[I3]                ; AReg = 3
        mov     [I2],eax                ; b = AReg
.L0:                                    ; repeat
        mov     eax,[I1]                ; AReg = a
        add     eax,[I2]                ; AReg = a + b
        mov     [I2],eax                ; b = AReg
        cmp     eax,[I0]                ; compare b and x
        jg      .L1                     ; if b > x then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L2                     ; unconditionally jump
.L1:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L2:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L0                     ; until T0 is true
        mov     eax,[I2]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I3      dd      3                       ; 3
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
I1      resd    1                       ; a
I2      resd    1                       ; b
I0      resd    1                       ; x
