; YOUR NAME(S)       Mon Nov 28 09:26:17 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no230

_start:                                 
.L0:                                    ; while
        mov     eax,[TRUE]              ; AReg = true
        cmp     eax,0                   ; compare eax to 0
        je      .L1                     ; if true is false then jump to end while
        jmp     .L0                     ; end while
.L1:                                    
        mov     eax,[I0]                ; load month in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I0      dd      12                      ; month
TRUE    dd      -1                      ; true

SECTION .bss                            
