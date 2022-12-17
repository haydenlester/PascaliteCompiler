; YOUR NAME(S)       Mon Nov 28 09:26:16 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no212

_start:                                 
        mov     eax,[B0]                ; AReg = why
        cmp     eax,0                   ; compare eax to 0
        je      .L0                     ; if why is false then jump to end of if
        mov     eax,[I3]                ; AReg = 0
        mov     [I2],eax                ; z = AReg
        jmp     .L1                     ; jump to end if
.L0:                                    ; else
        mov     eax,[I4]                ; AReg = -3
        mov     [I2],eax                ; z = AReg
.L1:                                    ; end if
