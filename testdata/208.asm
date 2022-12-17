; YOUR NAME(S)       Mon Nov 28 09:26:16 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no208

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I2],eax                ; store eax at x
        call    ReadInt                 ; read int; value placed in eax
        mov     [I3],eax                ; store eax at y
        mov     eax,[I2]                ; AReg = x
        cmp     eax,[I4]                ; compare x and 0
        jg      .L0                     ; if x > 0 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        mov     [T0],eax                ; deassign AReg
        mov     eax,[I3]                ; AReg = y
        cmp     eax,[I4]                ; compare y and 0
        jg      .L2                     ; if y > 0 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L3                     ; unconditionally jump
.L2:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L3:                                    
        and     eax,[T0]                ; AReg = T1 and T0
        cmp     eax,0                   ; compare eax to 0
        je      .L4                     ; if T0 is false then jump to end of if
        mov     eax,[I2]                ; AReg = x
        mov     [I0],eax                ; a = AReg
        mov     eax,[I3]                ; AReg = y
        mov     [I1],eax                ; b = AReg
.L5:                                    ; repeat
.L6:                                    ; while
        mov     eax,[I0]                ; AReg = a
        cmp     eax,[I1]                ; compare a and b
        jg      .L7                     ; if a > b then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L8                     ; unconditionally jump
.L7:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L8:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L9                     ; if T0 is false then jump to end while
        mov     eax,[I0]                ; AReg = a
        sub     eax,[I1]                ; AReg = a - b
        mov     [I0],eax                ; a = AReg
        jmp     .L6                     ; end while
.L9:                                    
.L10:                                   ; while
        mov     eax,[I1]                ; AReg = b
        cmp     eax,[I0]                ; compare b and a
        jg      .L11                    ; if b > a then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L12                    ; unconditionally jump
.L11:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L12:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L13                    ; if T0 is false then jump to end while
        mov     eax,[I1]                ; AReg = b
        sub     eax,[I0]                ; AReg = b - a
        mov     [I1],eax                ; b = AReg
        jmp     .L10                    ; end while
.L13:                                   
        mov     eax,[I0]                ; AReg = a
        cmp     eax,[I1]                ; compare a and b
        je      .L14                    ; if a = b then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L15                    ; unconditionally jump
.L14:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L15:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L5                     ; until T0 is true
        mov     eax,[I2]                ; load x in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I3]                ; load y in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
.L4:                                    ; end if
        Exit    {0}                     

SECTION .data                           
I4      dd      0                       ; 0
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
T0      resd    1                       ; T0
I0      resd    1                       ; a
I1      resd    1                       ; b
I2      resd    1                       ; x
I3      resd    1                       ; y
