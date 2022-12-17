; YOUR NAME(S)       Mon Nov 28 09:26:17 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no225

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at a
        call    ReadInt                 ; read int; value placed in eax
        mov     [I1],eax                ; store eax at b
        mov     eax,[I0]                ; AReg = a
        cmp     eax,[I3]                ; compare a and 0
        jl      .L0                     ; if a < 0 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L2                     ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = a
        imul    dword [I4]              ; AReg = a * -1
        mov     [I0],eax                ; a = AReg
.L2:                                    ; end if
        mov     eax,[I1]                ; AReg = b
        cmp     eax,[I3]                ; compare b and 0
        jl      .L3                     ; if b < 0 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L4                     ; unconditionally jump
.L3:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L4:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L5                     ; if T0 is false then jump to end of if
        mov     eax,[I1]                ; AReg = b
        imul    dword [I4]              ; AReg = b * -1
        mov     [I1],eax                ; b = AReg
.L5:                                    ; end if
        mov     eax,[I0]                ; AReg = a
        cmp     eax,[I1]                ; compare a and b
        jl      .L6                     ; if a < b then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L7                     ; unconditionally jump
.L6:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L7:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L8                     ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = a
        mov     [I2],eax                ; gcd = AReg
        jmp     .L9                     ; jump to end if
.L8:                                    ; else
        mov     eax,[I1]                ; AReg = b
        mov     [I2],eax                ; gcd = AReg
.L9:                                    ; end if
.L10:                                   ; while
        mov     eax,[I0]                ; AReg = a
        cdq                             ; sign extend dividend from eax to edx:eax
        idiv    dword [I2]              ; AReg = a div gcd
        xchg    eax,edx                 ; exchange quotient and remainder
        cmp     eax,[I3]                ; compare T0 and 0
        jne     .L11                    ; if T0 <> 0 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L12                    ; unconditionally jump
.L11:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L12:                                   
        mov     [T0],eax                ; deassign AReg
        mov     eax,[I1]                ; AReg = b
        cdq                             ; sign extend dividend from eax to edx:eax
        idiv    dword [I2]              ; AReg = b div gcd
        xchg    eax,edx                 ; exchange quotient and remainder
        cmp     eax,[I3]                ; compare T1 and 0
        jne     .L13                    ; if T1 <> 0 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L14                    ; unconditionally jump
.L13:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L14:                                   
        or      eax,[T0]                ; AReg = T1 or T0
        cmp     eax,0                   ; compare eax to 0
        je      .L15                    ; if T0 is false then jump to end while
        mov     eax,[I2]                ; AReg = gcd
        sub     eax,[I5]                ; AReg = gcd - 1
        mov     [I2],eax                ; gcd = AReg
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        jmp     .L10                    ; end while
.L15:                                   
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I1]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I2]                ; load gcd in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
I4      dd      -1                      ; -1
I3      dd      0                       ; 0
I5      dd      1                       ; 1
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
T0      resd    1                       ; T0
I0      resd    1                       ; a
I1      resd    1                       ; b
I2      resd    1                       ; gcd
