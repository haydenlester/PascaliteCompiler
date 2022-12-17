; YOUR NAME(S)       Mon Nov 28 09:26:16 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no218

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at a
        call    ReadInt                 ; read int; value placed in eax
        mov     [I1],eax                ; store eax at b
        call    ReadInt                 ; read int; value placed in eax
        mov     [I2],eax                ; store eax at c
        mov     eax,[I0]                ; AReg = a
        cmp     eax,[I1]                ; compare a and b
        jl      .L0                     ; if a < b then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L2                     ; if T0 is false then jump to end of if
        mov     eax,[I1]                ; AReg = b
        cmp     eax,[I2]                ; compare b and c
        jl      .L3                     ; if b < c then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L4                     ; unconditionally jump
.L3:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L4:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L5                     ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I1]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I2]                ; load c in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        jmp     .L6                     ; jump to end if
.L5:                                    ; else
        mov     eax,[I0]                ; AReg = a
        cmp     eax,[I2]                ; compare a and c
        jl      .L7                     ; if a < c then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L8                     ; unconditionally jump
.L7:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L8:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L9                     ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I2]                ; load c in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I1]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        jmp     .L10                    ; jump to end if
.L9:                                    ; else
        mov     eax,[I2]                ; load c in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I1]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
.L10:                                   ; end if
.L6:                                    ; end if
        jmp     .L11                    ; jump to end if
.L2:                                    ; else
        mov     eax,[I0]                ; AReg = a
        cmp     eax,[I2]                ; compare a and c
        jl      .L12                    ; if a < c then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L13                    ; unconditionally jump
.L12:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L13:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L14                    ; if T0 is false then jump to end of if
        mov     eax,[I1]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I2]                ; load c in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        jmp     .L15                    ; jump to end if
.L14:                                   ; else
        mov     eax,[I1]                ; AReg = b
        cmp     eax,[I2]                ; compare b and c
        jl      .L16                    ; if b < c then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L17                    ; unconditionally jump
.L16:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L17:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L18                    ; if T0 is false then jump to end of if
        mov     eax,[I1]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I2]                ; load c in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        jmp     .L19                    ; jump to end if
.L18:                                   ; else
        mov     eax,[I2]                ; load c in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I1]                ; load b in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        mov     eax,[I0]                ; load a in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
.L19:                                   ; end if
.L15:                                   ; end if
.L11:                                   ; end if
        Exit    {0}                     

SECTION .data                           
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
I0      resd    1                       ; a
I1      resd    1                       ; b
I2      resd    1                       ; c
