; Hayden Lester      Sun Dec  4 14:38:41 2022
%INCLUDE "Along32.inc"
%INCLUDE "Macros_Along.inc"

SECTION .text                           
global  _start                          ; program stage2no237

_start:                                 
        call    ReadInt                 ; read int; value placed in eax
        mov     [I0],eax                ; store eax at var000
        call    ReadInt                 ; read int; value placed in eax
        mov     [I1],eax                ; store eax at var001
        call    ReadInt                 ; read int; value placed in eax
        mov     [I2],eax                ; store eax at var002
        call    ReadInt                 ; read int; value placed in eax
        mov     [I3],eax                ; store eax at var003
        call    ReadInt                 ; read int; value placed in eax
        mov     [I4],eax                ; store eax at var004
        call    ReadInt                 ; read int; value placed in eax
        mov     [I5],eax                ; store eax at var005
        call    ReadInt                 ; read int; value placed in eax
        mov     [I6],eax                ; store eax at var006
        call    ReadInt                 ; read int; value placed in eax
        mov     [I7],eax                ; store eax at var007
        call    ReadInt                 ; read int; value placed in eax
        mov     [I8],eax                ; store eax at var008
        call    ReadInt                 ; read int; value placed in eax
        mov     [I9],eax                ; store eax at var009
        call    ReadInt                 ; read int; value placed in eax
        mov     [I10],eax               ; store eax at var010
        call    ReadInt                 ; read int; value placed in eax
        mov     [I11],eax               ; store eax at var011
        call    ReadInt                 ; read int; value placed in eax
        mov     [I12],eax               ; store eax at var012
        call    ReadInt                 ; read int; value placed in eax
        mov     [I13],eax               ; store eax at var013
        call    ReadInt                 ; read int; value placed in eax
        mov     [I14],eax               ; store eax at var014
        call    ReadInt                 ; read int; value placed in eax
        mov     [I15],eax               ; store eax at var015
        call    ReadInt                 ; read int; value placed in eax
        mov     [I16],eax               ; store eax at var016
        call    ReadInt                 ; read int; value placed in eax
        mov     [I17],eax               ; store eax at var017
        call    ReadInt                 ; read int; value placed in eax
        mov     [I18],eax               ; store eax at var018
        call    ReadInt                 ; read int; value placed in eax
        mov     [I19],eax               ; store eax at var019
        call    ReadInt                 ; read int; value placed in eax
        mov     [I20],eax               ; store eax at var020
        call    ReadInt                 ; read int; value placed in eax
        mov     [I21],eax               ; store eax at var021
        call    ReadInt                 ; read int; value placed in eax
        mov     [I22],eax               ; store eax at var022
        call    ReadInt                 ; read int; value placed in eax
        mov     [I23],eax               ; store eax at var023
        call    ReadInt                 ; read int; value placed in eax
        mov     [I24],eax               ; store eax at var024
        call    ReadInt                 ; read int; value placed in eax
        mov     [I25],eax               ; store eax at var025
        call    ReadInt                 ; read int; value placed in eax
        mov     [I26],eax               ; store eax at var026
        call    ReadInt                 ; read int; value placed in eax
        mov     [I27],eax               ; store eax at var027
        call    ReadInt                 ; read int; value placed in eax
        mov     [I28],eax               ; store eax at var028
        call    ReadInt                 ; read int; value placed in eax
        mov     [I29],eax               ; store eax at var029
        call    ReadInt                 ; read int; value placed in eax
        mov     [I30],eax               ; store eax at var030
        call    ReadInt                 ; read int; value placed in eax
        mov     [I31],eax               ; store eax at var031
        call    ReadInt                 ; read int; value placed in eax
        mov     [I32],eax               ; store eax at var032
        call    ReadInt                 ; read int; value placed in eax
        mov     [I33],eax               ; store eax at var033
        call    ReadInt                 ; read int; value placed in eax
        mov     [I34],eax               ; store eax at var034
        call    ReadInt                 ; read int; value placed in eax
        mov     [I35],eax               ; store eax at var035
        call    ReadInt                 ; read int; value placed in eax
        mov     [I36],eax               ; store eax at var036
        call    ReadInt                 ; read int; value placed in eax
        mov     [I37],eax               ; store eax at var037
        call    ReadInt                 ; read int; value placed in eax
        mov     [I38],eax               ; store eax at var038
        call    ReadInt                 ; read int; value placed in eax
        mov     [I39],eax               ; store eax at var039
        call    ReadInt                 ; read int; value placed in eax
        mov     [I40],eax               ; store eax at var040
        call    ReadInt                 ; read int; value placed in eax
        mov     [I41],eax               ; store eax at var041
        call    ReadInt                 ; read int; value placed in eax
        mov     [I42],eax               ; store eax at var042
        call    ReadInt                 ; read int; value placed in eax
        mov     [I43],eax               ; store eax at var043
        call    ReadInt                 ; read int; value placed in eax
        mov     [I44],eax               ; store eax at var044
        call    ReadInt                 ; read int; value placed in eax
        mov     [I45],eax               ; store eax at var045
        call    ReadInt                 ; read int; value placed in eax
        mov     [I46],eax               ; store eax at var046
        call    ReadInt                 ; read int; value placed in eax
        mov     [I47],eax               ; store eax at var047
        call    ReadInt                 ; read int; value placed in eax
        mov     [I48],eax               ; store eax at var048
        call    ReadInt                 ; read int; value placed in eax
        mov     [I49],eax               ; store eax at var049
        call    ReadInt                 ; read int; value placed in eax
        mov     [I50],eax               ; store eax at var050
        call    ReadInt                 ; read int; value placed in eax
        mov     [I51],eax               ; store eax at var051
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I1]                ; compare var000 and var001
        jl      .L0                     ; if var000 < var001 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L1                     ; unconditionally jump
.L0:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L1:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L2                     ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I2]                ; compare var000 and var002
        jl      .L3                     ; if var000 < var002 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L4                     ; unconditionally jump
.L3:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L4:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L5                     ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I3]                ; compare var000 and var003
        jl      .L6                     ; if var000 < var003 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L7                     ; unconditionally jump
.L6:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L7:                                    
        cmp     eax,0                   ; compare eax to 0
        je      .L8                     ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I4]                ; compare var000 and var004
        jl      .L9                     ; if var000 < var004 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L10                    ; unconditionally jump
.L9:                                    
        mov     eax,[TRUE]              ; set eax to TRUE
.L10:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L11                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I5]                ; compare var000 and var005
        jl      .L12                    ; if var000 < var005 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L13                    ; unconditionally jump
.L12:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L13:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L14                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I6]                ; compare var000 and var006
        jl      .L15                    ; if var000 < var006 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L16                    ; unconditionally jump
.L15:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L16:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L17                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I7]                ; compare var000 and var007
        jl      .L18                    ; if var000 < var007 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L19                    ; unconditionally jump
.L18:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L19:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L20                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I8]                ; compare var000 and var008
        jl      .L21                    ; if var000 < var008 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L22                    ; unconditionally jump
.L21:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L22:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L23                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I9]                ; compare var000 and var009
        jl      .L24                    ; if var000 < var009 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L25                    ; unconditionally jump
.L24:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L25:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L26                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I10]               ; compare var000 and var010
        jl      .L27                    ; if var000 < var010 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L28                    ; unconditionally jump
.L27:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L28:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L29                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I11]               ; compare var000 and var011
        jl      .L30                    ; if var000 < var011 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L31                    ; unconditionally jump
.L30:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L31:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L32                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I12]               ; compare var000 and var012
        jl      .L33                    ; if var000 < var012 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L34                    ; unconditionally jump
.L33:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L34:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L35                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I13]               ; compare var000 and var013
        jl      .L36                    ; if var000 < var013 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L37                    ; unconditionally jump
.L36:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L37:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L38                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I14]               ; compare var000 and var014
        jl      .L39                    ; if var000 < var014 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L40                    ; unconditionally jump
.L39:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L40:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L41                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I15]               ; compare var000 and var015
        jl      .L42                    ; if var000 < var015 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L43                    ; unconditionally jump
.L42:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L43:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L44                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I16]               ; compare var000 and var016
        jl      .L45                    ; if var000 < var016 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L46                    ; unconditionally jump
.L45:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L46:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L47                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I17]               ; compare var000 and var017
        jl      .L48                    ; if var000 < var017 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L49                    ; unconditionally jump
.L48:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L49:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L50                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I18]               ; compare var000 and var018
        jl      .L51                    ; if var000 < var018 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L52                    ; unconditionally jump
.L51:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L52:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L53                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I19]               ; compare var000 and var019
        jl      .L54                    ; if var000 < var019 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L55                    ; unconditionally jump
.L54:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L55:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L56                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I20]               ; compare var000 and var020
        jl      .L57                    ; if var000 < var020 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L58                    ; unconditionally jump
.L57:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L58:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L59                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I21]               ; compare var000 and var021
        jl      .L60                    ; if var000 < var021 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L61                    ; unconditionally jump
.L60:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L61:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L62                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I22]               ; compare var000 and var022
        jl      .L63                    ; if var000 < var022 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L64                    ; unconditionally jump
.L63:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L64:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L65                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I23]               ; compare var000 and var023
        jl      .L66                    ; if var000 < var023 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L67                    ; unconditionally jump
.L66:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L67:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L68                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I24]               ; compare var000 and var024
        jl      .L69                    ; if var000 < var024 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L70                    ; unconditionally jump
.L69:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L70:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L71                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I25]               ; compare var000 and var025
        jl      .L72                    ; if var000 < var025 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L73                    ; unconditionally jump
.L72:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L73:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L74                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I26]               ; compare var000 and var026
        jl      .L75                    ; if var000 < var026 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L76                    ; unconditionally jump
.L75:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L76:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L77                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I27]               ; compare var000 and var027
        jl      .L78                    ; if var000 < var027 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L79                    ; unconditionally jump
.L78:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L79:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L80                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I28]               ; compare var000 and var028
        jl      .L81                    ; if var000 < var028 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L82                    ; unconditionally jump
.L81:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L82:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L83                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I29]               ; compare var000 and var029
        jl      .L84                    ; if var000 < var029 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L85                    ; unconditionally jump
.L84:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L85:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L86                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I30]               ; compare var000 and var030
        jl      .L87                    ; if var000 < var030 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L88                    ; unconditionally jump
.L87:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L88:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L89                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I31]               ; compare var000 and var031
        jl      .L90                    ; if var000 < var031 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L91                    ; unconditionally jump
.L90:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L91:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L92                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I32]               ; compare var000 and var032
        jl      .L93                    ; if var000 < var032 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L94                    ; unconditionally jump
.L93:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L94:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L95                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I33]               ; compare var000 and var033
        jl      .L96                    ; if var000 < var033 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L97                    ; unconditionally jump
.L96:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L97:                                   
        cmp     eax,0                   ; compare eax to 0
        je      .L98                    ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I34]               ; compare var000 and var034
        jl      .L99                    ; if var000 < var034 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L100                   ; unconditionally jump
.L99:                                   
        mov     eax,[TRUE]              ; set eax to TRUE
.L100:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L101                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I35]               ; compare var000 and var035
        jl      .L102                   ; if var000 < var035 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L103                   ; unconditionally jump
.L102:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L103:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L104                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I36]               ; compare var000 and var036
        jl      .L105                   ; if var000 < var036 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L106                   ; unconditionally jump
.L105:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L106:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L107                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I37]               ; compare var000 and var037
        jl      .L108                   ; if var000 < var037 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L109                   ; unconditionally jump
.L108:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L109:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L110                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I38]               ; compare var000 and var038
        jl      .L111                   ; if var000 < var038 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L112                   ; unconditionally jump
.L111:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L112:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L113                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I39]               ; compare var000 and var039
        jl      .L114                   ; if var000 < var039 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L115                   ; unconditionally jump
.L114:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L115:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L116                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I40]               ; compare var000 and var040
        jl      .L117                   ; if var000 < var040 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L118                   ; unconditionally jump
.L117:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L118:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L119                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I41]               ; compare var000 and var041
        jl      .L120                   ; if var000 < var041 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L121                   ; unconditionally jump
.L120:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L121:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L122                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I42]               ; compare var000 and var042
        jl      .L123                   ; if var000 < var042 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L124                   ; unconditionally jump
.L123:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L124:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L125                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I43]               ; compare var000 and var043
        jl      .L126                   ; if var000 < var043 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L127                   ; unconditionally jump
.L126:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L127:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L128                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I44]               ; compare var000 and var044
        jl      .L129                   ; if var000 < var044 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L130                   ; unconditionally jump
.L129:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L130:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L131                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I45]               ; compare var000 and var045
        jl      .L132                   ; if var000 < var045 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L133                   ; unconditionally jump
.L132:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L133:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L134                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I46]               ; compare var000 and var046
        jl      .L135                   ; if var000 < var046 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L136                   ; unconditionally jump
.L135:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L136:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L137                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I47]               ; compare var000 and var047
        jl      .L138                   ; if var000 < var047 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L139                   ; unconditionally jump
.L138:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L139:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L140                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I48]               ; compare var000 and var048
        jl      .L141                   ; if var000 < var048 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L142                   ; unconditionally jump
.L141:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L142:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L143                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I49]               ; compare var000 and var049
        jl      .L144                   ; if var000 < var049 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L145                   ; unconditionally jump
.L144:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L145:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L146                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I50]               ; compare var000 and var050
        jl      .L147                   ; if var000 < var050 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L148                   ; unconditionally jump
.L147:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L148:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L149                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        cmp     eax,[I51]               ; compare var000 and var051
        jl      .L150                   ; if var000 < var051 then jump to set eax to TRUE
        mov     eax,[FALSE]             ; else set eax to FALSE
        jmp     .L151                   ; unconditionally jump
.L150:                                  
        mov     eax,[TRUE]              ; set eax to TRUE
.L151:                                  
        cmp     eax,0                   ; compare eax to 0
        je      .L152                   ; if T0 is false then jump to end of if
        mov     eax,[I0]                ; AReg = var000
        mov     [I52],eax               ; small = AReg
.L152:                                  ; end if
.L149:                                  ; end if
.L146:                                  ; end if
.L143:                                  ; end if
.L140:                                  ; end if
.L137:                                  ; end if
.L134:                                  ; end if
.L131:                                  ; end if
.L128:                                  ; end if
.L125:                                  ; end if
.L122:                                  ; end if
.L119:                                  ; end if
.L116:                                  ; end if
.L113:                                  ; end if
.L110:                                  ; end if
.L107:                                  ; end if
.L104:                                  ; end if
.L101:                                  ; end if
.L98:                                   ; end if
.L95:                                   ; end if
.L92:                                   ; end if
.L89:                                   ; end if
.L86:                                   ; end if
.L83:                                   ; end if
.L80:                                   ; end if
.L77:                                   ; end if
.L74:                                   ; end if
.L71:                                   ; end if
.L68:                                   ; end if
.L65:                                   ; end if
.L62:                                   ; end if
.L59:                                   ; end if
.L56:                                   ; end if
.L53:                                   ; end if
.L50:                                   ; end if
.L47:                                   ; end if
.L44:                                   ; end if
.L41:                                   ; end if
.L38:                                   ; end if
.L35:                                   ; end if
.L32:                                   ; end if
.L29:                                   ; end if
.L26:                                   ; end if
.L23:                                   ; end if
.L20:                                   ; end if
.L17:                                   ; end if
.L14:                                   ; end if
.L11:                                   ; end if
.L8:                                    ; end if
.L5:                                    ; end if
.L2:                                    ; end if
        mov     eax,[I52]               ; load small in eax
        call    WriteInt                ; write int in eax to standard out
        call    Crlf                    ; write \r\n to standard out
        Exit    {0}                     

SECTION .data                           
FALSE   dd      0                       ; false
TRUE    dd      -1                      ; true

SECTION .bss                            
I52     resd    1                       ; small
I0      resd    1                       ; var000
I1      resd    1                       ; var001
I2      resd    1                       ; var002
I3      resd    1                       ; var003
I4      resd    1                       ; var004
I5      resd    1                       ; var005
I6      resd    1                       ; var006
I7      resd    1                       ; var007
I8      resd    1                       ; var008
I9      resd    1                       ; var009
I10     resd    1                       ; var010
I11     resd    1                       ; var011
I12     resd    1                       ; var012
I13     resd    1                       ; var013
I14     resd    1                       ; var014
I15     resd    1                       ; var015
I16     resd    1                       ; var016
I17     resd    1                       ; var017
I18     resd    1                       ; var018
I19     resd    1                       ; var019
I20     resd    1                       ; var020
I21     resd    1                       ; var021
I22     resd    1                       ; var022
I23     resd    1                       ; var023
I24     resd    1                       ; var024
I25     resd    1                       ; var025
I26     resd    1                       ; var026
I27     resd    1                       ; var027
I28     resd    1                       ; var028
I29     resd    1                       ; var029
I30     resd    1                       ; var030
I31     resd    1                       ; var031
I32     resd    1                       ; var032
I33     resd    1                       ; var033
I34     resd    1                       ; var034
I35     resd    1                       ; var035
I36     resd    1                       ; var036
I37     resd    1                       ; var037
I38     resd    1                       ; var038
I39     resd    1                       ; var039
I40     resd    1                       ; var040
I41     resd    1                       ; var041
I42     resd    1                       ; var042
I43     resd    1                       ; var043
I44     resd    1                       ; var044
I45     resd    1                       ; var045
I46     resd    1                       ; var046
I47     resd    1                       ; var047
I48     resd    1                       ; var048
I49     resd    1                       ; var049
I50     resd    1                       ; var050
I51     resd    1                       ; var051
