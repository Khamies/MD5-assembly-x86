 ; All rights reserved. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Authors: Waleed Daud, Soona Sead Ahmed, Ahmed Elmujtaba,Mohammed Ibrahim.
; Date:   May 2017
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This function  outputs 
;					the Padded Message from Preprocessing Proecdure.
;					the final hash value.
; Receives: the message , Length of the message in bytes.
; Returns: Padded message, hash value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 global _main
 extern _printf
 extern _malloc
 extern _free
; ----------------------------------------------- DATA SECTION -----------------------------------------------------------------
 section .data

;----------------------------------------- Preprocessing DATA: section .data  --------------------------------------------------

;this is what you should change.
message: db 'The'
message_size_byte: dd 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End of the changed inputs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
message_size_bit: dd 448
format_default: db '%d ',0
format_hex: db '%04x ',0
format_hex_digest_message: db '%x',0
format_char: db '%c ',0
format_newline: db 10,0
paded_message_length: dd 8

max_block_size: dd 64
CONSTANT: dd 448
;--------------------------------------------------- End of Preprocessing DATA : section .data ---------------------------------

;---------------------------------------------- MD5 Hash DATA : section .data ---------------------------------------------------
 KMD5 :
	dd 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
	dd 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
	dd 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
	dd 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
	dd 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
	dd 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
	dd 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
	dd 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
	dd 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
	dd 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
	dd 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
	dd 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
	dd 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
	dd 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
	dd 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
	dd 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391

 ; this is the constant values used for shifting operation.
 
 S :
	
	dd 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22
	dd 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20
	dd 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23
	dd 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21

 
; these are the initial hash values.
 h1: dd 0x67452301
 h2: dd 0xefcdab89
 h3: dd 0x98badcfe
 h4: dd 0x10325476

 ; Modified Total Rounds
 Rounds: dd 63 ; 63 because a counter will start form 0.
 
 Limit1: dd 15
 Limit2: dd 31
 Limit3: dd 47
 Limit4: dd 63

section .bss
 
;----------------------------------------- Preprocessing DATA: section .bss  --------------------------------------------------
block_counter: resd 1
allocating_space: resd 1
allocating_space_address: resd 1

K: resd 1
;----------------------------------------- End of Preprocessing DATA : section .bss  ---------=--------------------------------

;-----------------------------------------MD5 Hash DATA: section .bss ---------------------------------------------------------
 A: resd 1
 B: resd 1
 C: resd 1
 D: resd 1
 ; these are the varribles used to save the results of the functions used in this code.
 F_result: resd 1
 G_result: resd 1
 H_result: resd 1
 I_result: resd 1
 
 F_g_result: resd 1
 G_g_result: resd 1
 H_g_result: resd 1
 I_g_result: resd 1

 ;to save the result of calculation of function to use it in the main.
 result_f: resd 1
 result_g: resd 1
 ; to save the result of Leftrotate function.
 Leftrotate_result: resd 1
 ;final output.
 Digest_message: resd 1
 
 ; temp vrriables.
 temp_varriable: resd 1
 TIME1: resd 1
 TIME2: resd 1
;----------------------------------------------------- End of MD5 Hash DATA: section .bss ----------------------------------------------------

;-------------------------------------------------- End of DATA SECTION --------------------------------------------------------

;------------------------------------------------- Code SECTION ----------------------------------------------------------------

 section .text

 _main:

; ################################################# PREPROCESSING ##############################################################

; TIME Calculation: begin.
cpuid
rdtsc                       
xor ecx,ecx                ; get current timestamp (saved in a 64 bit value: EDX [first half], EAX [second half])
add ecx,eax   			   ; sets ECX to zero
mov [TIME1],ecx

cpuid
rdtsc                       
xor ecx,ecx                ; get current timestamp (saved in a 64 bit value: EDX [first half], EAX [second half])
add ecx,eax   			   ; sets ECX to zero
mov [TIME1],ecx

cpuid
rdtsc                       
xor ecx,ecx                ; get current timestamp (saved in a 64 bit value: EDX [first half], EAX [second half])
add ecx,eax   			   ; sets ECX to zero
mov [TIME1],ecx

;Begging
mov eax,[message_size_byte]
add eax,[paded_message_length]


add eax,1 ; this is the '1' byte which to be paded to the length of the message.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Allocation Section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Block numbers Calculation.
mov ecx,[max_block_size]
shr eax,6


cmp eax,0
je Block_allocation_1
cmp eax,0
jg Block_allocation_2


Block_allocation_1:
mov ebx,1
mov [block_counter],ebx
jmp allocation_Size

Block_allocation_2:
mov ebx,eax
inc ebx
mov [block_counter],ebx
jmp allocation_Size

; Allocating Process.
allocation_Size:
mov ebx,[max_block_size]
mov eax,[block_counter]
mul ebx
mov [allocating_space],eax


push eax
call _malloc
add esp,4

test eax,eax ; test if malloc faild to allocate memory
jz faild_allocation

mov [allocating_space_address],eax ; save the new address.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Padding Section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Adding the Message to the new Block.
; ebx will have the address of the allocating space reserved by malloc.

mov ebx , [allocating_space_address]
xor ecx,ecx
character_loop:
lea edx,[ebx+ecx]
mov byte  al, [message+ecx]
mov byte [edx],al
inc ecx
cmp ecx,[message_size_byte]
jl character_loop

mov eax , [message_size_byte]
mov byte [ebx+eax],0x80 ; adding the 1 byte.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; adding Zeros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Zeros Calculation.
mov ecx,[message_size_byte]
add ecx,1 ; this is the 8 bits used by the adding 1.
add ecx,8
mov eax, [allocating_space]
sub eax,ecx
add eax,7 ; this is the added zeros with the '1' bit.


; Zeros adding.
mov [K],eax
mov ecx,[message_size_byte]
mov edx,1
lea edi,[ebx+ecx]
zero_loop:
mov byte  al, 0
mov byte [edi+edx],al
inc edx
cmp edx,[K]
jle zero_loop

mov ecx ,[message_size_byte]
add ecx,1
add ecx,[K]


lea edi,[ebx+ecx]
mov esi,[message_size_byte]
mov  [edi] ,esi


   pusha
    mov ebx,[allocating_space_address]
    mov ecx,[allocating_space]
    call ShowMessageInByte
    popa

;;;;;;;;;;;;;;;; new line ;;;;;;;;;;;;;;;;;;

pusha
 push format_newline
 call _printf
 add esp,4
 popa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ############################################## End of PREPROCESSING ##########################################################


; #############################################  MD5 HASH COMPUTING ############################################################
 ; A loop for iterating in blocks.

 xor edi,edi ; a counter for block loop.

  Block:
 ; initialize hash value for this block.
  mov eax,[h1]
  mov [A],eax
  mov eax,[h2]
  mov [B],eax
  mov eax,[h3]
  mov [C],eax
  mov eax,[h4]
  mov [D],eax


 ; A loop inside the block.
  xor esi,esi ; a counter for the current block.
  Inside_Block:

   cmp esi,[Limit1]
   jle Process_1
   

   cmp esi,[Limit2]
   jle Process_2

   cmp esi,[Limit3]
   jle Process_3
  
   cmp esi,[Limit4]
   jle Process_4
 ;these are the procedures for calculation.
   Process_1:
   pusha
   mov eax,[B]
   mov ebx,[C]
   mov ecx,[D]
   mov esi,esi
   call F

   mov eax,[F_result]
   mov [result_f],eax
   mov eax,[F_g_result]
   
   mov [result_g],eax

   popa
   jmp Working_Varriables
  
   Process_2:
   pusha
   mov eax,[B]
   mov ebx,[C]
   mov ecx,[D]
   mov esi,esi
   call G
   mov eax,[G_result]
   mov [result_f],eax
   mov eax,[G_g_result]
   mov [result_g],eax

   popa
   jmp Working_Varriables
  
   Process_3:
   pusha
   mov eax,[B]
   mov ebx,[C]
   mov ecx,[D]
   mov esi,esi
   call H
   mov eax,[H_result]
   mov [result_f],eax
   mov eax,[H_g_result]
   mov [result_g],eax

   popa
   jmp Working_Varriables
  
   Process_4:
     pusha
   mov eax,[B]
   mov ebx,[C]
   mov ecx,[D]
   mov esi,esi
   call I
   mov eax,[I_result]
   mov [result_f],eax
   mov eax,[I_g_result]
   mov [result_g],eax

   popa
   jmp Working_Varriables
 
  ; end of functions space.

  Working_Varriables:
  mov edx,[D]
  mov ecx,[C]
  mov [D],ecx ; update D
  mov ecx,[B]
  mov [C],ecx ; update C
  

  pusha
  ;this is the  calculation of the first element to be pushed to Leftrotate function.
  mov ecx,[A]
  add ecx,[result_f]
  mov edx,[KMD5+esi]
  add ecx,edx
  ; calculate M[g]
  mov eax,edi
  

  mov ebx,64   ; to walk through the blocks.
  mul ebx 


  ; this step for getting the 32 bit number , jumping by 4 byte.
  mov [temp_varriable],eax ; save the value of eax.

; multiplication procedure.
  
  mov eax,[result_g]
  mov ebx,4
  mul ebx
  
  mov [result_g],eax
  
  mov eax,[temp_varriable] ; return the value of eax.
  add eax,[result_g]
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  mov ebx,[allocating_space_address]
  mov edi,[ebx+eax]
  

  ; add A,F,K[i] with M[g].
  add ecx,edi
  
  mov eax,ecx ; the first element to be pushed to the Leftrotate function.
  mov ebx,[S+esi] ; the second element to be pushed to Leftrotate function.
  call Leftrotate
  popa
 
  
  mov ecx,[B]
  add ecx,[Leftrotate_result]
  mov [B],ecx ; update B.
  mov [A],edx  ; update A.
  


  inc esi
  cmp esi,[Rounds]
  jle Inside_Block

  ; update initial hash value.
  mov eax,[h1]
  add eax,[A]   ; update h1.
  mov [h1],eax 

  mov eax,[h2]
  add eax,[B]   ; update h2
  mov [h2],eax
  
 
  mov eax,[h3]
  add eax,[C]   ; update h3
  mov [h3],eax
  
 
  mov eax,[h4]
  add eax,[D]   ; update h4
  mov [h4],eax
  
 
  inc edi 
  cmp edi,[block_counter]
  jl Block
 
  ; now let's reserve a size for final output by malloc.
  mov eax,16 ; this is the 128 bits.
  pusha
  push eax
  call _malloc
  add esp,4
  mov [Digest_message],eax
  test eax,eax ; test if malloc faild to allocate memory
  jz faild_allocation
  popa

  

  mov ebx,[Digest_message] ; save the address of the output in ebx.
 ; outputs will be in a little endian format.
  mov eax,[h4]
  mov dword [ebx],eax
  mov eax,[h3]
  mov dword [ebx+4],eax
  mov eax,[h2]
  mov dword [ebx+8],eax
  mov eax,[h1]
  mov dword [ebx+12],eax

 
 ; ############################################## END OF MD5 HASH COMPUTING ####################################################
 

 
 ; output the Hash.
   pusha
   mov eax,[Digest_message]
   mov ecx,16
   call ShowMessageInByte
   popa
 
 
 ret
; ###################################################### END OF MAIN ###########################################################
; --------------------------------------------------- Malloc test faild -------------------------------------------------------
 faild_allocation:
 ret


;--------------------------------------------- Functions SECTION ----------------------------------------------------------------

;-------------------------------------------------------------------------------------------------------------------------------
; F
; it performs bitwise operation: (B and C) or ((not B) and D).
; Received: a three working varriables B,C,D and g in eax,ebx,ecx,esi respectively.
; Returns : a 32 bit  number.
;-------------------------------------------------------------------------------------------------------------------------------
	F:
	mov edx,eax
	and eax,ebx
	mov edi,eax ; edi for saving result of operation one.
	not edx 
	and edx,ecx
	or edi,edx
	mov [F_result],edi
	mov [F_g_result],esi
	ret	
 
 
;-------------------------------------------------------------------------------------------------------------------------------
; G
; it performs bitwise operation: (D and B) or ((not D) and C).
; Received: a three working varriables B,C,D and g in eax,ebx,ecx,esi respectively.
; Returns : a 32 bit  number.
;-------------------------------------------------------------------------------------------------------------------------------
	G:
	mov edx,ecx ; saving value of d.
	and ecx,eax
	mov edi,ecx ; edi for saving result of operation one.
	not edx ;  inverting value of d.
	and edx,ebx
	or edi,edx
	mov [G_result],edi
	;calculate g
	;performs (5*i+1)modulo 16 .
	;(5*i+1)
	mov eax,esi
	mov ebx,5
	mul ebx
	add eax,1
	;modulo 16 .
	mov edx,0
	mov ebx,16
	div ebx ; result of division ( reminder) will be in edx.
	mov [G_g_result],edx
	ret	
 
	

 
 ;-------------------------------------------------------------------------------------------------------------------------------
; H
; it performs bitwise operation: B xor C xor D.
; Received: a three working varriables B,C,D and g in eax,ebx,ecx,esi respectively.
; Returns : a 32 bit  number.
;-------------------------------------------------------------------------------------------------------------------------------
	H:
	xor eax,ebx
	xor eax,ecx
	mov [H_result],eax
	;calculate g
	;performs (3*i+5)modulo 16 .
	;(3*i+5)
	mov eax,esi
	mov ebx,3
	mul ebx
	add eax,5
	;modulo 16 .
	mov edx,0
	mov ebx,16
	div ebx ; result of division ( reminder) will be in edx.
	mov [H_g_result],edx
	ret	
 


 ;-------------------------------------------------------------------------------------------------------------------------------
; I
; it performs bitwise operation: C xor (B or (not D)).
; Received: a three working varriables B,C,D and g in eax,ebx,ecx,esi respectively.
; Returns : a 32 bit  number.
;-------------------------------------------------------------------------------------------------------------------------------
	I:
	not ecx
	or eax,ecx
	xor ebx,eax
	mov [I_result],ebx
	;calculate g
	;performs (7*i)modulo 16 .
	;(7*i)
	mov eax,esi
	mov ebx,7
	mul ebx
	;modulo 16 .
	mov edx,0
	mov ebx,16
	div ebx ; result of division ( reminder) will be in edx.
	mov [I_g_result],edx
	ret 



;-------------------------------------------------------------------------------------------------------------------------------
;Leftrotate
; it performs: (x << c) binary or (x >> (32-c));
;Received: x (sum),c in eax,ebx respectively.
;Returns : a 32 bit number.
;-------------------------------------------------------------------------------------------------------------------------------
Leftrotate:

mov edi,eax ; save the value of x.
mov ecx,ebx ; I do this code to transfer ebx value to cl through ecx. ecx ==> cx:ch,cl.

shl eax,cl  ; because shl,shr the count must be in cl not any register.
mov edx,32
sub edx,ebx
mov ecx,edx ; I do this step for transfering edx value to cl through ecx.
shr edi,cl 
or eax,edi
mov [Leftrotate_result],eax

ret

; ------------------------------------------------Functions: Test functions ----------------------------------------------------

;-------------------------------------------------------------------------------------------------------------------------------
; ShowMessageInByte
; it outputs the message byte by byte.
; Receives: ebx pointer to the message, ecx number of bytes.
; Returns:  outputs the message in bytes.
;-------------------------------------------------------------------------------------------------------------------------------

; 

 ShowMessageInByte:
 mov edi,0
  L1:
  movzx eax,byte[ebx+edi]
  pusha
  push eax 
  push format_hex
  call _printf
  add esp,8
  popa
  inc edi
  cmp edi ,ecx
  jl L1 
  ret

;-------------------------------------------------------------------------------------------------------------------------------
; ShowMessageInWord
; it outputs the message word by word.
; Receives: ebx pointer to the message, ecx number of bytes.
; Returns:  outputs the message in words.
;-------------------------------------------------------------------------------------------------------------------------------

; 

 ShowMessageInWord:
 mov edi,0
  L2:
  mov eax,[ebx+edi]
  pusha
  push eax 
  push format_hex
  call _printf
  add esp,8
  popa
  add edi,4
  cmp edi ,ecx
  jl L2 
  ret

