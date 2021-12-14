bits 64
global main
    section   .text
main:

mov rax, [rbx]
mov rax, [rsp]
mov rax, [rbp]
mov rax, [rdi+2*rbp]


mov rax, [rbx-8]
mov rax, [rsp-8]
mov rax, [rbp-8]

mov rax, [32]
; special encoding for rax only!
mov rax, [qword 0x2233445566778899]
;mov rax, [0x1024+rip]
