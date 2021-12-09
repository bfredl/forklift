global main
    section   .text
main:

mov rax, [rbx]
mov rax, [rsp]
mov rax, [rbp]


mov rax, [rbx-8]
mov rax, [rsp-8]
mov rax, [rbp-8]
