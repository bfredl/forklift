bits 64
global main
    section   .text
main:

    mov rax, rdi
    mov eax, edi
    mov ax, di
    mov al, dil
    mov al, bh

    mov rax, rbx
    mov eax, ebx
    mov ax, bx
    mov al, bl

    push rax
    push r15
    pop rax
    pop r15
