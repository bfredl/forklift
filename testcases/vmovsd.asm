bits 64
global main
    section   .text
main:

mov rax, [rsi]
movsd xmm0, [rax]

mov rax, [rax]
movsd xmm0, [rax]
vmovsd xmm0, [rax]

vmovsd xmm0, [rcx]
vmovsd xmm0, [rsi]
vmovsd xmm8, [rsi]

movsd xmm0, xmm1
vmovsd xmm0, xmm1, xmm2
;vmovpd xmm0, xmm1

vaddsd xmm1, xmm1, xmm2

vaddsd xmm1, xmm9, xmm1
vaddsd xmm9, xmm1, xmm1
vaddsd xmm1, xmm1, xmm9
