bits 64
global main
    section   .text
main:

mov rax, [rsi]
movsd xmm1, [rsi]
movsd xmm1, [rsi]

vmovsd xmm1, [rax]
vmovsd xmm1, [rcx]
vmovsd xmm1, [rsi]
vmovsd xmm9, [rsi]

movsd xmm0, xmm1
vmovsd xmm0, xmm1, xmm2
vmovpd xmm0, xmm1

vaddsd xmm1, xmm1, xmm2

vaddsd xmm1, xmm9, xmm1
vaddsd xmm9, xmm1, xmm1
vaddsd xmm1, xmm1, xmm9
