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

vaddsd xmm1, xmm1, xmm2
vaddsd xmm1, xmm2, xmm2
vaddsd xmm1, xmm2, xmm3
vaddsd xmm2, xmm2, xmm3
vaddsd xmm2, xmm3, xmm3
