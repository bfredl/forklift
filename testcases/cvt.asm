bits 64
global main
    section   .text
main:
  vcvtsi2sd xmm0,xmm0,rsi
  vcvtsi2sd xmm0,xmm0,esi
