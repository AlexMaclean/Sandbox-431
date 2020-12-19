section .text
extern read_int
start:
    call read_int
    mov [rbp-8], rax
    mov rax, [rbp-8]
    neg rax
    mov [rbp-16], rax
    mov rax, 100
    add rax, [rbp-16]
    mov [rbp-24], rax
    mov rax, [rbp-24]
    add rax, [rbp-24]
    mov [rbp-32], rax
    mov rax, [rbp-32]
    add rax, [rbp-24]
    jmp conclusion

global blungentle_main 
blungentle_main:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    jmp start
conclusion:
    add rsp, 32
    pop rbp
    ret
