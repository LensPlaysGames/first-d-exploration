.section .text

.global main

main:
    sub $40, %rsp
    push $0
    mov $16, %rax
    pop %rax
    add $40, %rsp
    ret
