	.text
	.align	2
	.globl printInt
	.globl fish_main
	.globl main

main:
	move $s8, $31
	jal fish_main
	move $31, $s8
	move $a0, $2
	j printInt

printInt:
	add $t0, $v0, $zero
	li $v0, 1
	syscall
	add $v0, $t0, $zero
	jr $ra

fish_main:
	li	$2, 0xC
	addi	$24, $2, 0x0
	li	$2, 0x4
	addi	$22, $2, 0x0
	add	$2, $24, $22
	addi	$2, $2, 0x0
	jr	$31


	.data
	.align 0

