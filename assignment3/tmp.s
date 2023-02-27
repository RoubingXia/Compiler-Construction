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
	la	$24, prefix_x
	sw	$2, 0($24)
	li	$2, 0x4
	la	$22, prefix_y
	sw	$2, 0($22)
	la	$2, prefix_x
	lw	$2, 0($2)
	addi	$25, $2, 0x0
	la	$2, prefix_y
	lw	$2, 0($2)
	addi	$6, $2, 0x0
	add	$2, $25, $6
	addi	$2, $2, 0x0
	jr	$31


	.data
	.align 0
prefix_x:	.word 0
prefix_y:	.word 0

