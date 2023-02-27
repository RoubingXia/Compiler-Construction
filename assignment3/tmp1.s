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
	li	$2, 0x3E8
	la	$24, prefix_limit
	sw	$2, 0($24)
	li	$2, 0x0
	la	$22, prefix_x
	sw	$2, 0($22)
	li	$2, 0x1
	la	$25, prefix_y
	sw	$2, 0($25)
L1:
	la	$2, prefix_y
	lw	$2, 0($2)
	addi	$6, $2, 0x0
	la	$2, prefix_limit
	lw	$2, 0($2)
	addi	$21, $2, 0x0
	slt	$2, $6, $21
	blez	$2, L2
	la	$2, prefix_x
	lw	$2, 0($2)
	addi	$17, $2, 0x0
	la	$2, prefix_y
	lw	$2, 0($2)
	addi	$23, $2, 0x0
	add	$2, $17, $23
	la	$15, prefix_t
	sw	$2, 0($15)
	la	$2, prefix_y
	lw	$2, 0($2)
	la	$4, prefix_x
	sw	$2, 0($4)
	la	$2, prefix_t
	lw	$2, 0($2)
	la	$18, prefix_y
	sw	$2, 0($18)
	j L1
L2:
	la	$2, prefix_y
	lw	$2, 0($2)
	addi	$2, $2, 0x0
	jr	$31


	.data
	.align 0
prefix_limit:	.word 0
prefix_t:	.word 0
prefix_x:	.word 0
prefix_y:	.word 0

