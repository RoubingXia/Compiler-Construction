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
	li	$2, 0x2
	la	$24, prefix_x
	sw	$2, 0($24)
	li	$2, 0x3
	la	$22, prefix_y
	sw	$2, 0($22)
	la	$2, prefix_x
	lw	$2, 0($2)
	addi	$14, $2, 0x0
	li	$2, 0x0
	addi	$19, $2, 0x0
	slt	$2, $14, $19
	addi	$28, $2, 0x0
	la	$2, prefix_x
	lw	$2, 0($2)
	addi	$8, $2, 0x0
	li	$2, 0x3
	addi	$26, $2, 0x0
	sgt	$2, $8, $26
	addi	$20, $2, 0x0
	sne	$28, $28, $0
	sne	$20, $20, $0
	or	$2, $28, $20
	addi	$4, $2, 0x0
	la	$2, prefix_y
	lw	$2, 0($2)
	addi	$14, $2, 0x0
	li	$2, 0x0
	addi	$6, $2, 0x0
	slt	$2, $14, $6
	addi	$18, $2, 0x0
	sne	$4, $4, $0
	sne	$18, $18, $0
	or	$2, $4, $18
	addi	$21, $2, 0x0
	la	$2, prefix_y
	lw	$2, 0($2)
	addi	$17, $2, 0x0
	li	$2, 0x3
	addi	$23, $2, 0x0
	sgt	$2, $17, $23
	addi	$15, $2, 0x0
	sne	$21, $21, $0
	sne	$15, $15, $0
	or	$2, $21, $15
	blez	$2, L1
	li	$2, 0x0
	addi	$25, $2, 0x0
	li	$2, 0x1
	addi	$6, $2, 0x0
	sub	$2, $25, $6
	addi	$2, $2, 0x0
	jr	$31
	j L2
L1:
	li	$2, 0x0
L2:
	la	$2, prefix_x
	lw	$2, 0($2)
	addi	$14, $2, 0x0
	li	$2, 0x2
	addi	$15, $2, 0x0
	sge	$2, $14, $15
	blez	$2, L3
	li	$2, 0x1
	la	$16, prefix_x1
	sw	$2, 0($16)
	j L4
L3:
	li	$2, 0x0
	la	$26, prefix_x1
	sw	$2, 0($26)
L4:
	la	$2, prefix_x
	lw	$2, 0($2)
	addi	$7, $2, 0x0
	li	$2, 0x2
	addi	$12, $2, 0x0
	la	$2, prefix_x1
	lw	$2, 0($2)
	addi	$6, $2, 0x0
	mul	$2, $12, $6
	addi	$20, $2, 0x0
	sub	$2, $7, $20
	la	$17, prefix_x0
	sw	$2, 0($17)
	la	$2, prefix_y
	lw	$2, 0($2)
	addi	$12, $2, 0x0
	li	$2, 0x2
	addi	$13, $2, 0x0
	sge	$2, $12, $13
	blez	$2, L5
	li	$2, 0x1
	la	$28, prefix_y1
	sw	$2, 0($28)
	j L6
L5:
	li	$2, 0x0
	la	$7, prefix_y1
	sw	$2, 0($7)
L6:
	la	$2, prefix_y
	lw	$2, 0($2)
	addi	$20, $2, 0x0
	li	$2, 0x2
	addi	$11, $2, 0x0
	la	$2, prefix_y1
	lw	$2, 0($2)
	addi	$21, $2, 0x0
	mul	$2, $11, $21
	addi	$8, $2, 0x0
	sub	$2, $20, $8
	la	$4, prefix_y0
	sw	$2, 0($4)
	la	$2, prefix_x0
	lw	$2, 0($2)
	addi	$20, $2, 0x0
	la	$2, prefix_y0
	lw	$2, 0($2)
	addi	$23, $2, 0x0
	sne	$20, $20, $0
	sne	$23, $23, $0
	or	$2, $20, $23
	addi	$25, $2, 0x0
	la	$2, prefix_x0
	lw	$2, 0($2)
	addi	$15, $2, 0x0
	la	$2, prefix_y0
	lw	$2, 0($2)
	addi	$6, $2, 0x0
	and	$2, $15, $6
	addi	$9, $2, 0x0
	li	$29, 0x0
	seq	$2, $9, $29
	addi	$13, $2, 0x0
	and	$2, $25, $13
	blez	$2, L7
	li	$2, 0x1
	la	$21, prefix_s0
	sw	$2, 0($21)
	j L8
L7:
	li	$2, 0x0
	la	$3, prefix_s0
	sw	$2, 0($3)
L8:
	la	$2, prefix_x0
	lw	$2, 0($2)
	addi	$19, $2, 0x0
	la	$2, prefix_y0
	lw	$2, 0($2)
	addi	$29, $2, 0x0
	and	$2, $19, $29
	blez	$2, L9
	li	$2, 0x1
	la	$13, prefix_c1
	sw	$2, 0($13)
	j L10
L9:
	li	$2, 0x0
	la	$14, prefix_c1
	sw	$2, 0($14)
L10:
	la	$2, prefix_x1
	lw	$2, 0($2)
	addi	$5, $2, 0x0
	la	$2, prefix_y1
	lw	$2, 0($2)
	addi	$10, $2, 0x0
	and	$2, $5, $10
	addi	$20, $2, 0x0
	la	$2, prefix_c1
	lw	$2, 0($2)
	addi	$19, $2, 0x0
	and	$2, $20, $19
	addi	$6, $2, 0x0
	la	$2, prefix_x1
	lw	$2, 0($2)
	addi	$20, $2, 0x0
	la	$2, prefix_y1
	lw	$2, 0($2)
	addi	$18, $2, 0x0
	li	$15, 0x0
	seq	$2, $18, $15
	addi	$19, $2, 0x0
	and	$2, $20, $19
	addi	$10, $2, 0x0
	la	$2, prefix_c1
	lw	$2, 0($2)
	addi	$15, $2, 0x0
	li	$19, 0x0
	seq	$2, $15, $19
	addi	$5, $2, 0x0
	and	$2, $10, $5
	addi	$8, $2, 0x0
	sne	$6, $6, $0
	sne	$8, $8, $0
	or	$2, $6, $8
	addi	$9, $2, 0x0
	la	$2, prefix_x1
	lw	$2, 0($2)
	addi	$8, $2, 0x0
	li	$5, 0x0
	seq	$2, $8, $5
	addi	$19, $2, 0x0
	la	$2, prefix_y1
	lw	$2, 0($2)
	addi	$6, $2, 0x0
	and	$2, $19, $6
	addi	$10, $2, 0x0
	la	$2, prefix_c1
	lw	$2, 0($2)
	addi	$19, $2, 0x0
	li	$18, 0x0
	seq	$2, $19, $18
	addi	$15, $2, 0x0
	and	$2, $10, $15
	addi	$12, $2, 0x0
	sne	$9, $9, $0
	sne	$12, $12, $0
	or	$2, $9, $12
	addi	$29, $2, 0x0
	la	$2, prefix_x1
	lw	$2, 0($2)
	addi	$9, $2, 0x0
	li	$15, 0x0
	seq	$2, $9, $15
	addi	$19, $2, 0x0
	la	$2, prefix_y1
	lw	$2, 0($2)
	addi	$15, $2, 0x0
	li	$10, 0x0
	seq	$2, $15, $10
	addi	$18, $2, 0x0
	and	$2, $19, $18
	addi	$20, $2, 0x0
	la	$2, prefix_c1
	lw	$2, 0($2)
	addi	$12, $2, 0x0
	and	$2, $20, $12
	addi	$23, $2, 0x0
	sne	$29, $29, $0
	sne	$23, $23, $0
	or	$2, $29, $23
	blez	$2, L11
	li	$2, 0x1
	la	$11, prefix_s1
	sw	$2, 0($11)
	j L12
L11:
	li	$2, 0x0
	la	$25, prefix_s1
	sw	$2, 0($25)
L12:
	la	$2, prefix_x1
	lw	$2, 0($2)
	addi	$23, $2, 0x0
	la	$2, prefix_y1
	lw	$2, 0($2)
	addi	$5, $2, 0x0
	and	$2, $23, $5
	addi	$6, $2, 0x0
	la	$2, prefix_x1
	lw	$2, 0($2)
	addi	$10, $2, 0x0
	la	$2, prefix_c1
	lw	$2, 0($2)
	addi	$19, $2, 0x0
	and	$2, $10, $19
	addi	$20, $2, 0x0
	sne	$6, $6, $0
	sne	$20, $20, $0
	or	$2, $6, $20
	addi	$12, $2, 0x0
	la	$2, prefix_y1
	lw	$2, 0($2)
	addi	$29, $2, 0x0
	la	$2, prefix_c1
	lw	$2, 0($2)
	addi	$20, $2, 0x0
	and	$2, $29, $20
	addi	$8, $2, 0x0
	sne	$12, $12, $0
	sne	$8, $8, $0
	or	$2, $12, $8
	blez	$2, L13
	li	$2, 0x1
	la	$18, prefix_c2
	sw	$2, 0($18)
	j L14
L13:
	li	$2, 0x0
	la	$9, prefix_c2
	sw	$2, 0($9)
L14:
	li	$2, 0x4
	addi	$12, $2, 0x0
	la	$2, prefix_c2
	lw	$2, 0($2)
	addi	$23, $2, 0x0
	mul	$2, $12, $23
	addi	$6, $2, 0x0
	li	$2, 0x2
	addi	$23, $2, 0x0
	la	$2, prefix_s1
	lw	$2, 0($2)
	addi	$8, $2, 0x0
	mul	$2, $23, $8
	addi	$20, $2, 0x0
	add	$2, $6, $20
	addi	$15, $2, 0x0
	la	$2, prefix_s0
	lw	$2, 0($2)
	addi	$5, $2, 0x0
	add	$2, $15, $5
	addi	$2, $2, 0x0
	jr	$31


	.data
	.align 0
prefix_c1:	.word 0
prefix_c2:	.word 0
prefix_s0:	.word 0
prefix_s1:	.word 0
prefix_x:	.word 0
prefix_x0:	.word 0
prefix_x1:	.word 0
prefix_y:	.word 0
prefix_y0:	.word 0
prefix_y1:	.word 0

