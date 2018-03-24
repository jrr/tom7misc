# original version of peri_write, before optimizing..
bcm2835_peri_write:
	.fnstart
.LFB46:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	ldr	r3, .L61
	ldrb	r3, [r3]	@ zero_extendqisi2
	cmp	r3, #0
	bne	.L60
	mcr	p15, 0, r0, c7, c10, 5
	str	r1, [r0]
	mcr	p15, 0, r0, c7, c10, 5
	bx	lr
.L60:
	mov	r2, r1
	mov	r1, r0
	ldr	r0, .L61+4
	b	printf
.L62:
	.align	2
.L61:
	.word	.LANCHOR1
	.word	.LC4
	.cantunwind
	.fnend
	.size	bcm2835_peri_write, .-bcm2835_peri_write
	.align	2
	.global	bcm2835_peri_write_nb
	.syntax unified
	.arm
	.fpu vfp
	.type	bcm2835_peri_write_nb, %function
