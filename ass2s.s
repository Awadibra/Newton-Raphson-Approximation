section .data
	receive_epsilon: db "epsilon = %lf",10,0

    receive_order: db "order = %d",10,0

    receive_coeff:db "coeff %d = %lf %lf",10,0

    receive_initial:db "%*s = %lf %lf",10,0

    root_format :db "root = %.16lf %.16e",10,0
    horner_format :db "horner = %.16lf %.16e",10,0
    hornerder_format :db "horner der = %.16lf %.16e",10,0
    divi_format :db "divi = %.16lf %.16lf",10,0
    norm_format :db "norm = %.16lf",10,0
	message: db "the nums are %.16lf %.16lf\n",0
	minusOne: dq -1.0
	zero: dq 0.0
	one: dq 1.0
	conjugate: dq 0.0, 0.0
	conjb: dq 0.0
	index :dq 1.0
section .bss
	norma: resq 1
	resultR: resq 1
	resultIm: resq 1
	resulthR: resq 1
	resulthIm: resq 1
	resulthderR: resq 1
	resulthderIm: resq 1
	ar: resq 1
	aim: resq 1
	br: resq 1
	bim:resq 1
	cr: resq 1
	cim: resq 1
	har:resq 1
	haim:resq 1
	hbr: resq 1
	hbim:resq 1
	hdar: resq 1
	hdaim: resq 1
	hdbr: resq 1
	hdbim:resq 1
	initial: resq 2
	initialR:resq 1
	initialIm: resq 1
	epsilon: resq 1
	order: resq 1
	coeff: resq 1
	real: resq 1
	imag: resq 1
	func: resq 1
	der: resq 1
	derVar:resq 1
	rnorm:resq 1
	imnorm: resq 1

section .text
	global main
    global add
    global sub
    global mult
    global divi
    global ABC
    extern printf
    extern scanf
    extern malloc

add:
    push rbp
    mov rbp,rsp						;prepare a frame

    finit							;initialize the x87 subsystem
    fld qword [ar]					;load ar into st0
    fld qword [br]					;load br into st0, ar now in st1
    fadd st1						;add st1(ar) to st0(br) and store into st0
    fst qword [resultR]				;store st0(ar+br) into resultR
    fld qword [aim]					;load aim into st0
    fld qword [bim]					;load bim into st0, aim now in st1
    fadd st1						;add st1(aim) to st0(bim) and store into st0
    fst qword [resultIm]			;store st0(aim+bim) into resultIm

    mov rsp,rbp
    pop rbp
    ret

sub:
	push rbp
	mov rbp,rsp						;prepare a frame

	finit							;initialize the x87 subsystem
	fld qword [ar]					;load ar into st0
	fld qword [br]					;load br into st0, ar now in st1
    fsubr st1						;sub st1(ar) from st0(br) and store into st0
    fst qword [resultR]				;store st0(ar-br) into resultR
    fld qword [aim]					;load aim into st0
    fld qword [bim]					;load bim into st0, aim now in st1
    fsubr st1						;sub st1(aim) from st0(bim) and store into st0
    fst qword [resultIm]			;store st0(aim-bim) into resultIm

    mov rsp,rbp
    pop rbp
    ret


mult:
	push rbp
	mov rbp,rsp						;prepare a frame

	finit							;initialize the x87 subsystem
	fld qword [ar]					;load ar into st0
	fld qword [br]					;load br into st0, ar now in st1
	fmulp 							;mult st0(br) with st1(ar) and store (ar*br)in st0
	fld qword [aim]					;load aim into st0
	fld qword [bim]					;load bim into st0, aim now in st1
	fmulp 							;mult st0(aim) with st1(bim) and store (aim*bim) in st0
	fld qword [minusOne]			;load -1 (for i^2)
	fmulp 							;multiply (aim*bim*-1) and store in st0
	faddp							;add st0(aim*bim*-1) to st1(ar*br) and store in st0
	fst qword [resultR]				;store st0(resultR) into r9(result[0])
	fld qword [ar]					;load ar into st0
	fld qword [bim]					;load bim into st0, now st1(ar)
	fmulp							;st0 = (ar*bim)
	fld qword [aim]					;load aim into st0
	fld qword [br]					;load br into st0, now st1(aim)
	fmulp							;st0=(aim*br)
	faddp							;add st1(ar*bim) to st0(aim*br) and store in st0
	fst qword [resultIm]			;store (aim*br+ar*bim) in result[1]

	mov rsp,rbp
	pop rbp
	ret

divi:
	push rbp
	mov rbp,rsp						;prepare a frame

	finit
	fld qword [br]					;st0 =br
	fld qword [br]					;st0 = br
	fmulp st1						;st0 = br*br
	fld qword [bim]
	fld qword [bim]
	fmulp st1						;st0 = bim*bim
	faddp st1                		;st0 = conjb
	fst qword [conjb]

	fld qword [ar]					;st0 = ar, st1= conjb
	fld qword [br]					;st0=br, st1 = ar, st2= conjb
	fmulp st1 						;st0 =ar*br, st1= conjb
	fld qword [aim]					;st0=aim, st1 =ar*br, st2= conjb
	fld qword [bim]					;st0=bim, st1=aim, st2 =ar*br, st3= conjb
	fmulp st1						;st0= aim*bim,st1 = ar*br,st2=conjb
	faddp st1						;st0=aim*bim+ar*br,st1=conjb
	fdiv st1 						;st0 = resultR,st1=conjb
	fld qword [aim]					;st0=aim
	fld qword [br]					;st0=br ,st1=aim,st2=resultR,st3=conjb
	fmulp st1						;st0=aim*br,st1=resultR,st2=conjb
	fld qword [ar]					;st0=ar
	fld qword [bim]					;st0=bim
	fmulp st1 						;st0=ar*bim,st1=aim*br,st2=resultR,st3=conjb
	fsubp st1						;st0=aim*br-ar*bim, st1 = resultR,st2 = conjb
	fdiv st2 						;st0 =resultIm,st1=resultR,st2 = conjb

	fstp qword [resultIm]
	fstp qword [resultR]				;st0=conjb

	mov rsp,rbp
	pop rbp
	ret


ABC:								; A*B+C
	push rbp
	mov rbp,rsp						;prepare a frame

	finit
	fld qword [ar]					;load ar into st0
	fld qword [br]					;load br into st0, ar now in st1
	fmulp 							;st0=ar*br
	fld qword [aim]					;load aim into st0
	fld qword [bim]					;load bim into st0, aim now in st1
	fmulp 							;st0=aim*bim,st1=ar*br

	fsubp							;st0 = ar*br-aim*bim
	fld qword [cr]					;st0 = cr , st0=ar*br-aim*bim
	faddp							;st0 = ar*br - aim*bim + cr
	fstp qword [resultR]			;r10 = resultR
	fld qword [ar]					;st0 = ar
	fld qword [bim]					;st0 = bim
	fmulp							;st0 = ar*bim
	fld qword [br]					;st0 = br ,st1 = ar*bim
	fld qword [aim]					;st0 = aim,st1= br ,st2 = ar*bim
	fmulp 							;st0 = aim*br ,st1 = ar*bim
	faddp							;st0 = aim*br + ar*bim

	fld qword [cim]					;st0 = cim ,st1 = aim*br + ar*bim
	faddp							;st0 = aim*br + ar*bim + cim
	fstp qword [resultIm]			;r10+8=resultIm

	mov rsp,rbp
	pop rbp
	ret


horner:
	push rbp
	mov rbp,rsp						;prepare a frame

	mov rcx, qword[order]
	mov r14, qword[order]
	shl r14,4
	finit
	mov r13,[func]
	mov r12,[r13+r14]
	mov qword[ar], r12
	add r14,8
	mov r12,[r13+r14]
	mov qword[aim],r12
	mov r11,qword[initialR]
	mov qword[br],r11
	mov r11,qword[initialIm]
	mov qword[bim],r11
	cont:
		sub r14,24
		mov r12,[r13+r14]
		mov qword[cr],r12
		add r14,8
		mov r12,[r13+r14]
		mov qword[cim],r12
		call ABC
		mov r11,qword[resultR]
		mov qword[ar],r11
		mov r11,qword[resultIm]
		mov qword[aim],r11
		loop cont,rcx

	fld qword[resultR]
	fstp qword[resulthR]
	fld qword[resultIm]
	fstp qword[resulthIm]
	mov rsp,rbp
	pop rbp
	ret

hornerder:
	push rbp
	mov rbp,rsp						;prepare a frame

	mov rcx, qword[order]
	dec rcx
	mov r14, qword[order]
	dec r14
	shl r14,4
	finit
	mov r13,[der]
	mov r12,[r13+r14]
	mov qword[ar], r12
	add r14,8
	mov r12,[r13+r14]
	mov qword[aim],r12
	mov r11,qword[initialR]
	mov qword[br],r11
	mov r11,qword[initialIm]
	mov qword[bim],r11
	cmp rcx,0
	je zeroorder
	contder:
		sub r14,24
		mov r12,[r13+r14]
		mov qword[cr],r12
		add r14,8
		mov r12,[r13+r14]
		mov qword[cim],r12
		call ABC
		mov r11,qword[resultR]
		mov qword[ar],r11
		mov r11,qword[resultIm]
		mov qword[aim],r11
		loop contder,rcx
	jmp endhorder
	zeroorder:
		fld qword[ar]
		fstp qword[resultR]
		fld qword[aim]
		fstp qword[resultIm]
		jmp endhorder
	endhorder:
		fld qword[resultR]
		fstp qword[resulthderR]
		fld qword[resultIm]
		fstp qword[resulthderIm]


	mov rsp,rbp
	pop rbp
	ret

norm:
	push rbp
	mov rbp,rsp						;prepare a frame
	finit
	fld qword[rnorm]
	fld qword[rnorm]
	fmulp
	fld qword[imnorm]
	fld qword[imnorm]
	fmulp
	faddp
	fsqrt
	fstp qword[norma]
	mov rsp,rbp
	pop rbp
	ret


main:
	push rbp
	mov rbp,rsp						;prepare a frame

	mov rdi, receive_epsilon		;prepare to receive epsilon
	mov rsi, epsilon
	call scanf						;receive epsilon

	mov rax, 0
	mov rdi, receive_order			;prepare to receive order
	mov rsi, order
	call scanf						;receive order

	mov rdi,[order]  				;prepare malloc
	inc rdi
	shl rdi, 4						;order*2(real&imaginary)*8(sizeof double)
	call malloc						;pointer to memory allocated is returned in rax
	mov qword [func], rax					;save pointer to memory in func

	mov rdi,[order]
	imul rdi,16
	call malloc
	mov [der],rax

	mov r15,[order]
	inc r15
	mov r14,[func]
	get_coeff:
		mov rdi, receive_coeff		;receive coeff
		mov rsi, coeff
		mov rdx, real
		mov rcx, imag
		call scanf
		mov r8,[coeff]
		mov rax,[real]
		imul r8,16
		mov [r14+r8],rax
		mov rax,[imag]
		add r8,8
		mov [r14+r8],rax
		dec r15
		cmp r15,0
		jg get_coeff

	mov rdi, receive_initial		;receive initial
	mov rsi, initialR
	mov rdx, initialIm
	call scanf

	mov r14,16
	mov r13,0
	mov rcx,[order]
	mov r15,[func]
	mov r12,[der]
	derivative:
		finit
		fld qword[r15+r14]
		fld qword[index]
		fmulp
		fstp qword[r12+r13]
		add r14,8
		fld qword[r15+r14]
		fld qword[index]
		fmulp
		add r13,8
		fstp qword [r12+r13]
		add r14,8
		add r13,8
		fld qword[index]
		fld qword[one]
		faddp
		fstp qword[index]
		loop derivative,rcx


	;mov r15,7
	hornerloop:
		finit

		call horner
		fld qword[resulthR]
		fstp qword[rnorm]
		fld qword [resulthIm]
		fstp qword[imnorm]
		call norm

		fld qword[norma]
		fld qword[epsilon]
		fcomi st1
		ja end

		call horner
		fld qword[resulthR]
		fstp qword[har]
		fld qword[resulthIm]
		fstp qword[haim]

		call hornerder
		fld qword[resulthderR]
		fstp qword[hdbr]
		fld qword [resulthderIm]
		fstp qword [hdbim]

		fld qword[har]
		fstp qword[ar]
		fld qword[haim]
		fstp qword[aim]
		fld qword[hdbr]
		fstp qword[br]
		fld qword[hdbim]
		fstp qword[bim]
		call divi

		fld qword[initialR]
		fld qword[resultR]
		fsubp
		fstp qword [initialR]

		fld qword[initialIm]
		fld qword[resultIm]
		fsubp
		fstp qword[initialIm]
		jmp hornerloop

	end:
		mov rdi, root_format
		movsd xmm0,qword[initialR]
		movsd xmm1,qword[initialIm]
		mov rax,2
		call printf

	mov rsp,rbp
	pop rbp
	ret
