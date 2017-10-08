.include beta.uasm

|; Reg[Rc] <- Reg[Ra] mod Reg[Rb] (Rc should be different from Ra and Rb)
.macro MOD(Ra, Rb, Rc) 		DIV(Ra, Rb, Rc)  MUL(Rc, Rb, Rc)  SUB(Ra, Rc, Rc)
.macro MODC(Ra, CC, Rc) 	DIVC(Ra, CC, Rc)  MULC(Rc, CC, Rc)  SUB(Ra, Rc, Rc)
.macro SWAP(Ra, Rb) 		PUSH(Ra)  MOVE(Rb, Ra)  POP(Rb)


| Definition of useful constants.

NB_ROWS = 8
NB_COLS = 32
NB_CELLS = 256
WORDS_PER_MEM_LINE = 8
MEM_LINES_PER_ROW = 8
WORDS_PER_ROW = 64
NB_MAZE_WORDS = 512 
CELLS_PER_WORD = 4
BYTES_PER_WORD = 4


perfect_maze__:
	PUSH(LP)
	PUSH(BP)
	MOVE(SP, BP)
	PUSH(R0)
	PUSH(R1)
	PUSH(R2)
	PUSH(R3)
	PUSH(R4)
	PUSH(R5)

	LD(BP, -12, R0)					 |maze
	LD(BP, -16, R1) 				 |rows
	LD(BP, -20, R2)					 |cols
	LD(BP, -24, R3)					 |visited
	LD(BP, -28, R4) 				 |curr_cell

	DIVC(R4, 32, R5)


connect__:
	PUSH(LP)
	PUSH(BP)
	MOVE(SP, BP)
	PUSH(R0)
	PUSH(R1)
	PUSH(R2)
	PUSH(R3)

	LD(BP, -16, R2)					| One of the two cells to connect
	LD(BP, -20, R3)					| The other cell to connect
	CMPLT(R2, R3, R0)
	BT(R0, . + 4)					| Branch if swap not needed
	SWAP(R2, R3)
							| R2 offset < R3 offset
	DIVC(R3, NB_COLS, R0)
	MULC(R0, WORDS_PER_ROW, R0)			| R0 <- row_offset
	MODC(R2, NB_COLS, R1)				| R1 = R2 % NB_COLS (<- source_col)
	DIVC(R1, CELLS_PER_WORD, R1)			| R1 <- word_offset_in_line
	ADD(R1, R0, R0)
	PUSH(R0)					|Loc. var. 1: word_offset

	MODC(R2, CELLS_PER_WORD), R0);			| R0 = R2 % CELLS_PER_WORD
	PUSH(R0)					| Loc. var. 2: byte_offset

	LD(BP, -12, R1)					| First word of the maze
	SUB(R3, R2, R0)
	SUBC(R0, 1, R0)
	BEQ(R0, hor_connect)
	BNE(R0, ver_connect)


connect_hor:
	CMOVE(0xFFFFFF00, R0)
	JMP(mask_generator, LP)				| R0 <- mask

	POP(R3)						| R3 <- word_offset
	CMOVE(3, R2)
	MULC(R2, WORDS_PER_MEM_LINE, R2)
	ADD(R3, R2, R3)					| R3 <- word_offset + 3 * WORDS_PER_MEM_LINE
	MULC(R3, BYTES_PER_WORD, R3)			| Word offset to byte offset
	ADD(R1, R3, R1)					| R1 <- address of the first word to change
	JMP(word_change, LP)				| First word changed

	CMOVE(BYTES_PER_WORD, R3)
	MULC(R3, WORDS_PER_MEM_LINE, R3)
	ADD(R1, R3, R1)					| R1 <- address of the second word to change
	JMP(word_change, LP)				| Second word changed

	ADD(R1, R3, R1)					| R1 <- address of the third word to change
	JMP(word_change, LP)				| Third word changed

	ADD(R1, R3, R1)					| R1 <- address of the fourth word to change
	JMP(word_change, LP)				| Fourth word changed

	JMP(connect_end)


connect_ver:
	CMOVE(0xFFFFFFE1, R0)
	JMP(mask_generator, LP)				| R0 <- mask

	POP(R3)						| R3 <- word_offset
	MULC(R3, BYTES_PER_WORD, R3)			| Word offset to byte offset
	ADD(R1, R3, R1)					| R1 <- address of the first word to change
	JMP(word_change, LP)				| First word changed

	CMOVE(BYTES_PER_WORD, R3)
	MULC(R3, WORDS_PER_MEM_LINE, R3)
	ADD(R1, R3, R1)					| R1 <- address of the second word to change
	JMP(word_change, LP)				| Second word changed

	JMP(connect_end)


mask_generator:
	POP(R3)						| R3 <- byte_offset
	MULC(R3, 8, R2)
	SHL(R0, R2, R0)					| R0 <- R0 << (8 * byte_offset)
	CMOVE(4, R2)
	SUB(R2, R3, R3)					| R3 <- 4 - byte_offset
	MULC(R3, 8, R3)
	CMOVE(0xFFFFFFFF, R2)
	SHR(R2, R3, R2)					| R2 <- R2 >> 8 * (4 - byte_offset)
	OR(R0, R2, R0)					| R0 <- mask
	JMP(LP)


word_change:
	LD(R1, 0, R2)
	AND(R2, R0, R2)
	ST(R2, 0, R1)					| <R1> <- mask & Mem[R1]
	JMP(LP)


connect_end:
	POP(R3)
	POP(R2)
	POP(R1)
	POP(R0)
	POP(BP)
	POP(LP)
	RTN()
