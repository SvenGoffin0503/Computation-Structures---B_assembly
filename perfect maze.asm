.include beta.uasm


| Definition of useful constants.

WORDS_PER_MEM_LINE = 8
WORDS_PER_ROW = 64
CELLS_PER_WORD = 4
BYTES_PER_WORD = 4



|; Reg[Rc] <- Reg[Ra] mod Reg[Rb] (Rc should be different from Ra and Rb)
.macro MOD(Ra, Rb, Rc) 		DIV(Ra, Rb, Rc)  MUL(Rc, Rb, Rc)  SUB(Ra, Rc, Rc)

|; Reg[Rc] <- Reg[Ra] mod CC (Rc should be different from Ra)
.macro MODC(Ra, CC, Rc) 	DIVC(Ra, CC, Rc)  MULC(Rc, CC, Rc)  SUB(Ra, Rc, Rc)

|; Reg[Ra] <-> Reg[Rb]
.macro SWAP(Ra, Rb) 		PUSH(Ra)  MOVE(Rb, Ra)  POP(Rb)



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

	LD(BP, -20, R2)							| R2 <- cols
	LD(BP, -24, R3)					 		| R3 <- bitmap "visited"
	LD(BP, -28, R4) 				 		| R4 <- curr_cell

	PUSH(R3)								| Arg. 2 <- bitmap "visited"
	PUSH(R4)								| Arg. 1 <- curr_cell
	CALL(change_to_visited, 2)
	CMOVE(0, R1)							| R1 = nb_valid_neighbours
	
	MOD(R4, R2, R0)							| R0 <- col of curr_cell
	BF(R0, . + 12)
	ADDC(R1, 1, R1)
	SUBC(R4, 1, R5)
	PUSH(R5)								| Stack <- cell of a valid neighbour

	SUBC(R2, 1, R5)
	CMPEQ(R5, R0, R5)
	BT(R5, . + 12)
	ADDC(R1, 1, R1)
	ADDC(R4, 1, R5)
	PUSH(R5)								| Stack <- cell of a valid neighbour

	DIV(R4, R2, R0)							| R0 <- row of curr_cell
	BEQ(R0, . + 12)
	ADDC(R1, 1, R1)
	ADD(R4, R2, R5)
	PUSH(R5)								| Stack <- cell of a valid neighbour

	LD(BP, -16, R5)							| R5 <- rows
	SUBC(R5, 1, R5)
	CMPEQ(R5, R0, R5)
	BT(R5, . + 12)
	ADDC(R1, 1, R1)
	ADD(R4, R2, R0)
	PUSH(R0)								| Stack <- cell of a valid neighbour

build_maze_loop:
	RANDOM()
	CMPLTC(R0, 0, R2)
	BF(R2, . + 4)
	MULC(R0, -1, R0)
	MOD(R0, R1, R2)
	ADDC(R2, 1, R2)							| R2 <- neighbour chosen randomly
	MULC(R2, -4, R2)
	ADD(SP, R2, R2)
	LD(R2, 0, R0)							| R0 <- nb cell of the chosen neighbour
	SUBC(R1, 1, R1)
	LD(SP, -4, R5)
	ST(R5, 0, R2)							
	DEALLOCATE(1)							| Deletion of the chosen neighbour cell 
											| on the stack

	PUSH(R3)								| Arg. 2 <- bitmap "visited"
	PUSH(R0)								| Arg. 1 <- chosen neighbour cell
	CALL(is_visited__, 2)
	BT(build_maze_loop)

	LD(BP, -20, R2)							
	PUSH(R2)								| Arg. 4 <- nb_cols
	PUSH(R0)								| Arg. 3 <- chosen neighbour cell
	PUSH(R4)								| Arg. 2 <- curr_cell
	LD(BP, -12, R4)
	PUSH(R4)								| Arg. 1 <- maze
	CALL(connect__, 4)

	PUSH(R0)								| Arg. 5 <- chosen neighbour cell
	PUSH(R3)								| Arg. 4 <- bitmap "visited"
	PUSH(R2)								| Arg. 3 <- nb_cols
	LD(BP, -16, R5)
	PUSH(R5)								| Arg. 2 <- nb_rows
	PUSH(R4)								| Arg. 1 <- maze
	CALL(perfect_maze__, 5)

	BEQ(R1, build_maze_loop)

perfect_maze_end:
	POP(R5)
	POP(R4)
	POP(R3)
	POP(R2)
	POP(R1)
	POP(R0)
	POP(BP)
	POP(LP)
	RTN()



|; change_to_visited(curr_cell, bitmap)
|; Marks the cell curr_cell as visited

change_to_visited__:
	PUSH(LP)
	PUSH(BP)
	MOVE(SP, BP)
	PUSH(R1)
	PUSH(R2)
	PUSH(R3)
	
	LD(BP, -12, R1)							| R1 <- curr_cell
	DIVC(R1, 32, R2)
	MULC(R2, 4, R2)							| Word offset to byte offset for bitmap
	LD(BP, -16, R3)							| R3 <- Address of the first word of the bitmap
	ADD(R3, R2, R3)  
	PUSH(R3)								| Stack <- Address of the word to modify
	CMOVE(1, R3)
	MODC(R1, 32, R2)
	SHL(R3, R2, R3)
	POP(R1)									| R1 <- Address of the word to modify
	LD(R1, 0, R2)							| R2 <- Word of the bitmap to modify
	OR(R2, R3, R2)							| Modification of the concerned word
	ST(R2, 0, R1)

	POP(R3)
	POP(R2)
	POP(R1)
	POP(BP)
	POP(LP)
	RTN()
	


|; is_visited__(curr_cell, bitmap)
|; Returns 1 if curr_cell has already been visited, 0 otherwise.
|; Side effects: modifies the value contained in R0.

is_visited__:
	PUSH(LP)
	PUSH(BP)
	MOVE(SP, BP)
	PUSH(R1)
	PUSH(R2)
	PUSH(R3)
	
	CMOVE(0, R0)
	LD(BP, -12, R1)							| R1 <- curr_cell
	DIVC(R1, 32, R2)
	MULC(R2, 4, R2)							| Word offset to byte offset for bitmap
	LD(BP, -16, R3)							| R3 <- Address of the first word of the bitmap
	ADD(R3, R2, R3)  
	PUSH(R3)								| Stack <- Address of the word to check
	CMOVE(1, R3)
	MODC(R1, 32, R2)
	SHL(R3, R2, R3)
	POP(R1)									| R1 <- Address of the word to check
	LD(R1, 0, R2)							| R2 <- Word of the bitmap to check
	AND(R2, R3, R2)
	BF(is_visited_end)
	ADDC(R0, 1, R0)

is_visited_end:
	POP(R3)
	POP(R2)
	POP(R1)
	POP(BP)
	POP(LP)
	RTN()




connect__:
	PUSH(LP)
	PUSH(BP)
	MOVE(SP, BP)
	PUSH(R0)
	PUSH(R1)
	PUSH(R2)
	PUSH(R3)
	PUSH(R4)

	LD(BP, -8, R2)
	LD(BP, -12, R3)							| R2 and R3 <- cells to connect
	LD(BP, -16, R1)							| R1 <- nb_cols
	CMPLT(R2, R3, R0)
	BT(R0, . + 4)
	SWAP(R2, R3)							| nb_cell of R2 < nb_cell of R3
											
	DIV(R3, R1, R0)
	MULC(R0, WORDS_PER_ROW, R0)				| R0 <- row_offset
	MOD(R2, R1, R4)							| R4 <- R2 % nb_cols (<- source_col)
	DIVC(R4, CELLS_PER_WORD, R4)			| R4 <- word_offset_in_line
	ADD(R4, R0, R0)
	PUSH(R0)								| Loc. var. 1: word_offset

	MODC(R2, CELLS_PER_WORD, R0)			| R0 <- byte_offset 
	PUSH(R0)								| Loc. var. 2: byte_offset

	LD(BP, -4, R1)							| First word of the maze
	SUB(R3, R2, R0)
	SUBC(R0, 1, R0)
	BEQ(R0, hor_connect)
	BNE(R0, ver_connect)


connect_hor:
	CMOVE(0xFFFFFF00, R0)
	BR(mask_generator, LP)					| R0 <- mask

	POP(R3)									| R3 <- word_offset
	CMOVE(3, R2)
	MULC(R2, WORDS_PER_MEM_LINE, R2)
	ADD(R3, R2, R3)							| R3 <- word_offset + 3 * WORDS_PER_MEM_LINE
	MULC(R3, BYTES_PER_WORD, R3)			| Word offset to byte offset
	ADD(R1, R3, R1)							| R1 <- address of the first word to change
	BR(word_change, LP)						| First word changed

	CMOVE(BYTES_PER_WORD, R3)
	MULC(R3, WORDS_PER_MEM_LINE, R3)
	ADD(R1, R3, R1)							| R1 <- address of the second word to change
	BR(word_change, LP)						| Second word changed

	ADD(R1, R3, R1)							| R1 <- address of the third word to change
	BR(word_change, LP)						| Third word changed

	ADD(R1, R3, R1)							| R1 <- address of the fourth word to change
	BR(word_change, LP)						| Fourth word changed

	BR(connect_end)


connect_ver:
	CMOVE(0xFFFFFFE1, R0)
	BR(mask_generator, LP)					| R0 <- mask

	POP(R3)									| R3 <- word_offset
	MULC(R3, BYTES_PER_WORD, R3)			| Word offset to byte offset
	ADD(R1, R3, R1)							| R1 <- address of the first word to change
	BR(word_change, LP)						| First word changed

	CMOVE(BYTES_PER_WORD, R3)
	MULC(R3, WORDS_PER_MEM_LINE, R3)
	ADD(R1, R3, R1)							| R1 <- address of the second word to change
	BR(word_change, LP)						| Second word changed

	BR(connect_end)


mask_generator:
	POP(R3)									| R3 <- byte_offset
	MULC(R3, 8, R2)
	SHL(R0, R2, R0)							| R0 <- 0xFFFFFFxx << (8 * byte_offset)
											| where xx is 00 or E1
	CMOVE(4, R2)
	SUB(R2, R3, R3)
	MULC(R3, 8, R3)
	CMOVE(0xFFFFFFFF, R2)
	SHR(R2, R3, R2)							| R2 <- 0xFFFFFFFF >> 8 * (4 - byte_offset)
	OR(R0, R2, R0)							| R0 <- mask
	JMP(LP)


word_change:
	LD(R1, 0, R2)
	AND(R2, R0, R2)
	ST(R2, 0, R1)							| <R1> <- mask & Mem[R1]
	JMP(LP)


connect_end:
	POP(R4)
	POP(R3)
	POP(R2)
	POP(R1)
	POP(R0)
	POP(BP)
	POP(LP)
	RTN()
