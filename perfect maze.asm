
| Definition of useful constants.

WORDS_PER_MEM_LINE = 8
WORDS_PER_ROW = 64
CELLS_PER_WORD = 4
BYTES_PER_WORD = 4





|; Reg[Rc] <- Reg[Ra] mod CC (Rc should be different from Ra)
.macro MODC(Ra, CC, Rc) 	DIVC(Ra, CC, Rc)  MULC(Rc, CC, Rc)  SUB(Ra, Rc, Rc)

|; Reg[Ra] <-> Reg[Rb]
.macro SWAP(Ra, Rb) 		PUSH(Ra)  MOVE(Rb, Ra)  POP(Rb)



perfect_maze:
	PUSH(LP)
	PUSH(BP)
	MOVE(SP, BP)
	PUSH(R0)
	PUSH(R1)
	PUSH(R2)
	PUSH(R3)
	PUSH(R4)
	PUSH(R5)

	LD(BP, -20, R2)							| R2 <- nb_cols
	LD(BP, -24, R3)					 		| R3 <- bitmap "visited"
	LD(BP, -28, R4) 				 		| R4 <- curr_cell
	PUSH(R3)								| Arg. 2 <- bitmap "visited"
	PUSH(R4)								| Arg. 1 <- curr_cell
	CALL(change_to_visited__, 2)
	CMOVE(0, R1)							| R1 = nb_valid_neighbours
	
neighbour_1:
	MOD(R4, R2, R0)							| R0 <- col of curr_cell
	BF(R0, neighbour_2)
	ADDC(R1, 1, R1)
	SUBC(R4, 1, R5)
	PUSH(R5)								| Stack <- cell of a valid neighbour (left)

neighbour_2:
	SUBC(R2, 1, R5)
	CMPEQ(R5, R0, R5)
	BT(R5, neighbour_3)
	ADDC(R1, 1, R1)
	ADDC(R4, 1, R5)
	PUSH(R5)								| Stack <- cell of a valid neighbour (right)

neighbour_3:
	DIV(R4, R2, R0)							| R0 <- row of curr_cell
	BEQ(R0, neighbour_4)
	ADDC(R1, 1, R1)
	SUB(R4, R2, R5)
	PUSH(R5)								| Stack <- cell of a valid neighbour (top)

neighbour_4:
	LD(BP, -16, R5)							| R5 <- rows
	SUBC(R5, 1, R5)
	CMPEQ(R5, R0, R5)
	BT(R5, build_maze_loop)
	ADDC(R1, 1, R1)
	ADD(R4, R2, R0)
	PUSH(R0)								| Stack <- cell of a valid neighbour (bottom)
	

build_maze_loop:
	BEQ(R1, perfect_maze_end)
	RANDOM()
	CMPLTC(R0, 0, R2)
	BF(R2, . + 8)
	MULC(R0, -1, R0)
	MOD(R0, R1, R2)
	ADDC(R2, 1, R2)							| R2 <- neighbour chosen randomly
	MULC(R2, -4, R2)
	ADD(SP, R2, R2)
	LD(R2, 0, R5)							| R5 <- nb cell of the chosen neighbour
	SUBC(R1, 1, R1)
	LD(SP, -4, R0)
	ST(R0, 0, R2)							
	DEALLOCATE(1)							| Deletion of the chosen neighbour cell 
											| on the stack

	PUSH(R3)								| Arg. 2 <- bitmap "visited"
	PUSH(R5)								| Arg. 1 <- chosen neighbour cell
	CALL(is_visited__, 2)
	BT(R0, build_maze_loop)
	LD(BP, -20, R2)							
	PUSH(R2)								| Arg. 4 <- nb_cols
	PUSH(R5)								| Arg. 3 <- chosen neighbour cell
	PUSH(R4)								| Arg. 2 <- curr_cell
	LD(BP, -12, R4)
	PUSH(R4)								| Arg. 1 <- maze
	CALL(connect__, 4)

	PUSH(R5)								| Arg. 5 <- chosen neighbour cell
	PUSH(R3)								| Arg. 4 <- bitmap "visited"
	PUSH(R2)								| Arg. 3 <- nb_cols
	LD(BP, -16, R5)
	PUSH(R5)								| Arg. 2 <- nb_rows
	PUSH(R4)								| Arg. 1 <- maze
	CALL(perfect_maze, 5)

	CMPLEC(R1, 0, R5)
	BEQ(R5, build_maze_loop)

perfect_maze_end:
	POP(R5)
	POP(R4)
	POP(R3)
	POP(R2)
	POP(R1)
	POP(R0)
	POP(BP)
	POP(LP)
	JMP(LP)



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
	MULC(R2, BYTES_PER_WORD, R2)			| Word offset to byte offset for bitmap
	LD(BP, -16, R3)							| R3 <- Address of the first word of the bitmap
	ADD(R3, R2, R3)  
	PUSH(R3)								| Stack <- Address of the word to check
	CMOVE(1, R3)
	MODC(R1, 32, R2)
	SHL(R3, R2, R3)
	POP(R1)									| R1 <- Address of the word to check
	LD(R1, 0, R2)							| R2 <- Word of the bitmap to check
	AND(R2, R3, R2)
	BF(R2, is_visited_end)
	CMOVE(1, R0)

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

	LD(BP, -16, R2)
	LD(BP, -20, R3)							| R2 and R3 <- cells to connect
	LD(BP, -24, R1)							| R1 <- nb_cols
	CMPLT(R2, R3, R0)
	BT(R0, R2_less_than_R3)
	SWAP(R2, R3)

R2_less_than_R3:									
	DIV(R3, R1, R0)
	MULC(R0, WORDS_PER_ROW, R0)				| R0 <- row_offset of cell contained in R3
	MOD(R2, R1, R4)							| R4 <- col of cell contained in R2
	PUSH(R4)
	DIVC(R4, CELLS_PER_WORD, R4)			| R4 <- word_offset_in_line of cell contained in R2
	ADD(R4, R0, R0)
	POP(R4)									| R4 <- source_col
	PUSH(R0)								| Loc. var. 1: word_offset of the cell to modify

	MODC(R4, CELLS_PER_WORD, R0)			| R0 <- byte_offset 
	PUSH(R0)								| Loc. var. 2: byte_offset

	LD(BP, -12, R1)							| First word of the maze
	SUB(R3, R2, R0)
	SUBC(R0, 1, R0)
	BEQ(R0, connect_hor)
	BNE(R0, connect_ver)


connect_hor:
	CMOVE(0xFFFFFF00, R0)
	BR(mask_generator, LP)					| R0 <- mask

	POP(R3)									| R3 <- word_offset
	CMOVE(3, R2)
	MULC(R2, WORDS_PER_MEM_LINE, R2)
	ADD(R3, R2, R3)							| R3 <- word_offset + 3 * WORDS_PER_MEM_LINE
	MULC(R3, BYTES_PER_WORD, R3)			| Word offset to byte offset
	ADD(R1, R3, R1)							| R1 <- address of the first word to change
	BR(word_change, LP)

	CMOVE(BYTES_PER_WORD, R3)
	MULC(R3, WORDS_PER_MEM_LINE, R3)
	ADD(R1, R3, R1)							| R1 <- address of the second word to change
	BR(word_change, LP)	

	ADD(R1, R3, R1)							| R1 <- address of the third word to change
	BR(word_change, LP)

	ADD(R1, R3, R1)							| R1 <- address of the fourth word to change
	BR(word_change, LP)

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
	CMOVE(32, R3)
	SUB(R3, R2, R3)
	CMOVE(0xFFFFFFFF, R2)
	SHR(R2, R3, R2)							| R2 <- 0xFFFFFFFF >> 32 - 8 * byte_offset)
	CMPLTC(R3, 32, R3)						| Shift isn't performed if R3 = 32
	BT(R3, . + 8)
	CMOVE(0x00000000, R2)
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
