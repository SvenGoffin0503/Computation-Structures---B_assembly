
| Definition of useful constants.

WORDS_PER_MEM_LINE = 8
WORDS_PER_ROW = 64
CELLS_PER_WORD = 4
BYTES_PER_WORD = 4


|; Reg[Rc] <- Reg[Ra] mod CC (Rc should be different from Ra)
.macro MODC(Ra, CC, Rc) 	DIVC(Ra, CC, Rc)  MULC(Rc, CC, Rc)  SUB(Ra, Rc, Rc)

|; Reg[Ra] <-> Reg[Rb]
.macro SWAP(Ra, Rb) 		PUSH(Ra)  MOVE(Rb, Ra)  POP(Rb)


|; Perfect maze function 
|; 		-> Creates the perfect maze
|; 		-> At the end, no modification of the values of the registers

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

	LD(BP, -20, R2)							| R2 <- nb_col
	LD(BP, -24, R3)					 		| R3 <- visited
	LD(BP, -28, R4) 				 		| R4 <- cur_cell

|; Creation of identifiers
	col_cur_cell = R0 						| The value of the column of the current cell
	row_cur_cell = R0 						| The value of the row of the current cell
	nb_val_n = R1 							| Number of valid neighbours
	nb_col = R2 							| Number of columns in the maze
	visited = R3 							| The bitmap which indicates if the cell has already been visited (1) or not (0)
	cur_cell = R4 							| The initial cell where the maze has to be started
	val_neigh = R5 							| The value of the valid neighbour
	nb_row = R5 							| Number of rows in the maze

	BR(change_to_visited, LP)
	CMOVE(0, nb_val_n)
	

|; Check left neighbour function 
|; 		-> if col_cur_cell = 0, go to the check right neighbour function. Otherwise, put the value of the valid neighbour on the stack.

check_left_neighbour:
	MOD(cur_cell, nb_col, col_cur_cell)
	BEQ(col_cur_cell, check_right_neighbour)
	ADDC(nb_val_n, 1, nb_val_n)
	SUBC(cur_cell, 1, val_neigh)
	PUSH(val_neigh)


|; Check right neighbour function 
|; 		-> if col_cur_cell < nb_col - 1, put the value of the valid neighbour on the stack. Otherwise, go to the check top neighbour function.  

check_right_neighbour:
	SUBC(nb_col, 1, R5)
	CMPEQ(R5, col_cur_cell, R5)
	BT(R5, check_top_neighbour)
	ADDC(nb_val_n, 1, nb_val_n)
	ADDC(cur_cell, 1, val_neigh)
	PUSH(val_neigh)


|; Check top neighbour function 
|; 		-> if row_cur_cell = 0, go to the check bottom neighbour function. Otherwise, put the value of the valid neighbour on the stack.

check_top_neighbour:
	DIV(cur_cell, nb_col, row_cur_cell)
	BEQ(row_cur_cell, check_bottom_neighbour)
	ADDC(nb_val_n, 1, nb_val_n)
	SUB(cur_cell, nb_col, val_neigh)
	PUSH(val_neigh)


|; Check bottom neighbour function 
|; 		-> if row_cur_cell < nb_row - 1, put the value of the valid neighbour on the stack. Otherwise, go to the build maze loop function.

check_bottom_neighbour:
	LD(BP, -16, nb_row)
	SUBC(nb_row, 1, R5)
	CMPEQ(R5, row_cur_cell, R5)
	BT(R5, build_maze_loop)
	ADDC(nb_val_n, 1, nb_val_n)
	ADD(cur_cell, nb_col, val_neigh)
	PUSH(val_neigh)


|; Build maze loop is a function which explores the valid neighbours and build the perfect maze
|; 		-> We need the valid neigbour which is at this time stored in R5
|; 		-> Side effects : the values contained in R2 and R5 are modified

build_maze_loop:
	chos_neigh = R5

	BEQ(nb_val_n, perfect_maze_end) 		| When there is no more neigbours to explore, end the entire function
	RANDOM() 								| Get a random number
	CMPLTC(R0, 0, R2)
	BF(R2, . + 8)
	MULC(R0, -1, R0)
	MOD(R0, R1, R2)
	ADDC(R2, 1, R2)							| R2 <- nb of the neighbour chosen randomly
	MULC(R2, -4, R2)
	ADD(SP, R2, R2)
	LD(R2, 0, chos_neigh)
	SUBC(nb_val_n, 1, nb_val_n)

	LD(SP, -4, R0)							|; Deletion of the chosen neighbour cell 
	ST(R0, 0, R2)							|; on the stack
	DEALLOCATE(1)						

	PUSH(R3)								| Arg. 2 <- bitmap "visited"
	PUSH(R5)								| Arg. 1 <- chosen neighbour cell
	CALL(is_visited__, 2)
	BT(R0, build_maze_loop)
	LD(BP, -20, R2)							
	PUSH(R2)								| Arg. 4 <- nb_cols
	PUSH(R5)								| Arg. 3 <- chosen neighbour cell
	PUSH(R4)								| Arg. 2 <- curr_cell
	LD(BP, -12, R0)
	PUSH(R0)								| Arg. 1 <- maze
	CALL(connect__, 4)

	PUSH(R5)								| Arg. 5 <- chosen neighbour cell
	PUSH(R3)								| Arg. 4 <- bitmap "visited"
	PUSH(R2)								| Arg. 3 <- nb_cols
	LD(BP, -16, R5)
	PUSH(R5)								| Arg. 2 <- nb_rows
	PUSH(R0)								| Arg. 1 <- maze
	CALL(perfect_maze, 5)

	CMPLEC(R1, 0, R5)
	BEQ(R5, build_maze_loop)


|; End of the perfect maze function
|; 		-> put back the initial values of the registers in the corresponding registers and finish the stack

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


|; change_to_visited:
|; 		-> Marks the cell curr_cell as visited in the bitmap.
|;		-> Side effects : values contained in R0, R1 and R5 are modified.

change_to_visited:
	add_to_update = R1
	w_to_update = R5

	DIVC(cur_cell, 32, R1)
	MULC(R1, 4, R1)
	ADD(visited, R1, add_to_update)
	CMOVE(1, R0)
	MODC(cur_cell, 32, R5)
	SHL(R0, R5, R0)
	LD(add_to_update, 0, w_to_update)
	OR(w_to_update, R0, w_to_update)
	ST(w_to_update, 0, add_to_update)
	JMP(LP)


|; is_visited__(curr_cell, bitmap)
|;  		-> Returns 1 if curr_cell has already been visited, 0 otherwise.
|;  		-> Side effects: modifies the value contained in R0, R1, R2 and R3.

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


|; End of the is_visited funtion 
|; 		-> put back the initial values of the registers in the corresponding registers

is_visited_end:
	POP(R3)
	POP(R2)
	POP(R1)
	POP(BP)
	POP(LP)
	RTN()


|; Connect function
|; 		-> Opens a vertical or horizontal connection between 2 cells
|; 		-> Side effects : the values contained in R1, R2 and R3 are modified

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

	cell1 = R2
	cell2 = R3
	nb_cols = R1

	CMPLT(cell1, cell2, R0)						
	BT(R0, cell1_less_than_cell2)			| If R2 < R3, go to the function cell1_less_than_cell2
	SWAP(cell1, cell2) 						| If it is not, swap the values


|; Cell1 less than cell2 function
|; 		-> Opens a vertical or a horizontal connection in function of cell1 and cell2 
|; 		-> Side effects : the values contained in R0 and R4 are modified

cell1_less_than_cell2:									
	DIV(cell2, nb_cols, R0)
	MULC(R0, WORDS_PER_ROW, R0)				| R0 <- row_offset of cell contained in cell2
	MOD(cell1, nb_cols, R4)					| R4 <- col of cell contained in cell1
	PUSH(R4)
	DIVC(R4, CELLS_PER_WORD, R4)			| R4 <- word_offset_in_line of cell contained in cell1
	ADD(R4, R0, R0)
	POP(R4)									| R4 <- source_col
	PUSH(R0)								| Loc. var. 1: word_offset of the cell to modify

	MODC(R4, CELLS_PER_WORD, R0)			| R0 <- byte_offset 
	PUSH(R0)								| Loc. var. 2: byte_offset

	LD(BP, -12, nb_cols)					| First word of the maze
	SUB(cell2, cell1, R0)
	SUBC(R0, 1, R0)
	BEQ(R0, connect_hor)
	BNE(R0, connect_ver)


|; Connect hor function
|; 		-> Opens a horizontal connection
|; 		-> Side effects : the values contained in R0, R1, R2 and R3 are modified

connect_hor:
	CMOVE(0xFFFFFF00, R0)
	BR(mask_generator, LP)					| R0 <- mask

	POP(R3)									| R3 <- word_offset
	CMOVE(3, R2)
	MULC(R2, WORDS_PER_MEM_LINE, R2)
	ADD(R3, R2, R3)							| R3 <- word_offset + 3 * WORDS_PER_MEM_LINE
	MULC(R3, BYTES_PER_WORD, R3)			| Word offset to byte offset

|; Update of the four words :

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


|; Connect ver function
|; 		-> Opens a vertical connection
|; 		-> Side effects : the values contained in R0, R1 and R3 are modified

connect_ver:
	CMOVE(0xFFFFFFE1, R0)
	BR(mask_generator, LP)					| R0 <- mask

	POP(R3)									| R3 <- word_offset
	MULC(R3, BYTES_PER_WORD, R3)			| Word offset to byte offset

| Update of the two words :

	ADD(R1, R3, R1)							| R1 <- address of the first word to change
	BR(word_change, LP)						| First word changed

	CMOVE(BYTES_PER_WORD, R3)
	MULC(R3, WORDS_PER_MEM_LINE, R3)
	ADD(R1, R3, R1)							| R1 <- address of the second word to change
	BR(word_change, LP)						| Second word changed

	BR(connect_end)


|; Mask generator function
|; 		-> Put in R0 the correct mask that we need in function of R3 which contains byte_offset
|; 		-> Side effects : the values contained in R0, R2 and R3 are modified


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


|; End of the connect function
|; 		-> put back the initial values of the registers in the corresponding registers and finish the stack

connect_end:
	POP(R4)
	POP(R3)
	POP(R2)
	POP(R1)
	POP(R0)
	POP(BP)
	POP(LP)
	RTN()
