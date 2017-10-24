
|; Useful constants

WORDS_PER_MEM_LINE = 8
WORDS_PER_ROW = 64
CELLS_PER_WORD = 4
BYTES_PER_WORD = 4



|; MODC(Ra, CC, Rc) computes Reg[Ra] % CC and store the result in register Rc
|; (Rc should be different from Ra)
.macro MODC(Ra, CC, Rc) 	DIVC(Ra, CC, Rc)  MULC(Rc, CC, Rc)  SUB(Ra, Rc, Rc)

|; SWAP(Ra, Rb) swaps the content of registers Ra and Rb
.macro SWAP(Ra, Rb) 		PUSH(Ra)  MOVE(Rb, Ra)  POP(Rb)



|; perfect_maze(maze, nb_row, nb_col, visited, cur_cell)
|;
|; This function builds recursively the perfect maze in the memory.
|;
|; ARGUMENTS:
|; 		-> maze 	: memory address of the first word of the maze
|;		-> nb_row 	: number of rows of the maze
|;		-> nb_col 	: number of columns of the maze
|;		-> visited 	: memory address of the first word of the bitmap 
|;		-> cur_cell : Number of the cell to which the next cell to be connected to 
|;					  the maze will be contected
|;
|; RETURNS: nothing
|;
|; SIDE-EFFECTS: none

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
	maze = R0								|; Address of the first word of the maze
	col_cur_cell = R0 						|; Column of the current cell
	row_cur_cell = R0 						|; Row of the current cell
	nb_val_n = R1 							|; Number of valid neighbours
	nb_col = R2 							|; Number of columns in the maze
	visited = R3 							|; Address of the first word of the bitmap
	cur_cell = R4 							|; Number of the cell to which the next cell to
											|; be connected to the maze will be contected
	val_neigh = R5 							|; Number of the cell of a valid neighbour
	nb_row = R5 							|; Number of rows in the maze

	BR(change_to_visited, LP)
	CMOVE(0, nb_val_n)
	

|; If valid, the number of the cell of the left neighbour of cur_cell is pushed on the stack.
check_left_neighbour:
	MOD(cur_cell, nb_col, col_cur_cell)
	BEQ(col_cur_cell, check_right_neighbour)
	ADDC(nb_val_n, 1, nb_val_n)
	SUBC(cur_cell, 1, val_neigh)
	PUSH(val_neigh)


|; If valid, the number of the cell of the right neighbour of cur_cell is pushed on the stack.
check_right_neighbour:
	SUBC(nb_col, 1, R5)
	CMPEQ(R5, col_cur_cell, R5)
	BT(R5, check_top_neighbour)
	ADDC(nb_val_n, 1, nb_val_n)
	ADDC(cur_cell, 1, val_neigh)
	PUSH(val_neigh)


|; If valid, the number of the cell of the top neighbour of cur_cell is pushed on the stack.
check_top_neighbour:
	DIV(cur_cell, nb_col, row_cur_cell)
	BEQ(row_cur_cell, check_bottom_neighbour)
	ADDC(nb_val_n, 1, nb_val_n)
	SUB(cur_cell, nb_col, val_neigh)
	PUSH(val_neigh)


|; If valid, the number of the cell of the bottom neighbour of cur_cell is pushed on the stack.
check_bottom_neighbour:
	LD(BP, -16, nb_row)
	SUBC(nb_row, 1, R5)
	CMPEQ(R5, row_cur_cell, R5)
	BT(R5, build_maze_loop)
	ADDC(nb_val_n, 1, nb_val_n)
	ADD(cur_cell, nb_col, val_neigh)
	PUSH(val_neigh)


|; build_maze_loop explores one by one the valid neighbours of cur_cell. It chooses randomly a 
|; neighbour and connects it to the maze if it hasn't been visited yet.
build_maze_loop:
	chos_neigh = R5							|; Number of the cell of the chosen neighbour

	BEQ(nb_val_n, perfect_maze_end) 		|; Exits when all valid neighbours are explored.
	RANDOM()
	PUSH(R0)
	CALL(abs__, 1)
	MOD(R0, nb_val_n, R2)
	ADDC(R2, 1, R2)							| R2 <- nb of the neighbour chosen randomly
	MULC(R2, -4, R2)
	ADD(SP, R2, R2)
	LD(R2, 0, chos_neigh)

	SUBC(nb_val_n, 1, nb_val_n)				|; Removes the chosen neighbour cell from the 
	LD(SP, -4, R0)							|; stack and decrements the number of valid 
	ST(R0, 0, R2)							|; neighbours not yet explored.
	DEALLOCATE(1)						

	PUSH(visited)							|; Arg. 2 <- bitmap "visited"
	PUSH(chos_neigh)						|; Arg. 1 <- chosen neighbour cell
	CALL(is_visited, 2)

	BT(R0, build_maze_loop)

	LD(BP, -20, R2)							
	PUSH(nb_col)							|; Arg. 4 <- nb_cols
	PUSH(chos_neigh)						|; Arg. 3 <- chosen neighbour cell
	PUSH(cur_cell)							|; Arg. 2 <- cur_cell
	LD(BP, -12, R0)
	PUSH(maze)								|; Arg. 1 <- maze
	CALL(connect, 4)

	PUSH(chos_neigh)						|; Arg. 5 <- chosen neighbour cell
	PUSH(visited)							|; Arg. 4 <- bitmap "visited"
	PUSH(nb_col)							|; Arg. 3 <- nb_col
	LD(BP, -16, R5)
	PUSH(nb_row)							|; Arg. 2 <- nb_row
	PUSH(maze)								|; Arg. 1 <- maze
	CALL(perfect_maze, 5)

	CMPLEC(nb_val_n, 0, R5)
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


|; change_to_visited marks the current cell as visited in the bitmap.
|;
|; SIDE-EFFECTS : values contained in R0, R1 and R5 are modified.

change_to_visited:
	add_to_update = R1						|; Memory address of the bitmap word to update
	w_to_update = R5						|; Bitmap word to update

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


|; is_visited(cur_cell, visited)
|;
|; This function checks whether cur_cell is already connected to the maze or not.
|;
|; ARGUMENTS:
|;		-> cur_cell : number of the potential next cell to be connected to the maze
|;		-> visited 	: memory address of the first word of the bitmap 
|;
|; RETURNS: 1 if curr_cell has already been visited, 0 otherwise (in register R0).
|:
|; SIDE-EFFECTS: Value contained in R0 is modified.

is_visited:
	PUSH(LP)
	PUSH(BP)
	MOVE(SP, BP)
	PUSH(R2)
	PUSH(R3)
	PUSH(R4)
	
	w_to_check = R2							|; Bitmap word to check
	add_w_to_check = R3						|; Address of the bitmap word to check

	CMOVE(0, R0)
	LD(BP, -12, cur_cell)
	DIVC(cur_cell, 32, R2)
	MULC(R2, BYTES_PER_WORD, R2)			|; Word offset to byte offset for bitmap
	LD(BP, -16, visited)
	ADD(visited, R2, add_w_to_check)
	PUSH(add_w_to_check)
	CMOVE(1, R3)
	MODC(cur_cell, 32, R2)
	SHL(R3, R2, R4)
	POP(add_w_to_check)
	LD(add_w_to_check, 0, w_to_check)
	AND(w_to_check, R4, R2)					|; Isolates the bit of the bitmap corresponding
											|; to cur_cell
	BF(R2, is_visited_end)
	CMOVE(1, R0)

is_visited_end:
	POP(R4)
	POP(R3)
	POP(R2)
	POP(BP)
	POP(LP)
	RTN()



|; connect(maze, cell1, cell2, nb_cols)
|;
|; This function connects the cells cell1 and cell2. After its execution, cell1 and cell2
|; both belong to the maze.
|;
|; ARGUMENTS:
|; 		-> maze 	: memory address of the first word of the maze
|;		-> cell1 	: number of one of the cells to connect
|;		-> cell2 	: number of the other cell to connect
|;		-> nb_cols 	: number of columns of the maze
|;
|; RETURNS: nothing
|:
|; SIDE-EFFECTS: none

connect:
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
	LD(BP, -24, R1)							| R1 <- nb_col

	byte_off = R0							|; Byte offset of the cell to modify
	w_off = R0								|; Word offset of the cell to modify
	nb_cols = R1
	cell1 = R2
	cell2 = R3
	col_cell1 = R4

	CMPLT(cell1, cell2, R0)					|; Swaps cell1 and cell2 if cell2 < cell1	
	BT(R0, cell1_less_than_cell2)
	SWAP(cell1, cell2)

cell1_less_than_cell2:									
	DIV(cell2, nb_cols, R0)
	MULC(R0, WORDS_PER_ROW, R0)				|; R0 <- row_offset of cell cell2
	MOD(cell1, nb_cols, col_cell1)
	PUSH(col_cell1)
	DIVC(col_cell1, CELLS_PER_WORD, R4)		| R4 <- word_offset_in_line of cell cell1
	ADD(R4, R0, w_off)
	POP(R4)									| R4 <- col_cell1
	PUSH(w_off)								| Loc. var. 1: w_off

	MODC(col_cell1, CELLS_PER_WORD, byte_off)			| R0 <- byte_offset 
	PUSH(byte_off)								| Loc. var. 2: byte_offset

	LD(BP, -12, nb_cols)					| First word of the maze
	SUB(cell2, cell1, R0)
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



|; mask_generator creates creates the appropriate mask in order to perform either a 
|; vertical or a horizontal connection. The mask is saved in register R0.
|;
|; SIDE-EFFECTS : values contained in R0, R2 and R3 are modified.

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



|; word_change creates the connection by performing a AND opperation between the mask
|; and an apropriate memory word.
|;
|; SIDE-EFFECTS : value contained in R2 is modified.

word_change:
	LD(R1, 0, R2)
	AND(R2, R0, R2)
	ST(R2, 0, R1)							|; Stores the modified word in memory
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
