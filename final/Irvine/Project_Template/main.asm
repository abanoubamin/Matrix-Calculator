INCLUDE Irvine32.inc
.DATA
; data of heap allocation 

ARRAY_SIZE = 1000
HEAP_START = 2000000
HEAP_MAX = 400000000
BLOCK_SIZE = 500000
pArray SDWORD ? ; pointer to block of memory
hHeap HANDLE ? ; handle to the process heap
dwFlags DWORD HEAP_ZERO_MEMORY ; set memory bytes to all zeros
str11 BYTE "Cannot handle or allocate heap memory!",0


MAX_MATRIX_SIZE EQU 10
space BYTE "    ",0
str00 BYTE "Welcome to Matrix Calculator",0
str0 BYTE "Please select a choice:",0
str1 BYTE "1 - Matrix Addition",0
str2 BYTE "2 - Matrix Subtraction",0
str3 BYTE "3 - Multiplying a Matrix by a Constant",0
str4 BYTE "4 - Multiplying Matrices",0
str5 BYTE "5 - Transposing a Matrix",0
str6 BYTE "6 - Determinant of Matrix 4*4",0 
str7 BYTE "Enter your choice:  ",0
str8 BYTE "Enter Multiplier:  ",0
str9 BYTE "Multiplication Error: ",0
str10 BYTE"Enter Matrix Elements: ",0
str22 BYTE "Enter dimensions: ",0
str_error BYTE "Invalid choice !",0
str_invalid BYTE "Invalid Dimension!",0
str_dimension BYTE "Enter the dimension of the matrix:",0
str_First_Dimention BYTE "Enter the dimension of the First Matrix:",0
str_Second_Dimention BYTE "Enter the dimension of the Second Matrix:",0
Output BYTE "OUTPUT:",0

M BYTE ?
N BYTE ?
MN BYTE ?
temp1 DWORD ?
TEMP2 DWORD ?
MATRIX1_INDEX DWORD 0
MATRIX2_INDEX DWORD 0
SUM sDWORD 0
COL_M1 BYTE ?

str_elem_st BYTE "Enter the elements of the first matrix:",0
Matrix1 SDWORD MAX_MATRIX_SIZE DUP(MAX_MATRIX_SIZE DUP(?))
str_elem_nd BYTE "Enter the elements of the Second matrix:",0
Matrix2 SDWORD MAX_MATRIX_SIZE DUP(MAX_MATRIX_SIZE DUP(?))
Matrix3 SDWORD MAX_MATRIX_SIZE DUP(MAX_MATRIX_SIZE DUP(?))
str_elem BYTE "Enter the elements of the matrix:",0
str_const BYTE "Enter the constant:",0
multiplier	SDWORD	?




table	SBYTE 2, 5 ,-3 , -2
		SBYTE -2, -3, 2 ,-5
		SBYTE 1, 3 ,-2 , 0
		SBYTE -1, -6, 4, 0
		
		

col1 dword 0
col2 dword 0
row  dword 0
temprow dword 0

startRow dword 0
startCol dword 0

skipcol dword 100
currentcol dword 0
limit dword 2
cofactor sdword 0
sign sdword 1

startRowdet4 dword 0
startColdet4 dword 0
currentcoldet4 dword 0
cofactord4 dword 0
signd4 dword 1
skipcoldet4 dword 100

determinant sdword 0
smalldeterminant sdword 0
middeterminant sdword 0
NumCols = 4
NumRows = 4


.CODE
main PROC
 
    INVOKE HeapCreate, 0,HEAP_START, HEAP_MAX
	CMP EAX , NULL
    JE quit
	mov hHeap,eax
    INVOKE HeapAlloc, hHeap, HEAP_ZERO_MEMORY, ARRAY_SIZE
	CMP EAX , NULL
    JE quit
    mov pArray,eax 

	INVOKE HeapAlloc, hHeap, HEAP_ZERO_MEMORY, ARRAY_SIZE
	CMP EAX , NULL
    JE quit
    mov Matrix1,eax 
	MOV ecx,ARRAY_SIZE
    MOV esi,Matrix1

	INVOKE HeapAlloc, hHeap, HEAP_ZERO_MEMORY, ARRAY_SIZE
	CMP EAX , NULL
    JE quit
    mov Matrix2,eax 
	MOV ecx,ARRAY_SIZE
    MOV esi,Matrix2

	INVOKE HeapAlloc, hHeap, HEAP_ZERO_MEMORY, ARRAY_SIZE
	CMP EAX , NULL
    JE quit
    mov Matrix3,eax 
	MOV ecx,ARRAY_SIZE
    MOV esi,Matrix3



	
	MOV EDX, OFFSET str00
	CALL WriteString
	CALL CRLF

	MOV EDX, OFFSET str0
	CALL WriteString
	CALL CRLF

	MOV EDX, OFFSET str1
	CALL WriteString
	CALL CRLF

	MOV EDX, OFFSET str2
	CALL WriteString
	CALL CRLF

	MOV EDX, OFFSET str3
	CALL WriteString
	CALL CRLF

	MOV EDX, OFFSET str4
	CALL WriteString
	CALL CRLF

	MOV EDX, OFFSET str5
	CALL WriteString
	CALL CRLF

	MOV EDX, OFFSET str6
	CALL WriteString
	CALL CRLF

	MOV EDX, OFFSET str7
	CALL WriteString

	CALL ReadDec
	
	CMP AL, 1
	JNE N1
	CALL Matrix_Addition
	JMP E

	N1:
	CMP AL, 2
	JNE N2
	CALL Matrix_Subtraction
	JMP E

	N2:
	CMP AL, 3
	JNE N3
	CALL Multiplying_Matrix_by_Constant
	JMP E

	N3:
	CMP AL, 4
	JNE N4
	CALL Multiplying_Matrices
	JMP E

	N4:
	CMP al, 5
	jne N5
	call Transposing
	JMP E

	N5:
	CMP AL, 6
	JNE N6
	call det4
    mov eax, determinant
    call writeint
	call crlf
	JMP E
	N6:
	MOV EDX, OFFSET str_error
	CALL WriteString
	CALL CRLF
	INVOKE HeapFree, hHeap, 0, pArray
	JMP E1
	E:
	jmp E1
	quit:
	MOV EDX,OFFSET str11
	CALL WRITESTRING

	E1:
	;INVOKE HeapFree, hHeap, 0, pArray
	;INVOKE HeapDestroy, hHeap
	EXIT
main ENDP


;-----------------------------------------------------------------------------------
;Reads dimensions of matrix from user
;Returns: ECX Contains the multiplication of the dimensions (length of the arrays)				   
;-----------------------------------------------------------------------------------
Enter_Dimensions PROC USES EAX
    CALL CRLF
	CALL ReadDec
	MOV M, AL
	CALL ReadDec
	MOV N, AL

	MOV AL, 0
	MOV CL, N
	L0:
		ADD AL, M
	LOOP L0
	MOV MN, AL
	MOV CL, MN
	
	RET
Enter_Dimensions ENDP

;-----------------------------------------------------------------------------------
;Reads an integer array from user (First Matrix)
;Receives: ESI Contains the offset of the array
;          ECX Contains the length of the array 
;-----------------------------------------------------------------------------------
Read_First_Matrix PROC USES ECX ESI EAX

	L1:
		CALL ReadInt
		MOV [ESI], EAX
		ADD ESI, TYPE Matrix1
	LOOP L1

	RET
Read_First_Matrix ENDP

;-----------------------------------------------------------------------------------
;Reads an integer array from user (Second Matrix)
;Receives: ESI Contains the offset of the array
;          ECX Contains the length of the array 
;-----------------------------------------------------------------------------------
Read_Second_Matrix PROC USES ECX ESI EAX

	L1:
		CALL ReadInt
		MOV [ESI], EAX
		ADD ESI, TYPE Matrix2
	LOOP L1

	RET
Read_Second_Matrix ENDP

;-----------------------------------------------------------------------------------
;Displays an integer array to console (Third Matrix)
;Receives: ESI Contains the offset of the array 
;-----------------------------------------------------------------------------------
Display_Third_Matrix PROC USES ECX ESI EAX EDX

	MOV CL, M
	L3:
		PUSH ECX
		MOV CL, N
		L4:
			MOV EAX, [ESI]
			CALL WriteInt
			ADD ESI, TYPE Matrix3
			MOV EDX, offset space
			CALL WriteString
		LOOP L4
		
		POP ECX
		CALL CRLF
	LOOP L3
	CALL CRLF
		
	RET
Display_Third_Matrix ENDP

;-----------------------------------------------------------------------------------
;Calculates: Addition of two integer arrays (Matrix1 + Matrix1) into (Matrix3)
;Receives: ECX Contains the length of the array (Third Matrix)
;-----------------------------------------------------------------------------------
Matrix_Addition PROC USES EDX ECX ESI EAX
  
	MOV EDX, OFFSET str_dimension
	CALL WriteString
	CALL CRLF

	CALL Enter_Dimensions

	MOV EDX, OFFSET str_elem_st
	CALL WriteString
	CALL CRLF

	MOV ESI, OFFSET Matrix1
	CALL Read_First_Matrix

	MOV EDX, OFFSET str_elem_nd
	CALL WriteString
	CALL CRLF

	MOV ESI, OFFSET Matrix2
	CALL Read_Second_Matrix

	MOV ESI, 0
	L:
		MOV EAX, Matrix1[ESI]
		MOV Matrix3[ESI], EAX
		MOV EAX, Matrix2[ESI]
		ADD Matrix3[ESI], EAX
		ADD ESI, TYPE Matrix3
	LOOP L

	CALL CRLF
	
	MOV ESI, OFFSET Matrix3
	CALL Display_Third_Matrix

	RET
Matrix_Addition ENDP

;-----------------------------------------------------------------------------------
;Calculates: Subtraction of two integer arrays (Matrix1 - Matrix1) into (Matrix3)
;Receives: ECX Contains the length of the array (Third Matrix)
;-----------------------------------------------------------------------------------
Matrix_Subtraction PROC USES EDX ECX ESI EAX

	MOV EDX, OFFSET str_dimension
	CALL WriteString
	CALL CRLF

	CALL Enter_Dimensions

	MOV EDX, OFFSET str_elem_st
	CALL WriteString
	CALL CRLF

	MOV ESI, OFFSET Matrix1
	CALL Read_First_Matrix

	MOV EDX, OFFSET str_elem_nd
	CALL WriteString
	CALL CRLF

	MOV ESI, OFFSET Matrix2
	CALL Read_Second_Matrix

	MOV ESI, 0
	L:
		MOV EAX, Matrix1[ESI]
		MOV Matrix3[ESI], EAX
		MOV EAX, Matrix2[ESI]
		SUB Matrix3[ESI], EAX
		ADD ESI, TYPE Matrix3
	LOOP L

	CALL CRLF
	
	MOV ESI, OFFSET Matrix3
	CALL Display_Third_Matrix

	RET
Matrix_Subtraction ENDP

;-----------------------------------------------------------------------------------
;Calculates: Multiplication of constant with matrix and generates another matrix with each
;element is element in the original matrix multiplied with that constant"Multiplier"
;Receives: ECX Contains the length of the array
;-----------------------------------------------------------------------------------

Multiplying_Matrix_by_Constant PROC USES EDX ECX ESI EAX

    MOV EDX , OFFSET str22
	CALL WRITESTRING
    CALL Enter_Dimensions

    CALL get_matrix_multipl
	CALL get_number_to_mul

	mov EBX,multiplier
	MOV ESI, 0
	
		L:
		  MOV EAX, pArray[ESI]
		  IMUL multiplier
		  JO Mul_Error
		  MOV pArray[ESI], EAX
		  ADD ESI, TYPE pArray
	    LOOP L
	CALL CRLF
	MOV ESI, OFFSET pArray
   MOV CL, M
	L3:
		PUSH ECX
		MOV CL, N
		L4:
			MOV EAX, [ESI]
			CALL WriteInt
			ADD ESI, TYPE pArray
			MOV EDX, offset space
			CALL WriteString
		LOOP L4
		
		POP ECX
		CALL CRLF
	LOOP L3
	CALL CRLF
	JMP Skip
	Mul_Error:
	MOV EDX,OFFSET str9
	Call writestring
	Skip:
	RET
Multiplying_Matrix_by_Constant ENDP

;-----------------------------------------------------------------------------------
;Calculates: No calculated value
;Receives:   No recived arguments
;Fill Multiplier variable with specific value entered by the user
;-----------------------------------------------------------------------------------

get_number_to_mul PROC USES EDX EAX
  mov EDX,OFFSET str8
  Call writestring
  call readint
  mov multiplier,eax
  RET
get_number_to_mul ENDP 

;-----------------------------------------------------------------------------------
;Calculates: No calculated value
;Receives:   ECX With dimensions
;Fill matrix pArray with data which will be used for multiplication with constant and transposing a matrix
;-----------------------------------------------------------------------------------
get_matrix_multipl PROC USES ECX ESI EDX EAX
MOV EDX,OFFSET str10
CALL WRITESTRING
CALL CRLF
MOV ESI,OFFSET pArray
L:
CALL readint
MOV [ESI],EAX
ADD ESI,TYPE pArray
LOOP L
RET
get_matrix_multipl ENDP
;-----------------------------------------------------------------------------------
;Calculates: transpose a matrix by turning all the rows of a given matrix into columns and vice-versa
;Receives:   No recived arguments
;Returns : ESI contain the offset of the pArray
;-----------------------------------------------------------------------------------
Transposing PROC uses eax ebx ecx edx ebp esi edi

MOV EDX , OFFSET str22
	CALL WRITESTRING
    CALL Enter_Dimensions
	movzx ecx,MN
	call get_matrix_multipl 




	movzx ecx,N
	mov esi,0
	movzx eax,N
	mov edx,4
	imul edx
	L1:
		mov ebp, ecx
		movzx ecx,M
		mov edi,esi
		L2:
			mov ebx, pArray[esi]
			push ebx
			add esi,eax
		loop L2
		mov esi,edi
		add esi,4
		mov ecx,ebp
	loop L1

	movzx ecx,MN
	movzx eax,MN
	dec eax
	mov edx,4
	imul edx
	L3:
		pop ebx
		mov pArray[eax],ebx
		sub eax,4
	Loop L3
	mov esi, offset pArray
	call Display_Transposed_Matrix
RET
Transposing ENDP
;-----------------------------------------------------------------------------------
;Display transposed matrix
;Receives:   ESI contain the offset of the pArray
;-----------------------------------------------------------------------------------
Display_Transposed_Matrix PROC uses ecx ebp esi edx
movzx ecx,N
	L4:
		mov ebp, ecx
		movzx ecx,M
		L5:
			mov eax,[esi]
			call writeint
			add esi,4
			MOV EDX, offset space
			CALL WriteString
		loop L5
		call crlf
		mov ecx,ebp
	loop L4

	
RET
Display_Transposed_Matrix ENDP
;-----------------------------------------------------------------------------------------------------------------------------
;Multiplying 2 Matrix (Matrix3=Matrix1*Matrix2)
;receives M,N--->Dimensions of the Matrix
;------------------------------------------------------------------------------------------------------------------------------
Multiplying_Matrices Proc 

	MOV EDX, OFFSET str_First_Dimention
	CALL WriteString
	CALL Enter_Dimensions
	MOV EAX,0
	MOV AX,4
	MUL N
	MOV TEMP2,EAX  ;Pointer to the Index OF the ROW of the First Matrix
	MOV AL, N      ;NO.of columns of the First Matrix
	MOV BL,M       ;NO.of Rows of the First Matrix
	MOV COL_M1,AL  ;Copy the NO.of Columns Of the First Matrix
	MOV EDX, OFFSET str_elem_st
	CALL WriteString
	CALL CRLF
	MOV ESI, OFFSET Matrix1
	CALL Read_First_Matrix  ;Fill the First Matrix
	MOV EDX, OFFSET str_Second_Dimention
	CALL WriteString
	CALL Enter_Dimensions
	CMP AL,M
	JNE EX
	MOV EDX, OFFSET str_elem_nd
	CALL WriteString
	CALL CRLF
	MOV ESI,offset Matrix2
	call Read_FIRST_Matrix
	MOV M,BL ;NO.OF Rows of the First Matrix
	MOV EAX,0
	MOV ax,4
	MUL N
	MOV TEMP1,EAX  ;Pointer to the Index OF the ROW of the Second Matrix
	MOVZX CX,BYTE PTR M
	MOV ESI,0
	MOV EBX,0
	BIG:
		PUSH CX
		MOV MATRIX2_INDEX,0
		MOVZX CX,N
		OUTER: 
			  PUSH CX
			  MOV ESI,MATRIX1_INDEX
			  MOV EDI,MATRIX2_INDEX
			  MOVZX CX,COL_M1
			  INNER:
					mov EAX,Matrix2[EDI]
					IMUL Matrix1[ESI]
					ADD EDI,TEMP1
					ADD ESI,4
					ADD SUM,EAX
					LOOP INNER
			ADD MATRIX2_INDEX,4
			MOV EAX,SUM
			MOV MATRIX3[EBX],EAX
			MOV SUM,0
			ADD EBX,4
			POP CX
			LOOP OUTER
		MOV EAX,TEMP2
		ADD MATRIX1_INDEX,EAX
		POP CX
		LOOP BIG
		mov esi,offset Matrix3
		MOV EDX,OFFSET OUTPUT
		CALL WRITESTRING
		CALL CRLF
		CALL Display_Third_Matrix
		jmp ending

ex:mov edx,offset str_invalid
call writestring
call crlf
ending:Ret
Multiplying_Matrices EndP







det2 proc uses eax ebx ecx

;// gets the starting index of the small matrix
mov eax, row;// column index 1 
add eax, 1
mov temprow, eax

mov ebx, NumCols
imul ebx, row
mov esi, col1
movsx EAX, table[ebx + esi]

mov ebx, NumCols
imul ebx, temprow
mov esi, col2
movsx ECX, table[ebx + esi]

imul eax, ecx
mov edx, eax

mov ebx, NumCols
imul ebx, temprow
mov esi, col1
movsx EAX, table[ebx + esi]

mov ebx, NumCols
imul ebx, row
mov esi, col2
movsx ECX, table[ebx + esi]

imul eax, ecx
sub edx, eax

mov smalldeterminant, edx
ret
det2 endp

;--------------------------
;INPUT startRow, startCol
;------------------------ -
det3 proc uses ecx eax ebx edx
mov middeterminant, 0
mov sign, 1
mov ecx, 3
mov edx, startCol
mov currentcol, edx
mov eax, startCol
add eax, 2

mov ebx, startRow	;//send startrow+1 to det2
add ebx, 1
mov row, ebx

L1:
cmp ecx, 0
je skipL1

mov edx, currentcol
cmp edx, skipcol
jne skipadd
add currentcol, 1
skipadd:

mov ebx, NumCols
imul ebx, startRow
mov esi, currentcol
movsx EAX, table[ebx + esi]
mov cofactor, eax


;//if(currentcol = limit)
mov edx, currentcol
cmp edx, limit
je equal

;//if(currentcol= start)

cmp edx, startCol
je equal2

;//else
;//send start
mov ebx, startCol
mov col1, ebx
;//send limit
mov ebx, limit
mov col2, ebx
call det2

jmp calMid


equal2:
;//send start+1 ( if possible )
mov ebx, startCol
add2:
add ebx, 1
cmp ebx, skipcol
je add2
mov col1, ebx
;//send limit
mov ebx, limit
mov col2, ebx
call det2

jmp calMid

equal:

;//send start
mov ebx, startCol
mov col1, ebx

;//send start+1  ( check if it's not skipcol else send start+2)
mov ebx, startCol
add1:
add ebx, 1
cmp ebx, skipcol
je add1
mov col2, ebx
call det2


calMid:
mov eax, cofactor
imul eax, sign
imul eax, smalldeterminant
add middeterminant, eax

skipcolmn:

add currentcol, 1
sub ecx, 1
mov ebx, sign
imul ebx, -1
mov sign, ebx
jmp L1
skipL1:

ret
det3 endp



det4 proc uses ecx ebx edx 
mov ecx, 4
mov edx, startColdet4
mov currentcoldet4 , edx
mov eax, startColdet4
mov ebx, startRowdet4;//send startrow+1 to det2
add ebx, 1
mov startRow, ebx

L2 :
cmp ecx, 0
je skipL2
mov skipcol, 100
mov edx, currentcoldet4
cmp edx, skipcoldet4
jne skipadd
add currentcoldet4, 1
skipadd:

mov ebx, NumCols
imul ebx, startRowdet4
mov esi, currentcoldet4
movsx EAX, table[ebx + esi]
mov cofactord4, eax


;//if(currentcol = lastcol)
mov edx, currentcoldet4
cmp edx, 3
je equald4

;//if(currentcol= start)

cmp edx, startColdet4
je equal2d4

;//if(currentcol= start+1)
mov edi, startColdet4
add edi , 1
cmp edx, edi
je equal3d4

;//else
;//(currentcol= start+2)
;//send start
mov ebx, startColdet4
mov startCol, ebx
;//send limit=3
mov limit, 3
;//skipcol = 2
mov skipcol, 2
call det3

jmp calMidd4


equal2d4 :
;//send start+1 ( if possible )
mov ebx, startColdet4
add ebx, 1
mov startCol, ebx
;//send limit = 3
mov limit, 3
call det3

jmp calMidd4

equal3d4:
;//send start
mov ebx, startColdet4
mov startCol, ebx
;// limit = 3
mov limit, 3
;// skipcol = 1
mov skipcol, 1
call det3
jmp calMidd4

equald4:

;//send start
mov ebx, startColdet4
mov startCol, ebx

;//send limit = 2 
mov limit, 2
call det3

calMidd4:
mov eax, cofactord4
imul eax, signd4
imul eax, middeterminant
add determinant, eax


add currentcoldet4, 1
sub ecx, 1
mov ebx, signd4
imul ebx, -1
mov signd4, ebx
jmp L2
skipL2 :



ret
det4 endp
END main