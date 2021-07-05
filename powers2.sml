(****************************************
* Programming Languages, 2019-2020		*
* Set 1, Exercise 1: powers2			*
* Language: smlNJ						*
* Polyvios Papakonstantinou 03114892	*
****************************************)

fun powers2(input) =
	let
		(*A function that reads the next integer from a file.*)
		fun intFromStream(stream) = Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) stream)
				
		val fStream = TextIO.openIn input;		(*Create a stream by opening the input file...*)
		val T = intFromStream(fStream);		(*... and read its first integer T = "the number of following integers' pairs".*)
		
		(*Two functions for printing all the elements of a given list, in their correct order, inside brackets.*)
		fun printElements([]) = ()
			|printElements(a::[]:IntInf.int list) = print(IntInf.toString (a ))
			|printElements(h::t:IntInf.int list) = (
				print(IntInf.toString (h) ^ ",");
				printElements(t)
			)
			
		fun printList([]) = print("[]\n")
			|printList(h::t:IntInf.int list) = (
				print("[");
				printElements(h::t);
				print("]\n")
			)
		
		(*This function takes a list l and an integer i and fills the list with i elements = 1.*)
		fun initList([], 0) = []
			|initList(h::t:IntInf.int list, 0) = h::t
			|initList([], i:IntInf.int) = initList([1], i-1)
			|initList(h::t:IntInf.int list, i:IntInf.int) = initList(1::h::t, i-1)
		
		(*A simple funtion for (int) math powers, it calculates a^b.*)
		fun pow(_, 0) = 1
			|pow(a:IntInf.int, b:IntInf.int) = a * pow(a, b - 1)
			
		(*A function that returns the ith element of a list.*)
		fun ithEl(0, _) = raise Empty
			|ithEl(_, nil) = raise Empty
			|ithEl(1, h::t:IntInf.int list) = h
			|ithEl(i, h::t:IntInf.int list) = ithEl(i-1, t)
		
		(*Our "main" function for solving the problem.*)
		fun mainFun(0) = ()
			|mainFun(i:IntInf.int) =
				let
					(*
					 * Firstly, read the next pair of integers (N, K) from the file.
					 * Our goal is to determine whether we can represent N with K powers of 2 or not.
					 *)
					val N = intFromStream(fStream);
					val K = intFromStream(fStream);
					
					val tmpFin = initList([], K);		(*Initiliaze the fin list by filling it with 1 'K' times.*)
					
					fun formFin(nil, _, _, _) = printList(nil)
						|formFin(h::t:IntInf.int list, counter:IntInf.int, exp:IntInf.int, tmpk:IntInf.int) =
							if (tmpk = K + 1) then print(IntInf.toString(counter) ^ "]\n")
							else if (tmpk = 0) then (
								print("[");
								formFin(h::t, counter, exp, tmpk + 1)
							)
							else if (pow(2, exp) = ithEl(tmpk, h::t)) then formFin(h::t, counter + 1, exp, tmpk + 1)
							else (
								print(IntInf.toString(counter) ^ ",");
								formFin(h::t, 0, exp + 1, tmpk)
							)
					
					(*
					 *fillFin(j, sum, fin)
					 *"j" is a variable for creating and filling the tmpFin list, in which we will store the integers for the final answer.
					 *"sum" will hold the sum of the powers of 2.
					 *)
					fun fillFin(_, _, nil) = raise Empty
						|fillFin(_, 0, _) = raise Empty
						|fillFin(0, _, _) = printList(nil)		(*It's impossible, therefore print the NULL list.*)
						|fillFin(tmpj:IntInf.int, tmpSum:IntInf.int, tmpList:IntInf.int list) = (
							if (tmpSum = N) then formFin(tmpList, 0, 0, 0)		(*If sum = N, we're almost done, what's left is to print our list in the correct format.*)
							else (
								let
									(*The following functions are for multipying a specific element of a given element by 2.*)
									fun mergeBackList(nil, nil) = nil
										|mergeBackList(nil, h::t:IntInf.int list) = mergeBackList([h], t)
										|mergeBackList(h::t:IntInf.int list, nil) = h::t
										|mergeBackList(finList:IntInf.int list, h::t:IntInf.int list) = mergeBackList(h::finList, t)

									fun multElBy2(_, nil, _) = raise Empty
										|multElBy2(0, _, nil) = raise Empty
										|multElBy2(1, h::t:IntInf.int list, tmpList:IntInf.int list) = mergeBackList((h*2)::t, tmpList)
										|multElBy2(l, h::t:IntInf.int list, tmpList:IntInf.int list) = multElBy2(l-1, t, h::tmpList)
										
									val check = ithEl(tmpj, tmpList);
									
								in
									if (tmpSum + check <= N) then fillFin(tmpj, tmpSum + check, multElBy2(tmpj, tmpList, nil))
									else fillFin(tmpj-1, tmpSum, tmpList)
								end
							)
						)
				in
					fillFin(K, K, tmpFin);
					mainFun(i-1)
				end
	in
		mainFun(T)
	end;