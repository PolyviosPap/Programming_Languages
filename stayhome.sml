(****************************************
* Programming Languages, 2019-2020		*
* Set 2, Exercise 3: stayhome			*
* Language: smlNJ						*
* Polyvios Papakonstantinou 03114892	*
****************************************)

val sQueue = Queue.mkQueue () : (int * int * string list) Queue.queue
val tmpSQueue = Queue.mkQueue () : (int * int * string list) Queue.queue
val vQueue = Queue.mkQueue () : (int * int * int) Queue.queue
val tmpVQueue = Queue.mkQueue () : (int * int * int) Queue.queue
val aQueue = Queue.mkQueue () : (int * int) Queue.queue

fun reverse (nil) = nil
	|reverse (x::xs) = (reverse (xs)) @ [x];

(*A function to count the columns and rows of our file.*)
fun count (file) =
	let
		(*Open the file.*)
		val input = TextIO.openIn file

		fun cntColumns (input: TextIO.instream) count =
			let
				val character = TextIO.input1 (input)

				fun cntColumnsInn (copt : char option) =
					case copt of
						(*If we reached the end of the file...*)
						NONE => (TextIO.closeIn (input); count)
						| SOME(c) => (if ((Char.toString c) = "\\n") then count
										else cntColumns (input) (count + 1)
									)
			in
				cntColumnsInn (character)
			end

		fun cntRows (input : TextIO.instream) =
			let
				fun cntRowsInn (input : TextIO.instream) count =
					let
						val row = TextIO.inputLine (input)

						fun helper (copt : string option) =
							case copt of
								NONE => count
								| SOME(c) => (cntRowsInn (input) (count + 1))

						in
							helper(row)
						end
			in
				cntRowsInn (input) 1
			end

	in
		((cntColumns input 0), (cntRows input))
	end

fun createGrid (file, rows, columns) = 
  let
    (*Initialise our grid.*)
    val grid = Array2.array (rows, columns, "X")
    (*Open the file.*)
    val input = TextIO.openIn file

    fun fillGrid ((input : TextIO.instream), index) =
      let
        (*Read one character.*)
        val character = TextIO.input1 (input)

        fun helper (copt : char option) =
          case copt of
            (*We reached the end of the file, close it and return the grid.*)
            NONE => (
              TextIO.closeIn(input);
              (grid)
            )
            | SOME(c) =>
              (if ((Char.toString c) = "\\n") then (fillGrid (input, (index + 2)))
                else (
                  if ((Char.toString c) = "S") then (
                    Array2.update (grid, (index div columns), (index mod columns), (Char.toString c));
                    Queue.enqueue (sQueue, ((index div columns), (index mod columns), []));
                    fillGrid (input, (index + 1))
                  )
                  else if ((Char.toString c) = "W") then (
                    Array2.update (grid, (index div columns), (index mod columns), (Char.toString c));
                    Queue.enqueue (vQueue, ((index div columns), (index mod columns), 0));
                    fillGrid (input, (index + 1))
                  )
                  else if ((Char.toString c) = "A") then (
                    Array2.update (grid, (index div columns), (index mod columns), (Char.toString c));
                    Queue.enqueue (aQueue, ((index div columns), (index mod columns)));
                    fillGrid (input, (index + 1))
                  )
                  else (
                    Array2.update (grid, (index div columns), (index mod columns), (Char.toString c));
                    fillGrid (input, (index + 1))
                  )
                )
              )
      in
        helper (character)
      end
  in
    fillGrid (input, (columns + 1))
  end

(*A function for placing a queue in another of the same type.*)
fun moveElements (q1, q2) =
	let
		fun helper (q1, q2) =
			let
				val tmpEl = Queue.dequeue q2
			in
				Queue.enqueue (q1, tmpEl);

				if (not (Queue.isEmpty q2)) then (
					helper (q1, q2)
				) else (
					()
				)
			end;
	in
		if (Queue.isEmpty q1) then (
		) else (
			Queue.clear q1
		);

		if (Queue.isEmpty q2) then (
		) else (
			helper (q1, q2)
		)
	end

(*A function that returns the last element of a queue.*)
fun lastElement (q1) =
	let
		val tmpEl = Queue.dequeue q1
	in
		if (Queue.isEmpty q1) then (
			tmpEl
		) else (
			lastElement (q1)
		)
	end

fun mainLoop (grid : string Array2.array, time : int, arrTime : int) =
	let
		fun arriveToAirports () =
			if (Queue.isEmpty aQueue) then (
			) else (
				let
					val tmpEl : int * int = Queue.dequeue aQueue
				in
					Array2.update (grid, #1 tmpEl, #2 tmpEl, "W");
					Queue.enqueue (vQueue, (#1 tmpEl, #2 tmpEl, time));
					arriveToAirports ()
				end
			);

		fun moveSotiris() =
			(*If we moved all Sotiris' blocks...*)
			if (Queue.isEmpty sQueue) then (
				moveElements (sQueue, tmpSQueue)
			) else (
				let
					(*Take the first element of Sotiris' queue.*)
					val tmpEl : int * int * string list = Queue.dequeue sQueue
				in
					if (Array2.sub (grid, #1 tmpEl, #2 tmpEl) <> "W") then (
						(*Down*)
						if ((Array2.sub (grid, (#1 tmpEl) + 1, #2 tmpEl) = ".") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, (#1 tmpEl) + 1, #2 tmpEl, "S");
							Array2.update (grid, 0, 0, "0");
							Queue.enqueue (tmpSQueue, ((#1 tmpEl) + 1, #2 tmpEl, "D"::(#3 tmpEl)))
						) else ();

						if ((Array2.sub (grid, (#1 tmpEl) + 1, #2 tmpEl) = "A") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, 0, 0, "0");
							Queue.enqueue (tmpSQueue, ((#1 tmpEl) + 1, #2 tmpEl, "D"::(#3 tmpEl)))
						) else ();

						if ((Array2.sub (grid, (#1 tmpEl) + 1, #2 tmpEl) = "T") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, 0, 0, "1");
							Queue.enqueue (tmpSQueue, ((#1 tmpEl) + 1, #2 tmpEl, "D"::(#3 tmpEl)))
						) else ();

						(*Left.*)
						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) - 1) = ".") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, #1 tmpEl, (#2 tmpEl) - 1, "S");
							Array2.update (grid, 0, 0, "0");
							Queue.enqueue (tmpSQueue, (#1 tmpEl, (#2 tmpEl) - 1, "L"::(#3 tmpEl)))
						) else ();

						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) - 1) = "A") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, 0, 0, "0");
							Queue.enqueue (tmpSQueue, (#1 tmpEl, (#2 tmpEl) - 1, "L"::(#3 tmpEl)))
						) else ();

						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) - 1) = "T") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, 0, 0, "1");
							Queue.enqueue (tmpSQueue, (#1 tmpEl, (#2 tmpEl) - 1, "L"::(#3 tmpEl)))
						) else ();

						(*Right.*)
						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) + 1) = ".") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, #1 tmpEl, (#2 tmpEl) + 1, "S");
							Array2.update (grid, 0, 0, "0");
							Queue.enqueue (tmpSQueue, (#1 tmpEl, (#2 tmpEl) + 1, "R"::(#3 tmpEl)))
						) else ();

						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) + 1) = "A") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, 0, 0, "0");
							Queue.enqueue (tmpSQueue, (#1 tmpEl, (#2 tmpEl) + 1, "R"::(#3 tmpEl)))
						) else ();

						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) + 1) = "T") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, 0, 0, "1");
							Queue.enqueue (tmpSQueue, (#1 tmpEl, (#2 tmpEl) + 1, "R"::(#3 tmpEl)))
						) else ();

						(*Up.*)
						if ((Array2.sub (grid, (#1 tmpEl) - 1, #2 tmpEl) = ".") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, (#1 tmpEl) - 1, #2 tmpEl, "S");
							Array2.update (grid, 0, 0, "0");
							Queue.enqueue (tmpSQueue, ((#1 tmpEl) - 1, #2 tmpEl, "U"::(#3 tmpEl)))
						) else ();

						if ((Array2.sub (grid, (#1 tmpEl) - 1, #2 tmpEl) = "A") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, 0, 0, "0");
							Queue.enqueue (tmpSQueue, ((#1 tmpEl) - 1, #2 tmpEl, "U"::(#3 tmpEl)))
						) else ();

						if ((Array2.sub (grid, (#1 tmpEl) - 1, #2 tmpEl) = "T") andalso (Array2.sub (grid, 0, 0) <> "1")) then (
							Array2.update (grid, 0, 0, "1");
							Queue.enqueue (tmpSQueue, ((#1 tmpEl) - 1, #2 tmpEl, "U"::(#3 tmpEl)))
						) else ()

					) else ();
				
				if (Array2.sub (grid, 0, 0) <> "1") then (
					moveSotiris ()
				) else ()

				end
			);
		
		fun moveVirus() =
			if (Queue.isEmpty vQueue) then (
				moveElements (vQueue, tmpVQueue)
			) else (
				let
					val tmpEl : int * int * int = Queue.dequeue vQueue
				in
					if ((time - (#3 tmpEl)) mod 2 = 0) then (
						(*Down*)
						if ((Array2.sub (grid, (#1 tmpEl) + 1, #2 tmpEl) = ".") orelse (Array2.sub (grid, (#1 tmpEl) + 1, #2 tmpEl) = "S")) then (
							Array2.update (grid, (#1 tmpEl) + 1, #2 tmpEl, "W");
							Queue.enqueue (tmpVQueue, ((#1 tmpEl) + 1, #2 tmpEl, time))
						) else ();

						if ((Array2.sub (grid, (#1 tmpEl) + 1, #2 tmpEl) = "A") andalso (Array2.sub (grid, 0, 1) = "~1")) then (
							Array2.update (grid, (#1 tmpEl) + 1, #2 tmpEl, "W");
							Queue.enqueue (tmpVQueue, ((#1 tmpEl) + 1, #2 tmpEl, time));
							Array2.update (grid, 0, 1, "1")
						) else ();

						(*Left.*)
						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) - 1) = ".") orelse (Array2.sub (grid, #1 tmpEl, (#2 tmpEl) - 1) = "S")) then (
							Array2.update (grid, #1 tmpEl, (#2 tmpEl) - 1, "W");
							Queue.enqueue (tmpVQueue, (#1 tmpEl, (#2 tmpEl) - 1, time))
						) else ();

						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) - 1) = "A") andalso (Array2.sub (grid, 0, 1) = "~1")) then (
							Array2.update (grid, #1 tmpEl, (#2 tmpEl) - 1, "W");
							Queue.enqueue (tmpVQueue, (#1 tmpEl, (#2 tmpEl) - 1, time));
							Array2.update (grid, 0, 1, "1")
						) else ();

						(*Right.*)
						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) + 1) = ".") orelse (Array2.sub (grid, #1 tmpEl, (#2 tmpEl) + 1) = "S")) then (
							Array2.update (grid, #1 tmpEl, (#2 tmpEl) + 1, "W");
							Queue.enqueue (tmpVQueue, (#1 tmpEl, (#2 tmpEl) + 1, time))
						) else ();

						if ((Array2.sub (grid, #1 tmpEl, (#2 tmpEl) + 1) = "A") andalso (Array2.sub (grid, 0, 1) = "~1")) then (
							Array2.update (grid, #1 tmpEl, (#2 tmpEl) + 1, "W");
							Queue.enqueue (tmpVQueue, (#1 tmpEl, (#2 tmpEl) + 1, time));
							Array2.update (grid, 0, 1, "1")
						) else ();

						(*Up.*)
						if ((Array2.sub (grid, (#1 tmpEl) - 1, #2 tmpEl) = ".") orelse (Array2.sub (grid, (#1 tmpEl) - 1, #2 tmpEl) = "S")) then (
							Array2.update (grid, (#1 tmpEl) - 1, #2 tmpEl, "W");
							Queue.enqueue (tmpVQueue, ((#1 tmpEl) - 1, #2 tmpEl, time))
						) else ();

						if ((Array2.sub (grid, (#1 tmpEl) - 1, #2 tmpEl) = "A") andalso (Array2.sub (grid, 0, 1) = "~1")) then (
							Array2.update (grid, (#1 tmpEl) - 1, #2 tmpEl, "W");
							Queue.enqueue (tmpVQueue, ((#1 tmpEl) - 1, #2 tmpEl, time));
							Array2.update (grid, 0, 1, "1")
						) else ()
					) else (
						Queue.enqueue (tmpVQueue, tmpEl)
					);

					moveVirus ()
				end
			)

	in
		moveSotiris ();

		if (Array2.sub (grid, 0, 0) = "1") then (
			let
				val tmpEl = lastElement (tmpSQueue)
				val ans = reverse (#3 tmpEl)
			in
				print (Int.toString(time) ^ "\n" ^ String.concatWith "" ans ^ "\n")
			end

		) else if (Array2.sub (grid, 0, 0) = "~1") then (
			print ("IMPOSSIBLE\n")
		) else (
			if (Queue.isEmpty vQueue) then (
			) else (
				moveVirus ()
			);
			
			if (time = arrTime) then (
				arriveToAirports ()
			) else ();

			Array2.update (grid, 0, 0, "~1");
			
			if (Array2.sub (grid, 0, 1) = "1") then (
				Array2.update(grid, 0, 1, "X");
				mainLoop (grid, (time + 1), (time + 5))
			) else (
				mainLoop (grid, (time + 1), arrTime)
			)
		)
	end


fun stayhome file =
	let
		val counted = count (file)
		(*We will add 'X's surrounding the grid for borders, so add 2 columns and 2 rows.*)
		val columns = (#1 counted) + 2
		val rows = (#2 counted) + 2
		val grid = createGrid (file, rows, columns)
	in
		(*grid[0][0] determines whether or not we can move any of Sotiris' block and if we've found a solution.*)
		Array2.update (grid, 0, 0, "~1");
		(*grid[0][1] determines whether or not the virus has reached to an airport yet.*)
		Array2.update (grid, 0, 1, "~1");

		mainLoop (grid, 1, 0);

		Queue.clear sQueue;
		Queue.clear tmpSQueue;
		Queue.clear vQueue;
		Queue.clear tmpVQueue;
		Queue.clear aQueue
	end