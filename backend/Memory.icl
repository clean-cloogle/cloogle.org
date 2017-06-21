implementation module Memory

import StdInt

mlockall :: !Int !*World -> *(!Bool, !*World)
mlockall flags w
# (res,w) = lock flags w
= (res == 0, w)
where
	lock :: !Int !*World -> *(!Int, !*World)
	lock flags w = code {
		ccall mlockall "I:I:A"
	}
