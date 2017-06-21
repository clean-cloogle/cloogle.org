definition module Memory

MCL_CURRENT :== 1
MCL_FUTURE  :== 2

mlockall :: !Int !*World -> *(!Bool, !*World)
