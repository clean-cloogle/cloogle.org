implementation module SimpleTCPServer

import TCPIP
import StdEnv
import StdMaybe

instance zero (Logger a b s) where zero = \_ _ w -> (undef, w)

:: *Channel s = { schan :: *TCP_SChannel
                , rchan :: *TCP_RChannel
                , state :: s
                }

serve :: (a *World -> *(b,*World)) (Maybe (Logger a b s)) Port *World -> *World | fromString a & toString b
serve f log port w
# (ok, mbListener, w) = openTCP_Listener port w
| not ok = abort ("Couldn't open port " +++ toString port)
# listener = fromJust mbListener
# log = if (isNothing log) zero (fromJust log)
= handle f log listener [] w
where
	handle :: (a *World -> *(b,*World)) (Logger a b s)
	          !*TCP_Listener ![*Channel s] !*World
	       -> *World | fromString a & toString b
	handle f log li chans w
	// Wait for event
	# (schans,rchans,states) = unzipchans chans
	# glue = TCP_Pair (TCP_Listeners [li]) (TCP_RChannels rchans)
	# ([(who,what):_],glue,_,w)
		= selectChannel_MT Nothing glue TCP_Void w
	# (TCP_Pair (TCP_Listeners [li:_]) (TCP_RChannels rchans)) = glue
	# chans = zipchans schans rchans states

	// Event on li: new connection
	| who == 0
		# (tRep,mbNewChan,li,w) = receive_MT (Just 0) li w
		| tRep <> TR_Success
			= handle f log li chans w
		# (Just (ip,dup)) = mbNewChan
		# (s, w) = log (Connected ip) undef w
		# chan = {schan=dup.sChannel,rchan=dup.rChannel,state=s}
		# chans = [chan:chans]
		= handle f log li chans w
	// Event on a TCP_RChannel
	| what == SR_Available // new message
		# ({rchan,schan,state},chans) = select (who-1) chans
		# (msg, rchan, w) = receive rchan w
		# msg = fromString (toString msg)
		# (state, w) = log (Received msg) state w
		# (resp, w) = f msg w
		# (schan, w) = send (toByteSeq (toString resp)) schan w
		# (state, w) = log (Sent resp) state w
		# chans = [{rchan=rchan,schan=schan,state=state}:chans]
		= handle f log li chans w
	| what == SR_EOM // Somebody left
		# ({schan,rchan,state},chans) = select (who-1) chans
		# w = seq [closeChannel schan, closeRChannel rchan] w
		# (_, w) = log Disconnected state w
		= handle f log li chans w
	# w = closeRChannel dupChan.rChannel w
	# w = closeChannel dupChan.sChannel w
	# (s, w) = log Disconnected s w

	unzipchans :: ![Channel a] -> (![TCP_SChannel], ![TCP_RChannel], ![a])
	unzipchans [] = ([], [], [])
	unzipchans [{schan,rchan,state}:chans]
		# (ss, rs, sts) = unzipchans chans
		= ([schan:ss], [rchan:rs], [state:sts])

	zipchans :: ![*TCP_SChannel] ![*TCP_RChannel] ![a] -> [*Channel a]
	zipchans [] [] [] = []
	zipchans [s:ss] [r:rs] [st:sts] = [{schan=s,rchan=r,state=st}:zipchans ss rs sts]

	select :: !Int l:[u:a] -> t:(!u:a, !l:[u:a]), [t <= l, l <= u]
	select 0 [x:xs] = (x, xs)
	select i [x:xs] = let (e,xs`) = select (i-1) xs in (e,[x:xs`])
