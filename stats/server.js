var util = require('util');
var spawn = require('child_process').spawn;
var WebSocketServer = require('websocket').server;

var filename = process.argv[2];
if (!filename)
	return util.puts("Usage: node server.js <LOG>");

http = require('http');
var server = http.createServer(function(req,res){
	res.writeHead(400);
	res.end();
});
server.listen(31216);

var ws = new WebSocketServer({
	httpServer: server,
	autoAcceptConnections: false
});

ws.on('request', function(req){
	var con = req.accept('cloogle-stats', req.origin);
	var tail = spawn("tail", ["-n", "2", "-f", filename]);

	con.on('close', function(reason, desc){
		tail.kill();
	});
	
	tail.stdout.on('data', function(data){
		var match = /<-- (\{.*\})/.exec(data);
		if (match != null) {
			console.log(match[1]);
			con.sendUTF(match[1]);
		}
	});
});
