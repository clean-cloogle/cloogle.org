var fs = require('fs');
var spawn = require('child_process').spawn;
var WebSocketServer = require('websocket').server;

var filename = process.argv[2];
if (!filename)
	return console.log("Usage: node server.js <LOG> [<SSL_CERT> <SSL_KEY>]");

var connections = [];
var connCounter = 0;
var tail = spawn("tail", ["-n", "0", "-f", filename]);
tail.stdout.on('data', function(data){
	if (connections.length == 0)
		return;

	try {
		data = JSON.parse(data);
		var logline = JSON.stringify({'request': data['request']});

		console.log('Outgoing: ' + logline);

		for (var i in connections) {
			try {
				connections[i].sendUTF(logline);
			} catch (e) {
				console.error(e);
			}
		}
	} catch (e) {
		console.error(e);
	}
});

if (process.argv.length >= 5) {
	https = require('https');
	var server = https.createServer({
		cert: fs.readFileSync(process.argv[3]),
		key: fs.readFileSync(process.argv[4])
	}, function(req,res){
		res.writeHead(400);
		res.end();
	});
} else {
	http = require('http');
	var server = http.createServer(function(req,res){
		res.writeHead(400);
		res.end();
	});
}
server.listen(31216);

var ws = new WebSocketServer({
	httpServer: server,
	autoAcceptConnections: false
});

ws.on('request', function(req){
	var con = req.accept('cloogle-stats', req.origin);

	con.cId = connCounter++;
	connections.push(con);
	console.log('Opened connection ' + conn.cId + ' from ' + con.remoteAddress);

	con.on('close', function(reason, desc){
		var i = connections.indexOf(con);
		if (i != -1)
			connections.splice(i, 1);

		console.log('Closed connection ' + connections[i].cId);
	});
});
