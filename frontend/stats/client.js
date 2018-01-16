var searches = [];
var activity = [];

function timeToString(seconds) {
	if (typeof seconds != 'undefined')
		var date = new Date(seconds * 1000);
	else
		var date = new Date();

	return ((date.getHours() < 10)?"0":"") + date.getHours() +":"+
	  ((date.getMinutes() < 10)?"0":"") + date.getMinutes();
}

function updateSearches() {
	var query = searches[0][0];
	var time = searches[0][1];
	var last = document.getElementById('last-search');

	var highlighter = highlightFunction;
	if (query.substring(0,6) == 'class ') {
		highlighter = highlightClassDef;
	} else if (query.substring(0,5) == 'type ') {
		highlighter = function(q) {
			return '<span class="keyword">type</span> ' +
				highlightFunction(q.substring(5));
		}
	} else if (query.substring(0,6) == 'using ') {
		highlighter = function(q) {
			return '<span class="keyword">using</span> ' +
				highlightToHTML({
					start: [
						[/(,)/,     ['punctuation']],
						[/([^,]+)/, ['funcname']],
					]
				}, q.substring(6));
		}
	}

	last.innerHTML = '<pre>' + highlighter(query) + '</pre>';
	last.innerHTML += '<div class="time">' + time + '</div>';

	var previous = document.getElementById('previous-searches');
	previous.innerHTML = '';
	for (var i in searches) {
		if (i == 0) continue;
		var color = 100 + i * 20;
		var color = 'rgb(' + color + ',' + color + ',' + color + ')';
		previous.innerHTML += '<tt style="color:' + color + ';">' +
				escapeHTML(searches[i][0]) + '</tt><br/>';
	}

	var last = document.getElementById('last-search');
	var fontSize = 70;
	do {
		last.style.fontSize = (--fontSize) + 'px';
	} while (last.scrollWidth > (window.innerWidth || document.body.clientWidth));
}

function updateChart() {
	var data = { title: 'Activity'
	           , yLabel: 'Requests'
	           , dataPoints: activity
	           , minMaxY: 5
	           , yLines: 5
	           };
	var canvas = document.getElementById('activity');
	var context = canvas.getContext('2d');
	context.clearRect(0, 0, canvas.width, canvas.height);
	Chart.render('activity', data);
}

function addRecord(req) {
	var time = timeToString();
	var query = (req.name ? req.name : '') +
		(req.unify ? ' :: ' + req.unify : '');
	if ('className' in req)
		query = 'class ' + req.className;
	else if ('typeName' in req)
		query = 'type ' + req.typeName;
	else if ('using' in req)
		query = 'using ' + req.using.join(', ');

	searches.splice(0, 0, [query, time]);
	if (searches.length > 10)
		searches.splice(searches.length - 1, searches.length);
	updateSearches();

	activity[activity.length - 1].y++;
	updateChart();
}

var open_timer;
function addConnectionCallbacks(connection) {
	connection.onopen = function() {
		console.log('Connection open');
		window.clearInterval(open_timer);
	};

	var tryConnect = function() {
		if (typeof connection.close != 'undefined')
			connection.close();
		console.log('Attempting connection...');
		connection = new WebSocket(
				(window.location.protocol == 'https:' ? 'wss' : 'ws') + '://' +
				window.location.hostname + ':31216/',
				'cloogle-stats');
		addConnectionCallbacks(connection);
	}

	connection.onclose = function() {
		console.log('Connection closed');
		window.clearTimeout(open_timer);
		open_timer = window.setInterval(tryConnect, 10000);
		tryConnect();
	};

	connection.onmessage = function(msg) {
		addRecord(JSON.parse(msg.data));
	};
}
var connection = {};
addConnectionCallbacks(connection);
connection.onclose();

activity[0] = { x: timeToString(), y: 0 };

window.setInterval(function(){
	var date = new Date();
	if ((activity.length == 0 ||
			activity[activity.length - 1].x != timeToString())) {
		activity.push({ x: timeToString(), y: 0 });
		if (activity.length > 10)
			activity.splice(0, activity.length - 10);
	}
	updateChart();
}, 1000);
