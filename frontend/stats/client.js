var searches = [];
var activity = [];

Date.prototype.timeNow = function (seconds) {
	return ((this.getHours() < 10)?"0":"") + this.getHours() +":"+
	  ((this.getMinutes() < 10)?"0":"") + this.getMinutes() +
		(seconds ? (":"+((this.getSeconds() < 10)?"0":"") + this.getSeconds()) : '');
}

function updateSearches() {
	query = searches[0];
	var last = document.getElementById('last-search');

	var highlighter = highlightFunction;
	if (query.substring(0,6) == 'class ') {
		highlighter = highlightClassDef;
	}

	last.innerHTML = '<pre>' + highlighter(query) + '</pre>';
	last.innerHTML += '<div class="time">' + new Date().timeNow(true) + '</div>';

	var previous = document.getElementById('previous-searches');
	previous.innerHTML = '';
	for (var i in searches) {
		if (i == 0) continue;
		var color = 100 + i * 20;
		var color = 'rgb(' + color + ',' + color + ',' + color + ')';
		previous.innerHTML += '<tt style="color:' + color + ';">' +
				escapeHTML(searches[i]) + '</tt><br/>';
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

var open_timer;
var is_open_message = false;
function addConnectionCallbacks(connection) {
	connection.onopen = function() {
		is_open_message = true;
		console.log('Connection open');
		window.clearInterval(open_timer);
	};

	function tryConnect() {
		if (typeof connection.close != 'undefined') {
			connection.close();
		}
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
		var req = JSON.parse(msg.data);
		var query = (req.name ? req.name : '') +
			(req.unify ? ' :: ' + req.unify : '');
		if ('className' in req) {
			query = 'class ' + req.className;
		} else if ('typeName' in req) {
			query = ':: ' + req.typeName;
		}

		if (searches.length == 0 || !is_open_message) {
			searches.splice(0, 0, query);
			if (searches.length > 10)
				searches.splice(searches.length - 1, searches.length);
		}

		if (!is_open_message) {
			var time = new Date().timeNow(false);
			if (activity.length == 0 || activity[activity.length - 1].x != time) {
				activity.push({ x: time, y: 0 });
			}
			activity[activity.length - 1].y++;
		} else {
			is_open_message = false;
		}

		updateSearches();
		updateChart();
	};
}
var connection = {};
addConnectionCallbacks(connection);
connection.onclose();

activity[0] = { x: new Date().timeNow(false), y: 0 };

window.setInterval(function(){
	var date = new Date();
	if (date.getSeconds() === 0 &&
			(searches.length == 0 ||
			searches[searches.length - 1].x != date.timeNow(false))) {
		activity.push({ x: date.timeNow(false), y: 0 });
		if (activity.length > 10) {
			activity.splice(0, activity.length - 10);
		}
	}
	updateChart();
}, 1000);
