var searches = [];
var activity = [];

Date.prototype.timeNow = function (seconds) {
	return ((this.getHours() < 10)?"0":"") + this.getHours() +":"+
	  ((this.getMinutes() < 10)?"0":"") + this.getMinutes() +
		(seconds ? (":"+((this.getSeconds() < 10)?"0":"") + this.getSeconds()) : '');
}

function updateSearches() {
	query = searches[0];
	console.log(query);
	var last = document.getElementById('last-search');
	last.innerHTML = '<pre>' + highlight(query) + '</pre>';
	last.innerHTML += '<div class="time">' + new Date().timeNow(true) + '</div>';

	var previous = document.getElementById('previous-searches');
	previous.innerHTML = '';
	for (var i in searches) {
		if (i == 0) continue;
		var color = 100 + i * 20;
		var color = 'rgb(' + color + ',' + color + ',' + color + ')';
		previous.innerHTML += '<tt style="color:' + color + ';">' + searches[i] + '</tt><br/>';
	}
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

connection = new WebSocket('ws://' + window.location.hostname + ':31216/', 'cloogle-stats');
connection.onopen = function() { console.log('Connection open'); };
connection.onclose = function() { console.log('Connection closed'); };
connection.onmessage = function(msg) {
	console.log(msg.data);
	var req = JSON.parse(msg.data);
	var query = req.name + (req.unify ? ' :: ' + req.unify : '');
	console.log(query);

	searches.splice(0, 0, query);
	if (searches.length > 10)
		searches.splice(searches.length - 1, searches.length);

	var time = new Date().timeNow(false);
	if (activity.length == 0 || activity[activity.length - 1].x != time) {
		activity.push({ x: time, y: 0 });
	}
	activity[activity.length - 1].y++;

	updateSearches();
	updateChart();
};

activity[0] = { x: new Date().timeNow(false), y: -1 };

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
