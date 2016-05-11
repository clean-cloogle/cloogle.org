var searches = [];

Date.prototype.timeNow = function () {
	return ((this.getHours() < 10)?"0":"") + this.getHours() +":"+
	  ((this.getMinutes() < 10)?"0":"") + this.getMinutes() +":"+
	  ((this.getSeconds() < 10)?"0":"") + this.getSeconds();
}

function updateSearches() {
	query = searches[0];
	console.log(query);
	var last = document.getElementById('last-search');
	last.innerHTML = '<pre>' + highlight(query) + '</pre>';
	last.innerHTML += '<div class="time">' + new Date().timeNow() + '</div>';

	var previous = document.getElementById('previous-searches');
	previous.innerHTML = '';
	for (var i in searches) {
		if (i == 0) continue;
		var color = 100 + i * 20;
		var color = 'rgb(' + color + ',' + color + ',' + color + ')';
		previous.innerHTML += '<tt style="color:' + color + ';">' + searches[i] + '</tt><br/>';
	}
}

connection = new WebSocket('ws://' + window.location.hostname + ':31216/', 'cloogle-stats');
connection.onopen = function() { console.log('Connection open'); }
connection.onclose = function() { console.log('Connection closed'); }
connection.onmessage = function(msg) {
	console.log(msg.data);
	var req = JSON.parse(msg.data);
	var query = req.name + (req.unify ? ' :: ' + req.unify : '');
	console.log(query);
	searches.splice(0, 0, query);
	if (searches.length > 10)
		searches.splice(searches.length - 1, searches.length);
	updateSearches();
}
