var form_str = document.getElementById('search_str');
var cb_type = document.getElementById('cb_func');
var cb_func = document.getElementById('cb_type');
var sform = document.getElementById('search_form');
var sresults = document.getElementById('search_results');

function highlight(type) {
	str = '';

	var lex = [
			[/^(.+)(::.*)/, 'name'],
			[/^([a-z]+)(.*)/, 'typevar'],
			[/^([A-Z]\w*)(.*)/, 'type'],
			[/^(\/\/.*)(.*?)/, 'comment'],
			[/^(\W)(.*)/, 'punctuation']
		];

	while (true) {
		var found = false;
		for (i in lex) {
			if (type.match(lex[i][0])) {
				parts = lex[i][0].exec(type);
				str += '<span class="' + lex[i][1] + '">' + parts[1] + '</span>';
				type = parts[2];
				found = true;
				break;
			}
		}
		if (!found || type == '')
			return str;
	}
}

cb_type.onchange = function(){
	if(!cb_type.checked && !cb_func.checked){
		cb_type.checked = true;
	}
};

cb_func.onchange = function(){
	if(!cb_type.checked && !cb_func.checked){
		cb_func.checked = true;
	}
};

sform.onsubmit = function(){
	if(form_str.value === ''){
		sresults.innerHTML = 'Can\'t search for the empty string';
	} else {
		sresults.innerHTML = 'Proccessing...';
		var url = 'http://martlubbers.net/cloogle/api.php?str=' +
			encodeURIComponent(form_str.value);
		console.log('Async: ' + url);
		var xmlHttp = new XMLHttpRequest();
		xmlHttp.onreadystatechange = function() { 
			if(xmlHttp.readyState == 4 && xmlHttp.status == 200){
				console.log(xmlHttp.responseText);
				var responsedata = JSON.parse(xmlHttp.responseText);
				sresults.innerHTML =
					'<p>Return code: ' + responsedata['return'] + '</p>' +
					'<p>Message: ' + responsedata['msg'] + '</p>';
				if(responsedata['return'] === 0){
					for(var i = 0; i<responsedata['data'].length; i++){
						var c = responsedata['data'][i];
						sresults.innerHTML += '<hr /><table>' +
							'<tr><th>Filename: </th><td>' + c['filename'] + '</td></tr>' +
							'<tr><th>Module: </th><td>' + c['module'] + '</td></tr>' +
							'</table>' + 
							'<code>' + highlight(c['func']) + '</code>';
					}
				}
			}
		};
		xmlHttp.open('GET', url, true); // true for asynchronous 
		xmlHttp.send(null);
	}
	return false;
};
