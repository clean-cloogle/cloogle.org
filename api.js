var form_str = document.getElementById('search_str');
var cb_type = document.getElementById('cb_func');
var cb_func = document.getElementById('cb_type');
var sform = document.getElementById('search_form');
var sresults = document.getElementById('search_results');

function highlight(type) {
	str = '';

	var lex = {
		start: [
			[/^(\s+)(.*)/, ['whitespace']],
			[/^(.+)(::.*)/, ['funcname'], 'type']
		],
		type: [
			[/^(\s+)(.*)/, ['whitespace']],
			[/^([a-z]+)(.*)/, ['typevar']],
			[/^([A-Z]\w*)(.*)/, ['type']],
			[/^(\/\/.*)(.*?)/, ['comment']],
			[/^(\|)(.*)/, ['punctuation'], 'context'],
			[/^(\W)(.*)/, ['punctuation']]
		],
		context: [
			[/^(\s+)(.*)/, ['whitespace']],
			[/^(\/\/.*)(.*?)/, ['comment']],
			[/^(\S+)(.*)/, ['classname'], 'contextType']
		],
		contextType: [
			[/^(\s+)(.*)/, ['whitespace']],
			[/^(\/\/.*)(.*?)/, ['comment']],
			[/^([,&])(.*)/, ['punctuation'], 'context'],
			[/^([^\s,]+)(.*)/, ['typevar']]
		]
	};

	state = 'start';
	while (true) {
		var found = false;
		for (i in lex[state]) {
			patt = lex[state][i][0];
			clss = lex[state][i][1];
			if (type.match(patt)) {
				parts = patt.exec(type);
				var j = 0;
				for (k in clss) {
					j = parseInt(k)+1;
					str += '<span class="' + clss[k] + '">' + parts[j] + '</span>';
				}
				type = parts[j+1];

				found = true;
				if (lex[state][i].length > 2)
					state = lex[state][i][2];

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
		var url = 'api.php?str=' +
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
							'<tr><th>Library: </th><td>' + c['library'] + '</td></tr>' +
							'<tr><th>Filename: </th><td>' + c['filename'] + '</td></tr>' +
							'<tr><th>Module: </th><td>' + c['module'] + '</td>' +
							'<td>' + c['distance'] + '</td></tr>' +
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
