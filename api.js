var form_str = document.getElementById('search_str');
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
					str += '<span class="' + clss[k] + '">' + escapeHTML(parts[j]) + '</span>';
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

function escapeHTML(unsafe) {
	// http://stackoverflow.com/a/25396011/1544337
	var text = document.createTextNode(unsafe);
	var div = document.createElement('div');
	div.appendChild(text);
	return div.innerHTML;
}

function formsubmit(){
	if(form_str.value === ''){
		sresults.innerHTML = 'Can\'t search for the empty string';
	} else {
		sresults.innerHTML = 'Proccessing...';
		var str = encodeURIComponent(form_str.value);
		var url = 'api.php?str=' + str;
		var xmlHttp = new XMLHttpRequest();
		xmlHttp.onreadystatechange = function() { 
			if(xmlHttp.readyState == 4 && xmlHttp.status == 200){
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
		document.location.hash = "#" + str;
	}
	return false;
};

window.onload = function(){
	sform.onsubmit = formsubmit;
	var str = decodeURIComponent(document.location.hash);
	if(str !== ''){
		str = str.substring(1);
		form_str.value = str;
		formsubmit();
	}

	document.getElementById('search_str').focus();
}
