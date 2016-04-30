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
			[/^([a-z][a-zA-Z]*)(.*)/, ['typevar']],
			[/^([A-Z]\w*)(.*)/, ['type']],
			[/^(\/\/.*)(.*?)/, ['comment']],
			[/^(\|)(.*)/, ['punctuation'], 'context'],
			[/^(\W)(.*)/, ['punctuation']]
		],
		context: [
			[/^(\s+)(.*)/, ['whitespace']],
			[/^(\/\/.*)(.*?)/, ['comment']],
			[/^(,)(.*)/, ['punctuation']],
			[/^(\S+)(,.*)/, ['classname']],
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
	var map = { "&": "&amp;", "<": "&lt;", ">": "&gt;",
		'"': '&quot;', "'": '&#39;', "/": '&#x2F;' };
	return String(unsafe).replace(/[&<>"'\/]/g, function(s){return map[s];});
}

function getResults(str, page) {
	var url = 'api.php?str=' + encodeURIComponent(str) + '&page=' + page;
	var xmlHttp = new XMLHttpRequest();

	var elem = document.getElementById('page-' + page);

	elem.innerHTML += '<p id="loading">Processing...</p>';
	var more = document.getElementById('more');
	if (more !== null) {
		more.remove();
	}

	xmlHttp.onreadystatechange = function() {
		if(xmlHttp.readyState == 4 && xmlHttp.status == 200){
			document.getElementById('loading').remove();
			var responsedata = JSON.parse(xmlHttp.responseText);
			if(responsedata['return'] === 0){
				for(var i = 0; i<responsedata['data'].length; i++){
					var c = responsedata['data'][i];
					elem.innerHTML += '<hr/><table>' +
						'<tr><th>Library: </th><td>' + c['library'] + '</td></tr>' +
						'<tr><th>Filename: </th><td>' + c['filename'] + '</td></tr>' +
						'<tr><th>Module: </th><td>' + c['modul'] + '</td>' +
						'<td>' + c['distance'] + '</td></tr>' +
						('cls' in c ? ('<tr><th>Class: </th><td>' + c['cls']['cls_name'] +
								' ' + c['cls']['cls_vars'].join(' ') + '</td></tr>') : '') +
						'</table>' +
						'<code>' + highlight(c['func']) + '</code>';
				}

				if (responsedata['more_available'] != 0) {
					elem.parentNode.innerHTML += '<div id="page-' + (page+1) + '">' +
						'<p id="more"><a href="javascript:getResults(\'' +
							escapeJS(str) + '\',' + (page+1) +
						')">' + responsedata['more_available'] + ' more...</a></p>' +
						'</div>';
				}
			} else {
				elem.innerHTML =
					'<p>Return code: ' + responsedata['return'] + '</p>' +
					'<p>Message: ' + responsedata['msg'] + '</p>';
			}
		}
	};
	xmlHttp.open('GET', url, true); // true for asynchronous 
	xmlHttp.send(null);
	document.location.hash = "#" + str;
}

function escapeJS(s) {
	return ('' + s).replace(/["'\\\n\r\u2028\u2029]/g, function (c) {
		switch (c) {
			case '"':
			case "'":
			case '\\': return '\\' + c;
			case '\n': return '\\n';
			case '\r': return '\\r';
			case '\u2028': return '\\u2028';
			case '\u2029': return '\\u2029';
		}
	});
}

function formsubmit(){
	if (form_str.value === '') {
		sresults.innerHTML = 'Can\'t search for the empty string';
	} else {
		sresults.innerHTML = '<div id="page-0"></div>';
		getResults(form_str.value, 0);
	}
	return false;
};

window.onload = function(){
	sform.onsubmit = formsubmit;
	var str = decodeURIComponent(document.location.hash);
	if(str !== ''){
		str = str.substring(1);
		form_str.value = decodeURIComponent(str);
		formsubmit();
	}

	document.getElementById('search_str').focus();
}
