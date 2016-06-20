var form_str = document.getElementById('search_str');
var sform = document.getElementById('search_form');
var sresults = document.getElementById('search_results');
var refresh_on_hash = true;

function getResults(str, page) {
	var url = 'api.php?str=' + encodeURIComponent(str) + '&page=' + page;
	var xmlHttp = new XMLHttpRequest();

	var elem = document.getElementById('page-' + page);

	elem.innerHTML += '<p id="loading">Processing...</p>';
	var more = document.getElementById('more');
	if (more !== null) {
		more.remove();
	}

	function highlightCallback(span, cls, str) {
		if (cls == 'type') {
			return '<a class="hidden" title="Search :: ' + str + '" href="#' +
				encodeURIComponent(':: ' + str) + '">' +
				span + '</a>';
		} else if (cls == 'classname') {
			return '<a class="hidden" title="Search class ' + str + '" href="#' +
				encodeURIComponent('class ' + str) + '">' +
				span + '</a>';
		} else if (cls == 'generic') {
			return '<a class="hidden" title="Search generic ' + str + '" href="#' +
				encodeURIComponent(str) + '">' +
				span + '</a>';
		} else {
			return span;
		}
	}

	function makeTable(d) {
		var html = '<table>';
		for (i in d) {
			if (d[i].length == 1) {
				html += '<tr><td colspan="2">' + d[i][0] + '</td></tr>';
			} else if (d[i].length == 2) {
				html += '<tr><th>' + d[i][0] + ': </th><td>' +
					d[i][1] + '</td></tr>';
			}
		}
		html += '</table>';
		return html;
	}

	function makeResultHTML(result) {
		var kind = result[0];
		var basic = result[1][0];
		var specific = result[1][1];

		var dclUrl =
			'src/view.php?lib=' + encodeURIComponent(basic['library']) +
			'&mod=' + encodeURIComponent(basic['modul']) +
			'&hl';
		var iclUrl = dclUrl + '&icl';

		var basicData = [
			['Library',  basic['library']],
			['Filename', '<a href="' + dclUrl + '" target="_blank">' +
				basic['filename'] +
				'</a> (<a href="' + iclUrl + '" target="_blank">icl</a>)'],
			['Module',   basic['modul']],
			['Distance', basic['distance']]
		];

		switch (kind) {
			case 'FunctionResult':
				var specificData = [];
				if ('constructor_of' in specific) {
					specificData.push([
						'This function is a type constructor of <code>' +
						highlightFunction(':: ' + specific['constructor_of'],
							highlightCallback) + '</code>.'
					]);
				}
				specificData.push(
					('cls' in specific ?
						[ 'Class', '<code>' +
						  highlightClassDef(specific['cls']['cls_name'] +
							  ' ' + specific['cls']['cls_vars'].join(' '),
							  highlightCallback, 'className') + '</code>'] :
						[]),
					('unifier' in specific &&
						(specific['unifier'][0].length > 0 ||
						 specific['unifier'][1].length > 0) ?
						['Unifier', makeUnifier(specific['unifier'])] :
						[])
				);
				return '<hr/>' +
					makeTable(basicData.concat(specificData)) +
					'<code>' +
					highlightFunction(specific['func'], highlightCallback) +
					'</code>';
				break;
			case 'TypeResult':
				return '<hr/>' +
					makeTable(basicData) +
					'<pre>' +
					highlightTypeDef(specific['type'], highlightCallback) +
					'</pre>';
				break;
			case 'ClassResult':
				var instances = '';
				for (var i in specific['class_instances']) {
					if (instances != '') {
						instances += ', ';
					}
					instances += '<code>' +
						highlightType(specific['class_instances'][i],
								highlightCallback) +
						'</code>'
				}
				var specificData = [
					['Instances', instances]
				];
				var html = '<hr/>' +
					makeTable(basicData.concat(specificData)) + '<pre>' +
					highlightClassDef(
							'class ' + specific['class_heading'] +
							(specific['class_funs'].length > 0 ? ' where' : ''),
							highlightCallback) +
					'<br/>';
				for (var i in specific['class_funs']) {
					html += '<br/>    ' +
						highlightFunction(specific['class_funs'][i],
								highlightCallback);
				}
				html += '</pre>';
				return html;
				break;
			default:
				return '';
		}
	}

	function makeSuggestions(suggs) {
		var str = '<hr/><div id="suggestions"><b>Did you mean...</b><table>';
		for (i in suggs) {
			var sug = suggs[i][0];
			var sugstr = []
			if ('name' in sug) {
				sugstr.push(sug.name);
			}
			if ('unify' in sug) {
				sugstr.push(':: ' + sug.unify);
			}
			sugstr = sugstr.join(' ');
			str += '<tr><td><a class="hidden" href="#' + encodeURIComponent(sugstr) + '"><code>' +
				highlightFunction(sugstr) + '</code></a></td><td>' +
				suggs[i][1] + ' results</td></tr>';
		}
		str += '</table></div>';
		return str;
	}

	xmlHttp.onreadystatechange = function() {
		if(xmlHttp.readyState == 4 && xmlHttp.status == 200){
			document.getElementById('loading').remove();
			var responsedata = JSON.parse(xmlHttp.responseText);
			if(responsedata['return'] === 0){
				for(var i = 0; i<responsedata['data'].length; i++){
					var c = responsedata['data'][i];
					elem.innerHTML += makeResultHTML(c);
				}

				var par = elem.parentNode
				if (responsedata['more_available'] != 0) {
					par.innerHTML += '<div id="page-' + (page+1) + '">' +
						'<p id="more"><a href="javascript:getResults(\'' +
							escapeJS(str) + '\',' + (page+1) +
						')">' + responsedata['more_available'] + ' more...</a></p>' +
						'</div>';
				}

				if ('suggestions' in responsedata &&
						responsedata['suggestions'].length > 0) {
					par.innerHTML =
						makeSuggestions(responsedata['suggestions'])
						+ par.innerHTML;
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
	if (document.location.hash.substring(1) != encodeURIComponent(str)) {
		refresh_on_hash = false;
		document.location.hash = "#" + encodeURIComponent(str);
	}
}

function makeUnifier(ufr) {
	var from_left = ufr[0];
	var from_right = ufr[1];
	var s = '';
	for (i in from_right) {
		s += '<tt>' + from_right[i][0] + '</tt> &rarr; <tt>' + from_right[i][1] + '</tt>; ';
	}
	for (i in from_left) {
		s += '<tt>' + from_left[i][1] + '</tt> &larr; <tt>' + from_left[i][0] + '</tt>; ';
	}
	return s.substring(0, s.length - 2);
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

function formsubmit() {
	if (form_str.value === '') {
		sresults.innerHTML = 'Can\'t search for the empty string';
	} else {
		sresults.innerHTML = '<div id="page-0"></div>';
		getResults(form_str.value, 0);
	}
	return false;
};

window.onload = function() {
	sform.onsubmit = formsubmit;
	var str = decodeURIComponent(document.location.hash);
	if(str !== ''){
		str = str.substring(1);
		form_str.value = decodeURIComponent(str);
		formsubmit();
	}

	document.getElementById('search_str').focus();
}

window.onhashchange = function() {
	if (!refresh_on_hash) {
		refresh_on_hash = true;
	} else {
		var str = decodeURIComponent(document.location.hash);
		form_str.value = str.substring(1);
		formsubmit();
	}
}
