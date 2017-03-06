var form_str = document.getElementById('search_str');
var form_libs = document.getElementsByClassName('search_libs');
var sform = document.getElementById('search_form');
var sresults = document.getElementById('search_results');
var advanced_checkbox = document.getElementById('search_advanced');
var include_builtins_checkbox = document.getElementById('include_builtins');
var include_core_checkbox = document.getElementById('include_core');
var refresh_on_hash = true;

var old_str = null;
var old_libs = null;
var old_include_builtins = null;
var old_include_core = null;

function toggleLibSelection(className) {
	var boxes =
		document.getElementById(className).getElementsByClassName('search_libs');
	var checkAll = true;
	for (var i in boxes)
		if (boxes[i].checked)
			checkAll = false;

	for (var i in boxes)
		boxes[i].checked = checkAll;
}

function highlightCallback(span, cls, str) {
	if (cls == 'type') {
		return '<a class="hidden" title="Search type ' + str + '" href="#' +
			encodeURIComponent('type ' + str) + '">' +
			span + '</a>';
	} else if (cls == 'classname') {
		return '<a class="hidden" title="Search class ' + str + '" href="#' +
			encodeURIComponent('class ' + str) + '">' +
			span + '</a>';
	} else if (cls == 'generic') {
		return '<a class="hidden" title="Search generic ' + str + '" href="#' +
			encodeURIComponent(str) + '">' +
			span + '</a>';
	} else if (cls == 'funcname funcname-onlyused' || cls == 'constructor') {
		return '<a class="hidden" title="Search ' + str + '" href="#' +
			encodeURIComponent(str) + '">' +
			span + '</a>';
	} else if (cls == 'funcname funcname-generic') {
		return '<a class="hidden" title="Search ' + str + '" href="#' +
			encodeURIComponent(str) + '">' +
			span + '</a>';
	} else {
		return span;
	}
}

function makeSummary(hidden) {
	var sumlen = 0;
	var restore = false;
	for (var i in hidden)
		if (hidden[i].length == 3)
			sumlen++;
	if (sumlen != hidden.length) {
		hidden.push([null, null, 'more information']);
		restore = true;
	}

	var summ = '';
	for (var i in hidden) {
		if (hidden[i].length == 3) {
			summ += hidden[i][2];
			if (i == sumlen - 2)
				summ += ' and ';
			else if (i < sumlen - 2)
				summ += ', ';
			sumlen--;
		}
	}

	if (restore)
		hidden.splice(hidden.length-1, 1);

	return summ;
}

function getResults(str, libs, include_builtins, include_core, page) {
	if (str == null)  str  = old_str;
	if (libs == null) libs = old_libs;
	if (include_builtins == null) include_builtins = old_include_builtins;
	if (include_core == null) include_core = old_include_core;

	old_str  = str;
	old_libs = libs;
	old_include_builtins = include_builtins;
	old_include_core = include_core;

	var url = 'api.php' +
		'?str='  + encodeURIComponent(str) +
		(libs != -1
			? ('&lib=' + encodeURIComponent(libs)) : '') +
		(include_builtins != -1
			? '&include_builtins=' + encodeURIComponent(include_builtins) : '') +
		(include_core != -1
			? '&include_core=' + encodeURIComponent(include_core) : '') +
		'&page=' + page;
	var xmlHttp = new XMLHttpRequest();

	var elem = document.getElementById('page-' + page);

	elem.innerHTML += '<p id="loading">Processing...</p>';
	var more = document.getElementById('more');
	if (more !== null) {
		more.remove();
	}

	var makeTable = function (d) {
		var html = '<table>';
		for (i in d) {
			if (d[i].length == 1) {
				html += '<tr><td colspan="2">' + d[i][0] + '</td></tr>';
			} else if (d[i].length >= 2) {
				html += '<tr><th>' + d[i][0] + ': </th><td class="wide">' +
					d[i][1] + '</td></tr>';
			}
		}
		html += '</table>';
		return html;
	}

	var makeInstanceTable = function (list, highlightf, highlightstart) {
		if (list.length == 0)
			return '0';

		var instances = '<table>';

		var makeInstanceUrl = function (loc) {
			var dclUrl =
				'src/view.php?lib=' + encodeURIComponent(loc[0]) +
				'#' + encodeURIComponent(loc[1]);
			var iclUrl = dclUrl + ';icl';

			if (loc[2].length > 1)
				dclUrl += ';line=' + loc[2][1];
			if (loc[3].length > 1)
				iclUrl += ';line=' + loc[3][1];

			return loc[1] +
				' (<a target="_blank" href="' + dclUrl + '">dcl' +
					(loc[2].length > 1 ? ':' + loc[2][1] : '') + '</a>; ' +
				'<a target="_blank" href="' + iclUrl + '">icl' +
					(loc[3].length > 1 ? ':' + loc[3][1] : '') + '</a>) ' +
				'(' + loc[0] + ')';
		}

		for (var i in list) {
			instances += '<tr><th>';
			if (typeof list[i][0] === 'object') {
				for (var k in list[i][0]) {
					instances += '<code>' +
						highlightf(list[i][0][k],
								highlightCallback, highlightstart) +
						'</code> ';
				}
			} else {
				instances += '<code>' +
					highlightf(list[i][0],
							highlightCallback, highlightstart) +
					'</code>';
			}
			instances += '</th>';

			if (list[i].length == 3) {
				instances += '<td>';
				for (var k in list[i][1]) {
					instances += '<code>' +
						highlightType(list[i][1][k], highlightCallback) +
						'</code> ';
				}
				instances += '</td>';
			}

			var locs = '';
			var locsidx = list[i].length - 1;
			for (var j in list[i][locsidx]) {
				var loc = list[i][locsidx][j];
				if (locs != '') {
					locs += ', ';
				}
				locs += makeInstanceUrl(loc);
			}

			instances += '<td>&nbsp;in ' + locs + '</td></tr>';
		}
		instances += '</table>';

		return instances;
	}

	var makeGenericResultHTML = function (basic, meta, hidden, code) {
		var dclUrl =
			'src/view.php?lib=' + encodeURIComponent(basic['library']) +
			'#' + encodeURIComponent(basic['modul']);
		var iclUrl = dclUrl + ';icl';
		var dclLine = '';
		var iclLine = '';
		if ('dcl_line' in basic) {
			dclUrl += ';line=' + basic['dcl_line'];
			dclLine = ':' + basic['dcl_line'];
		}
		if ('icl_line' in basic) {
			iclUrl += ';line=' + basic['icl_line'];
			iclLine = ':' + basic['icl_line'];
		}

		var basicText = basic['library'] + ': ' +
				basic['modul'] + ' (' +
				'<a href="' + dclUrl + '" target="_blank">dcl' + dclLine + '</a>; ' +
				'<a href="' + iclUrl + '" target="_blank">icl' + iclLine + '</a>)';

		if ('builtin' in basic && basic['builtin'])
			basicText = [['Clean core (actual implementation may differ)']];

		var toggler = '';
		if (hidden.length > 0) {
			toggler = '<div class="toggler" title="More details" onclick="toggle(this)">' +
				'<span class="toggle-icon">&#x229e;</span>' + makeSummary(hidden) +
				'</div>';
		}

		return '<div class="result">' +
				'<div class="result-basic">' + basicText + '</div>' +
				'<div class="result-extra">' + meta.join('<br/>') + '</div>' +
				'<div class="result-extra toggle-container">' +
					toggler +
					'<div class="togglee">' + makeTable(hidden) + '</div></div>' +
				'<pre class="result-code">' + code + '</pre>' +
			'</div>';
	}

	var makeResultHTML = function (result) {
		var kind = result[0];
		var basic = result[1][0];
		var extra = result[1][1];

		var meta = [];
		var hidden = [];

		switch (kind) {
			case 'FunctionResult':
				if ('cls' in extra)
					meta.push('Class: <code>' +
							highlightClassDef(extra['cls']['cls_name'] +
							' ' + extra['cls']['cls_vars'].join(' '),
							highlightCallback, 'className') + '</code>');

				if ('unifier' in extra &&
					(extra['unifier'][0].length > 0 || extra['unifier'][1].length > 0))
					meta.push('Unifier: ' + makeUnifier(extra['unifier']));

				if ('generic_derivations' in extra) {
					var derivations = makeInstanceTable(
							extra['generic_derivations'],
							highlightType);
					hidden.push(['Derivations', derivations,
							pluralise(extra['generic_derivations'].length, 'derivation')]);
				}

				var hl_entry = 'start';
				if ('constructor_of' in extra) {
					meta.push('This is a type constructor of <code>' +
							highlightFunction(':: ' + extra['constructor_of'],
							highlightCallback) + '</code>.');
					hl_entry = 'startConstructor';
				} else if ('recordfield_of' in extra) {
					meta.push('This is a record field of <code>' +
							highlightFunction(':: ' + extra['recordfield_of'],
							highlightCallback) + '</code>.');
					hl_entry = 'startRecordField';
				}

				return makeGenericResultHTML(basic, meta, hidden,
						highlightFunction(extra['func'], highlightCallback, hl_entry));

			case 'TypeResult':
				if (extra['type_instances'].length > 0) {
					hidden.push([
							'Instances',
							makeInstanceTable(
								extra['type_instances'],
								highlightClassDef, 'className'),
							pluralise(extra['type_instances'].length, 'instance')]);
				}

				if (extra['type_derivations'].length > 0) {
					hidden.push([
							'Derivations',
							makeInstanceTable(
								extra['type_derivations'],
								highlightFunction, 'generic'),
							pluralise(extra['type_derivations'].length, 'derivation')]);
				}

				return makeGenericResultHTML(basic, [], hidden,
						highlightTypeDef(extra['type'], highlightCallback));

			case 'ClassResult':
				hidden.push([
						'Instances',
						makeInstanceTable(extra['class_instances'], highlightType),
						pluralise(extra['class_instances'].length, 'instance')]);

				var html = highlightClassDef(
						'class ' + extra['class_heading'] +
						(extra['class_funs'].length > 0 ? ' where' : ''),
						highlightCallback) + '\n';
				for (var i in extra['class_funs'])
					html += highlightFunction(
							'\n    ' + extra['class_funs'][i].replace(/\n/g, '\n    '),
							highlightCallback, 'macro');

				return makeGenericResultHTML(basic, [], hidden, html);

			case 'MacroResult':
				return makeGenericResultHTML(basic, [], [],
						highlightFunction(extra['macro_representation'], highlightCallback, 'macro'));

			case 'ModuleResult':
				if (extra['module_is_core'])
					meta.push(['<span class="core-module">' +
							'This is a core module and should usually only be used internally.' +
							'</span>']);

				return makeGenericResultHTML(basic, [], hidden,
						highlightFunction('import ' + basic['modul']));

			default:
				return '';
		}
	}

	var makeSuggestions = function (suggs) {
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

	xmlHttp.onreadystatechange = function () {
		if(xmlHttp.readyState == 4 && xmlHttp.status == 200){
			document.getElementById('loading').remove();
			var responsedata = JSON.parse(xmlHttp.responseText);
			if(responsedata['return'] >= 0 && responsedata['return'] <= 64){
				for(var i = 0; i<responsedata['data'].length; i++){
					var c = responsedata['data'][i];
					elem.innerHTML += makeResultHTML(c);
				}

				var par = elem.parentNode
				if (responsedata['more_available'] != 0) {
					par.innerHTML += '<div id="page-' + (page+1) + '">' +
						'<p id="more"><a href="javascript:getResults(null,null,null,null,' + (page+1) +
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

function getLibs() {
	if (!advanced_checkbox.checked)
		return -1;

	var libs = [];
	for (var i = 0; i < form_libs.length; i++) {
		if (form_libs[i].checked) {
			if (form_libs[i].value != '__builtin')
				libs.push(form_libs[i].value);
		}
	}

	return libs;
}

function formsubmit() {
	var q = form_str.value;
	if (q === '') {
		sresults.innerHTML = 'Can\'t search for the empty string';
	} else {
		sresults.innerHTML = '';

		if (q.indexOf('::') == -1 && q.indexOf('->') != -1) {
			var sug = ':: ' + q.replace('->', ' -> ');
			sresults.innerHTML = '<p>' +
				'Searching for <code>' + highlightFunction(q) + '</code>. ' +
				'Did you mean to search for ' +
				'<a class="hidden" href="#' + sug + '"><code>' +
				highlightFunction(sug) + '</code></a>?</p>';
		}

		var libs = getLibs();
		var include_builtins = -1;
		var include_core = -1;
		if (advanced_checkbox.checked) {
			var include_builtins = include_builtins_checkbox.checked;
			var include_core = include_core_checkbox.checked;
		}

		sresults.innerHTML += '<div id="page-0"></div>';
		getResults(q, libs, include_builtins, include_core, 0);
	}
	return false;
};

advanced_checkbox.onchange = function () {
	var el = document.getElementById('advanced');
	el.style.display = this.checked ? 'block' : 'none';
}

window.onload = function () {
	sform.onsubmit = formsubmit;
	var str = decodeURIComponent(document.location.hash);
	if(str !== ''){
		str = str.substring(1);
		form_str.value = decodeURIComponent(str);
		formsubmit();
	}

	if (advanced_checkbox.checked)
		advanced_checkbox.onchange();

	document.getElementById('search_str').focus();
}

window.onhashchange = function () {
	if (!refresh_on_hash) {
		refresh_on_hash = true;
	} else {
		var str = decodeURIComponent(document.location.hash.replace('+', '%20');
		form_str.value = str.substring(1);
		formsubmit();
	}
}
