var form_str = document.getElementById('search-str');
var form_libs = document.getElementsByClassName('search-libs');
var sform = document.getElementById('search-form');
var sresults = document.getElementById('search-results');
var include_builtins_checkbox = document.getElementById('include-builtins');
var include_core_checkbox = document.getElementById('include-core');
var include_apps_checkbox = document.getElementById('include-apps');
var share_button = document.getElementById('share-button');
var share_link = document.getElementById('share-link');

var refresh_on_hash = true;

var advanced = false;
var old_str = null;
var old_libs = null;
var old_include_builtins = null;
var old_include_core = null;
var old_include_apps = null;

function makeGeneralHelp(query) {
	return 'For general information about Clean, ' +
		'<a href="http://clean.cs.ru.nl/index.php?title=Special:Search&fulltext=Search&search=' + encodeURIComponent(query) + '" target="_blank">' +
		'search on the Clean wiki</a>.<br/>' +
		'For explanations about Clean concepts and syntax, see the ' +
		'<a href="http://clean.cs.ru.nl/download/doc/CleanLangRep.2.2.pdf" target="_blank">' +
		'Clean language report</a>.';
}

function toggleLibSelection(className) {
	var boxes =
		document.getElementById(className).getElementsByClassName('search-libs');
	var checkAll = true;
	for (var i in boxes)
		if (boxes[i].checked)
			checkAll = false;

	for (var i in boxes)
		boxes[i].checked = checkAll;
}

function toggleAdvanced() {
	advanced = !advanced;
	toggleById('advanced');
}

function highlightCallback(span, cls, str) {
	if (cls == 'type') {
		return '<a class="hidden" title="Search type ' + str + '" href="#' +
			encodeURIComponent('type ' + str) + '">' +
			span + '</a>';
	} else if (cls == 'classname' || cls == 'classname classname-generic') {
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
		sumlen++;
		restore = true;
	}

	var summ = '';
	for (var i in hidden) {
		if (hidden[i].length == 3) {
			summ += hidden[i][2];
			if (sumlen == 2)
				summ += ' and ';
			else if (sumlen > 2)
				summ += ', ';
			sumlen--;
		}
	}

	if (restore)
		hidden.splice(hidden.length-1, 1);

	return summ;
}

function mergeComments(code, comments) {
	var maxLength = 0;
	for (var i in code)
		if (code[i].length > maxLength)
			maxLength = code[i].length;

	for (var i in code)
		if (i < comments.length)
			code[i] = (code[i] + Array(maxLength + 1).join(' '))
								.substring(0, maxLength+1) + comments[i];

	return code;
}

String.prototype.makeParagraphs = function() {
	return this
		.split('\n-').join('<br/>-')
		.split('\n*').join('<br/>*')
		.split('\n\n').join('<br/>');
}

function makeParametersHTML(name, params) {
	if (params.length == 1)
		return name + ': ' + params[0].makeParagraphs();
	else
		return name + 's:<ul><li>' + params.join('</li><li>').makeParagraphs() + '</li></ul>';
}

function makeLocationUrl(loc) {
	var dclUrl = 'src#' + encodeURIComponent(loc[0] + '.' + loc[1]);
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

function makeLocationUrls(locs) {
	var html = '';
	for (var j in locs) {
		if (html != '')
			html += ', ';
		html += makeLocationUrl(locs[j]);
	}
	return html;
}

function makeUnifier(ufr) {
	var from_left = ufr.left_to_right;
	var from_right = ufr.right_to_left;
	var s = '';

	for (i in from_right)
		s += '<tt>' + from_right[i][0] + '</tt> &rarr; <tt>' + from_right[i][1] + '</tt>; ';
	for (i in from_left)
		s += '<tt>' + from_left[i][1] + '</tt> &larr; <tt>' + from_left[i][0] + '</tt>; ';

	return s.substring(0, s.length - 2);
}

function makeRequiredContext(context) {
	var html = '<table>';
	for (var i in context) {
		html += '<tr><td>' +
			'<code>' + highlightFunction(context[i][0], highlightCallback) + '</code>' +
			'</td><td>' +
			(context[i][1].length > 0
				? makeLocationUrls(context[i][1])
				: 'Not currently known to Cloogle.') +
			'</td></tr>';
	}
	html += '</table>';
	return html;
}

function highlightExample(example) {
	var f = 'highlight' + example.cleanjs_type;
	if (!(f in window)) {
		return example.example;
	} else if ('cleanjs_start' in example) {
		return window[f](
				example.example, highlightCallback, example.cleanjs_start);
	} else {
		return window[f](
				example.example, highlightCallback);
	}
}

function highlightSyntaxConstruct(elem) {
	return highlightToHTML({
		start: [
			[/(\/\/.*)/, ['comment']],
			[/(\[)/,     ['punctuation'], 'optional'],
			[/(\.{3})/,  ['punctuation']],
			[/(\s+)/,    ['whitespace']],
			[/(\w+)/,    ['keyword']],
			[/(\S)/,     ['punctuation']]
		],
		optional: [
			[/(\[)/,     ['punctuation'], 'optional'],
			[/(\])/,     ['punctuation'], 'pop'],
			[/(\.{3})/,  ['punctuation']],
			[/(\s+)/,    ['whitespace']],
			[/(\w+)/,    ['keyword optional']],
			[/(\S)/,     ['punctuation']]
		]
	}, elem, function (span, cls, str) {
		if (str == '...')
			return span.replace('...', '&#8230;');
		else
			return span;
	});
}

function makeExampleList(examples) {
	var html = '<ul class="examples">';
	for (var i in examples)
		html += '<li><pre class="example">' + highlightExample(examples[i]) + '</pre></li>';
	html += '</ul>';
	return html;
}

function markupDocumentation(doc) {
	doc = doc.replace(/\n\n/g, '<br class="parbreak"/>');
	doc = doc.replace(/\n\s*-\s*/g, '<br/>- ');
	doc = doc.replace(/{{`([^`}]+)`}}/g, '`{{$1}}`');
	doc = doc.replace(/`([^`]+)`/g, '<code>$1</code>');
	doc = doc.replace(/{{([^}]+)}}/g, function (m,c) {
		return '<a class="hidden" title="Search ' + c +
			'" href="#' + encodeURIComponent(c) + '">' + c + '</a>';
	});
	return doc;
}

function getResults(str, libs, include_builtins, include_core, include_apps, page) {
	if (str == null) str = old_str;
	if (libs == null) libs = old_libs;
	if (include_builtins == null) include_builtins = old_include_builtins;
	if (include_core == null) include_core = old_include_core;
	if (include_apps == null) include_apps = old_include_apps;

	old_str = str;
	old_libs = libs;
	old_include_builtins = include_builtins;
	old_include_apps = include_apps;

	var url = 'api.php' +
		'?str='  + encodeURIComponent(str) +
		(libs != -1
			? ('&lib=' + encodeURIComponent(libs)) : '') +
		(include_builtins != -1
			? '&include_builtins=' + encodeURIComponent(include_builtins) : '') +
		(include_core != -1
			? '&include_core=' + encodeURIComponent(include_core) : '') +
		(include_apps != -1
			? '&include_apps=' + encodeURIComponent(include_apps) : '') +
		'&page=' + page;
	var xmlHttp = new XMLHttpRequest();

	var elem = document.getElementById('page-' + page);

	elem.innerHTML += '<p id="loading">Processing...</p>';
	var remove = document.getElementsByClassName('remove-at-request');
	for (var i = remove.length - 1; i >= 0; i--)
		remove[i].remove();

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

			var locsidx = list[i].length - 1;
			if (list[i][locsidx].length == 0)
				instances += '<td></td></tr>'
			else
				instances += '<td>&nbsp;in ' + makeLocationUrls(list[i][locsidx]) + '</td></tr>';
		}
		instances += '</table>';

		return instances;
	}

	var makeGenericResultHTML = function (basic, meta, hidden, code) {
		var dclUrl = 'src#' + encodeURIComponent(basic['library'] + '.' + basic['modul']);
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
			basicText = [['Clean core. The actual implementation may differ.']];

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

		if ('langrep_documentation' in basic &&
				basic['langrep_documentation'].length > 0) {
			var doc = 'See the language report: ';
			for (var i in basic['langrep_documentation']) {
				var loc = basic['langrep_documentation'][i];
				if (i != 0)
					doc += '; ';
				doc += '<a target="_blank" ' +
					'href="/doc/#' + loc.clr_file + ';jump=' + loc.clr_heading + '">' +
					loc.clr_section + '</a>';
			}
			doc += '.';
			meta.push(doc);
		}

		if ('documentation' in basic)
			meta.push(markupDocumentation(basic['documentation']));

		switch (kind) {
			case 'FunctionResult':
				if ('param_doc' in extra && extra['param_doc'].length > 0)
					hidden.push([makeParametersHTML('Parameter', extra['param_doc'])]);
				if ('generic_var_doc' in extra && extra['generic_var_doc'].length > 0)
					hidden.push([makeParametersHTML('Generic type variable', extra['generic_var_doc'])]);
				if ('result_doc' in extra)
					hidden.push(['Result: ' + extra['result_doc']]);

				if ('cls' in extra)
					meta.push('Class: <code>' +
							highlightClassDef(extra['cls']['cls_name'] +
							' ' + extra['cls']['cls_vars'].join(' '),
							highlightCallback, 'className') + '</code>');

				if ('required_context' in extra && extra['required_context'].length > 0) {
					hidden.push(['Required context', makeRequiredContext(extra['required_context'])]);
				}
				if ('unifier' in extra &&
						(extra['unifier'].left_to_right.length > 0
						 || extra['unifier'].right_to_left.length > 0))
					hidden.push(['Unifier', makeUnifier(extra['unifier'])]);
				if ('unifier' in extra) {
					var synonyms = extra['unifier'].used_synonyms;
					for (var i in synonyms) {
						hidden.push(['Used the type synonym <code>' + highlightTypeDef(
									':: ' + synonyms[i][0] + ' :== ' + synonyms[i][1],
									highlightCallback) + '</code>.']);
					}
				}

				if ('generic_derivations' in extra &&
						extra['generic_derivations'].length > 0) {
					var derivations = makeInstanceTable(
							extra['generic_derivations'],
							highlightType);
					hidden.push(['Derivations', derivations,
							pluralise(extra['generic_derivations'].length, 'derivation')]);
				}

				var hl_entry = 'start';
				switch (extra['kind'][0]) {
					case 'Constructor':
						meta.push('This is a type constructor of <code>' +
								highlightFunction(':: ' + extra['constructor_of'],
								highlightCallback) + '</code>.');
						hl_entry = 'startConstructor';
						break;
					case 'RecordField':
						meta.push('This is a record field of <code>' +
								highlightFunction(':: ' + extra['recordfield_of'],
								highlightCallback) + '</code>.');
						hl_entry = 'startRecordField';
						break;
					case 'Macro':
						hl_entry = 'macro';
						break;
				}

				var code = highlightFunction(extra['func'], highlightCallback, hl_entry);
				if ('type_doc' in extra) {
					var name = extra['func'].split(' ')[0];
					var type = highlightFunction(name + ' :: ' + extra['type_doc'], highlightCallback);
					code = type + '\r\n' + code;
				}

				return makeGenericResultHTML(basic, meta, hidden, code);

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

				var code = extra['type'].split('\n');
				if ('type_field_doc' in extra) {
					var comments = [];
					for (var i in extra['type_field_doc'])
						comments.push(extra['type_field_doc'][i].length > 1
								? '//* ' + extra['type_field_doc'][i][1]
								: '');
					code = mergeComments(code, comments)
				}
				if ('type_constructor_doc' in extra) {
					var comments = [];
					for (var i in extra['type_constructor_doc'])
						comments.push(extra['type_constructor_doc'][i].length > 1
								? '//* ' + extra['type_constructor_doc'][i][1]
								: '');
					code = mergeComments(code, comments)
				}

				return makeGenericResultHTML(basic, meta, hidden,
						highlightTypeDef(code.join('\n'), highlightCallback));

			case 'ClassResult':
				if (extra['class_instances'].length > 0)
					hidden.push([
							'Instances',
							makeInstanceTable(extra['class_instances'], highlightType),
							pluralise(extra['class_instances'].length, 'instance')]);
				if (extra['class_derivations'].length > 0)
					hidden.push([
							'Derivations',
							makeInstanceTable(extra['class_derivations'], highlightType),
							pluralise(extra['class_derivations'].length, 'derivation')]);

				var html = highlightClassDef(
						'class ' + extra['class_heading'] +
						(extra['class_funs'].length > 0 ? ' where' : ''),
						highlightCallback) + '\n';
				for (var i in extra['class_funs'])
					html += highlightFunction(
							'\n    ' + extra['class_funs'][i].replace(/\n/g, '\n    '),
							highlightCallback, 'macro');

				return makeGenericResultHTML(basic, meta, hidden, html);

			case 'ModuleResult':
				if (extra['module_is_core'])
					meta.push(['<span class="core-module">' +
							'This is a core module and should usually only be used internally.' +
							'</span>']);

				return makeGenericResultHTML(basic, meta, hidden,
						highlightFunction('import ' + basic['modul']));

			case 'SyntaxResult':
				var toggler = '';
				if (extra['syntax_examples'].length > 0) {
					toggler = '<div class="toggler" title="More details" onclick="toggle(this)">' +
						'<span class="toggle-icon">&#x229e;</span>' + pluralise(extra['syntax_examples'].length, 'example') +
						'</div>';
				}

				var code = '';
				for (var i in extra['syntax_code']) {
					code += highlightSyntaxConstruct(extra['syntax_code'][i]) + '\n';
				}

				return '<div class="result">' +
						'<div class="result-basic">Clean syntax: ' + extra['syntax_title'] + '</div>' +
						'<div class="result-extra">' + meta.join('<br/>') + '</div>' +
						'<div class="result-extra toggle-container">' +
							toggler +
							'<div class="togglee">' + makeExampleList(extra['syntax_examples']) + '</div></div>' +
						'<pre class="result-code">' + code + '</pre>' +
					'</div>';

			default:
				return '';
		}
	}

	var makeSuggestions = function (suggs) {
		var str = '<div id="suggestions"><b>Did you mean...</b><table>';
		for (i in suggs) {
			var sug = suggs[i][0];
			var sugstr = [];
			if ('name' in sug) {
				sugstr.push(sug.name);
			}
			if ('unify' in sug) {
				sugstr.push(':: ' + sug.unify);
			}
			sugstr = sugstr.join(' ');
			str += '<tr><td><a class="hidden" href="#' + encodeURIComponent(sugstr) + '"><code>' +
				highlightFunction(sugstr) + '</code></a></td><td>(' +
				suggs[i][1] + ' results)</td></tr>';
		}
		str += '</table></div>';
		return str;
	}

	xmlHttp.onreadystatechange = function () {
		if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
			document.getElementById('loading').remove();
			var responsedata = JSON.parse(xmlHttp.responseText);
			if (responsedata['return'] >= 0 && responsedata['return'] <= 64) {
				for (var i = 0; i<responsedata['data'].length; i++) {
					var c = responsedata['data'][i];
					elem.innerHTML += makeResultHTML(c);
				}

				var par = elem.parentNode;
				if (responsedata['more_available'] != 0) {
					par.innerHTML += '<div id="page-' + (page+1) + '">' +
						'<p id="more" class="remove-at-request">' +
						'<a href="javascript:getResults(null,null,null,null,null,' + (page+1) +
						')">' + responsedata['more_available'] + ' more...</a></p>' +
						'</div>';
				}

				par.innerHTML += '<div class="remove-at-request general-help">' + makeGeneralHelp(str) + '</span>';

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

	var newhash = encodeURIComponent(str) +
			(libs != -1
				? ('%0Alib=' + encodeURIComponent(libs)) : '') +
			(include_builtins != -1
				? '%0Ainclude_builtins=' + encodeURIComponent(include_builtins) : '') +
			(include_core != -1
				? '%0Ainclude_core=' + encodeURIComponent(include_core) : '') +
			(include_apps != -1
				? '%0Ainclude_apps=' + encodeURIComponent(include_apps) : '');
	if (newhash != document.location.hash.substring(1)) {
		refresh_on_hash = false;
		document.location.hash = '#' + newhash;
		restoreShareUI();
	}
}

function getLibs() {
	if (!advanced)
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
	document.getElementById("header").classList.add('result-view');

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
		var include_apps = -1;
		if (advanced) {
			include_builtins = include_builtins_checkbox.checked;
			include_core = include_core_checkbox.checked;
			include_apps = include_apps_checkbox.checked;
		}

		sresults.innerHTML += '<div id="page-0"></div>';
		getResults(q, libs, include_builtins, include_core, include_apps, 0);
	}
	return false;
};

window.onload = function () {
	sform.onsubmit = formsubmit;
	hashQuery();
	document.getElementById('search-str').focus();
}

function hashQuery() {
	if (document.location.hash == '' || document.location.hash == '#')
		return;

	var args = document.location.hash.substring(1).split('%0A');
	form_str.value = decodeURIComponent(args[0]);
	page = 0;

	if (args.length > 1) {
		if (!advanced)
			toggleAdvanced();

		for (var i = 1; i<args.length; i++) {
			var equal = args[i].indexOf('=');

			var value = args[i].substring(equal + 1);
			switch (args[i].substring(0, equal)) {
				case "lib":
					var libs = value.split('%2C');
					for (var j = 0; j < form_libs.length; j++) {
						form_libs[j].checked = false;
						for (var s = 0; s < libs.length; s++) {
							if (libs[s] == form_libs[j].value) {
								form_libs[j].checked = true;
								break;
							}
						}
					}
					break;
				case "include_builtins":
					include_builtins_checkbox.checked = value == 'true';
					break;
				case "include_core":
					include_core_checkbox.checked = value == 'true';
					break;
				case "include_apps":
					include_apps_checkbox.checked = value == 'true';
					break;
				case "page":
					page = value;
					break;
			}
		}
	}

	formsubmit();
}

window.onhashchange = function () {
	if (!refresh_on_hash) {
		refresh_on_hash = true;
	} else {
		hashQuery();
		restoreShareUI();
	}
}

function restoreShareUI() {
	share_button.innerHTML = "Share";
	share_button.classList.remove('disabled');
	share_button.classList.remove('visible');
	share_button.style.fontWeight = 'normal';
	share_link.classList.remove('visible');
}

function shareButtonClick () {
	if (share_button.innerHTML != "Share")
		return;

	var onUpdate = function (type, msg) {
		share_button.classList.add('disabled');
		switch (type) {
			case 'update':
				share_button.innerHTML = msg;
				break;
			case 'success':
				share_link.value = msg;
				share_link.classList.add('visible');
				share_link.select();
				share_button.innerHTML = '&#8601;';
				share_button.style.fontWeight = 'bold';
				break;
			case 'error':
				console.log(msg);
				share_button.innerHTML = "Failed, check console";
				break;
		}
	};

	if (document.location.hash == '#' || document.location.hash == '')
		onUpdate('success', 'https://cloo.gl');
	else
		shortenURL('cloogle', document.location.hash, onUpdate);
}
