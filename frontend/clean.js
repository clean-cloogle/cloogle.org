function highlight(lex, istr, start) {
	var out = [];

	for (var group in lex) {
		for (var i in lex[group]) {
			lex[group][i][0] = new RegExp(
					/^/.source +
					lex[group][i][0].source +
					/([\s\S]*)/.source);
		}
	}

	var state_stack = [];
	var state = 'start';
	if (typeof start != 'undefined') {
		state = start;
	}
	while (true) {
		var found = false;
		for (var i in lex[state]) {
			var patt = lex[state][i][0];
			var clss = lex[state][i][1];
			if (istr.match(patt)) {
				var parts = patt.exec(istr);
				var j = 0;
				for (var k in clss) {
					j++;
					out.push({'class': clss[k], 'str': parts[j]});
				}
				istr = parts[j+1];

				found = true;
				if (lex[state][i].length > 2) {
					var new_state = lex[state][i][2];
					if (new_state == 'pop') {
						state = state_stack.pop();
					} else {
						state_stack.push(state);
						state = new_state;
					}
				}

				break;
			}
		}
		if (!found || istr == '')
			return out;
	}
}

function highlightToHTML(lex, istr, callback, start) {
	var elems = highlight(lex, istr, start);
	var ostr = '';
	for (var i in elems) {
		var cls = elems[i]['class'];
		var str = elems[i]['str'];
		var span = '<span class="' + elems[i]['class'] + '">' +
			escapeHTML(elems[i]['str']) + '</span>';
		if (typeof callback != 'undefined') {
			ostr += callback(span, cls, str);
		} else {
			ostr += span;
		}
	}
	return ostr;
}

function highlightFunction(func, callback, start) {
	return highlightToHTML({
		start: [
			[/(\s+)/,        ['whitespace']],
			[/(generic)(\s)/,
			                 ['keyword', 'whitespace'], 'generic'],
			[/(\S+)(\s+)(::)/,
			                 ['funcname', 'whitespace', 'punctuation'], 'type'],
			[/(\()(\S+)(\))(\s+)(infix[rl]?)(\s*)(\d*)(\s*)(::)/,
			                 ['punctuation', 'funcname', 'punctuation', 'whitespace',
			                  'keyword', 'whitespace', 'keyword', 'whitespace',
			                  'punctuation']
			                 , 'type'],
			[/(infix[rl])(\s+)(\d*)/,
			                 ['keyword', 'whitespace', 'keyword']],
			[/([\(\)])/,     ['punctuation']],
			[/(::)/,         ['punctuation'], 'type'],
			[/([^\(\)]+)/,   ['funcname']]
		],
		startConstructor: [ // alternative entry point in case this is a constructor
			[/(\s+)/,        ['whitespace']],
			[/(.*)(::)/,     ['constructor', 'punctuation'], 'type']
		],
		startRecordField: [ // alternative entry point in case this is a record field
			[/(\s+)/,        ['whitespace']],
			[/(.*)(::)/,     ['field', 'punctuation'], 'type']
		],
		generic: [
			[/(\s+)/,        ['whitespace']],
			[/(\S+)/,        ['funcname'], 'genericVars']
		],
		genericVars: [
			[/(\s+)/,        ['whitespace']],
			[/(::)/,         ['punctuation'], 'type'],
			[/(\S)/,         ['typevar']]
		],
		type: [
			[/(\s+)/,        ['whitespace']],
			[/([a-z][\w`]*)/, ['typevar']],
			[/(A)(\.)/,      ['keyword', 'punctuation'], 'univars'],
			[/([A-Z]\w*)/,   ['type']],
			[/(\|)/,         ['punctuation'], 'context'],
			[/(\W)/,         ['punctuation']]
		],
		univars: [
			[/(\s+)/,        ['whitespace']],
			[/(\*)/,         ['punctuation']],
			[/(:)/,          ['punctuation'], 'pop'],
			[/([a-z][\w`]*)/, ['typevar']]
		],
		context: [
			[/(\s+)/,        ['whitespace']],
			[/(,)/,          ['punctuation']],
			[/(\[)/,         ['punctuation'], 'attrenv'],
			[/(\S+)(\{\|)/,  ['generic', 'punctuation'], 'genericContext'],
			// These are two hacks for class context in universally quantified types:
			[/(\()/,         ['punctuation'], 'pop'], // hack for sqlShare
			[/(->)/,         ['punctuation'], 'pop'], // hack for sqlExecute
			// End hacks
			[/([^\s{]+)(,)/, ['classname', 'punctuation']],
			[/([^\s{]+)/,    ['classname'], 'contextType']
		],
		genericContext: [
			[/([*>-]+\|\},)/, ['punctuation'], 'pop'],
			[/([*>-]+\|\})/, ['punctuation'], 'contextType']
		],
		contextType: [
			[/(\s+)/,        ['whitespace']],
			[/(,)/,          ['punctuation']],
			[/(&)/,          ['punctuation'], 'pop'],
			[/(\[)/,         ['punctuation'], 'attrenv'],
			[/([\(\[])/,     ['punctuation'], 'contextType'],
			[/([\)\]])/,     ['punctuation'], 'pop'],
			[/([a-z][\w`]*)/, ['typevar']],
			[/([A-Z][\w`]*)/, ['type']],
			[/([^\s\(\)\[\],]+)/, ['typevar']]
		],
		attrenv: [
			[/(\s+)/,        ['whitespace']],
			[/(\w)/,         ['typevar']],
			[/(<=)/,         ['punctuation']],
			[/(,)/,          ['punctuation']],
			[/(\])/,         ['punctuation'], 'pop']
		]
	}, func, callback, start);
}

function highlightTypeDef(type, callback, start) {
	return highlightToHTML({
		start: [
			[/(::)/,         ['punctuation'], 'name']
		],
		name: [
			[/(\s+)/,        ['whitespace']],
			[/(\*)/,         ['punctuation']],
			[/([A-Z][\w`]*)/, ['type'], 'vars'],
			[/([~@#\$%\^\?!\+\-\*<>\\\/\|&=:]+)/, ['type'], 'vars']
		],
		vars: [
			[/(\s+)/,        ['whitespace']],
			[/([a-z][\w`]*)/, ['typevar']],
			[/(\(?:==)/,     ['punctuation'], 'synonym'],
			[/(=)/,          ['punctuation'], 'lhs']
		],
		synonym: [
			[/(\s+)/,        ['whitespace']],
			[/([a-z][a-zA-Z]*)/, ['typevar']],
			[/([A-Z]\w*)/,   ['type']],
			[/(\W)/,         ['punctuation']]
		],
		lhs: [
			[/(\s*)(E)(\.)/, ['whitespace', 'existential', 'punctuation'], 'lhsexi'],
			[/(\s*)(\{)/,    ['whitespace', 'punctuation'], 'record'],
			[/(\s*)/,        ['whitespace'], 'conses']
		],
		lhsexi: [
			[/(\s+)/,        ['whitespace']],
			[/([a-z][\w`]*)/, ['typevar']],
			[/(:)/,          ['punctuation'], 'lhs']
		],
		record: [
			[/(\s+)/,        ['whitespace']],
			[/([_a-z][\w`]*)(\s+)(::)/,
			                 ['field', 'whitespace', 'punctuation'],
			                 'fieldtype'],
			[/(\})/,         ['punctuation']]
		],
		fieldtype: [
			[/(\s+)/,        ['whitespace']],
			[/([a-z][a-zA-Z]*)/, ['typevar']],
			[/([A-Z]\w*)/,   ['type']],
			[/(\()/,         ['punctuation'], 'tuple'],
			[/([\[\{])/,     ['punctuation'], 'fieldtype'],
			[/([\]\},])/,    ['punctuation'], 'pop'],
			[/(\W)/,         ['punctuation']]
		],
		tuple: [
			[/(\s+)/,        ['whitespace']],
			[/([a-z][a-zA-Z]*)/, ['typevar']],
			[/([A-Z]\w*)/,   ['type']],
			[/([\(\[\{])/,   ['punctuation'], 'tuple'],
			[/([\)\]\}])/,   ['punctuation'], 'pop'],
			[/(\W)/,         ['punctuation']]
		],
		conses: [
			[/(\s+)/,        ['whitespace']],
			[/(E)(\.)/,      ['existential', 'punctuation'], 'consexi'],
			[/([_A-Z][\w`]*)/, ['constructor'], 'consargs'],
			[/([~@#\$%\^\?!\+\-\*<>\\\/\|&=:]+)/, ['constructor'], 'consargs'],
			[/(\.\.)/,       ['punctuation']]
		],
		consexi: [
			[/(\s+)/,        ['whitespace']],
			[/([a-z][\w`]*)/, ['typevar']],
			[/(:)/,          ['punctuation'], 'conses']
		],
		consargs: [
			[/(\s+)/,        ['whitespace']],
			[/([a-z][\w`]*)/, ['typevar']],
			[/([A-Z]\w*)/,   ['type']],
			[/(\|)/,         ['punctuation'], 'conses'],
			[/(\W)/,         ['punctuation']]
		]
	}, type, callback, start);
}

function highlightClassDef(cls, callback, start) {
	return highlightToHTML({
		start: [
			[/(\s+)/,        ['whitespace']],
			[/(class)/,      ['keyword'], 'className'],
			[/(where)/,      ['keyword']],
			[/([a-z][\w`]*)/, ['typevar']],
			[/(\|)/,         ['punctuation'], 'context']
		],
		className: [
			[/(\s+)/,        ['whitespace']],
			[/(\S+)/,        ['classname'], 'start']
		],
		context: [
			[/(where)/,      ['keyword']],
			[/(\s+)/,        ['whitespace']],
			[/(,)/,          ['punctuation']],
			[/(\S+)(\{\|)/,  ['generic', 'punctuation'], 'generic'],
			[/([^\s{]+)(,)/, ['classname', 'punctuation']],
			[/([^\s{]+)/,    ['classname'], 'contextType']
		],
		generic: [
			[/([*>-]+\|\},)/, ['punctuation'], 'pop'],
			[/([*>-]+\|\})/, ['punctuation'], 'contextType']
		],
		contextType: [
			[/(where)/,      ['keyword']],
			[/(\s+)/,        ['whitespace']],
			[/([,&])/,       ['punctuation'], 'context'],
			[/([^\s,]+)/,    ['typevar']]
		]
	}, cls, callback, start);
}

function highlightMacro(macro, callback, start) {
	var myCallback = function(span, cls, str) {
		if (cls == '__type__') {
			return highlightFunction(str, callback);
		}
		return callback(span, cls, str);
	}

	return highlightToHTML({
		start: [
			[/(\s+)/,        ['whitespace']],
			[/(\(.+\)\s+infix.*)/,
			                 ['__type__']],
			[/(\()(\S+)(\))/, ['punctuation', 'funcname', 'punctuation'], 'args'],
			[/(\S+)/,        ['funcname'], 'args']
		],
		args: [
			[/(\s+)/,        ['whitespace']],
			[/(:==)/,        ['punctuation'], 'rhs'],
			[/(\S+)/,        ['funcname funcname-onlyused']]
		],
		rhs: [
			[/(\s+)/,        ['whitespace']],
			[/\b(if|let|in|with|where|case|of|otherwise)\b/,
			                 ['keyword']],
			[/('[\w`]+'\.)/, ['qualifiedname']],
			[/('([^'\\]|\\(x[0-9a-fA-F]+|\d+|.))')/,
			                 ['literal literal-char']],
			[/\b([+~-]?0[0-7]+)\b/,
			                 ['literal literal-int literal-int-oct']],
			[/\b([+~-]?\d+)\b/,
			                 ['literal literal-int literal-int-dec']],
			[/\b([+~-]?0x[\da-fA-F]+)\b/,
			                 ['literal literal-int literal-int-hex']],
			[/\b([+~-]?\d+\.\d+(E[+-]?\d+)?)\b/,
			                 ['literal literal-real']],
			[/\b(True|False)\b/,
			                 ['literal literal-bool']],
			[/(")/,          ['literal literal-string'], 'string'],
			[/(\(.+\)\s+infix.*)/,
			                 ['__type__']],
			[/([\w`]+\s*::.*)/,
			                 ['__type__']],
			[/([A-Z][\w`]*)/,['constructor']],
			[/\b(_)\b/,      ['argument argument-wildcard']],
			[/([\w`]+)/,     ['funcname funcname-onlyused']],
			[/(.)/,          ['punctuation']]
		],
		string: [
			[/(")/,          ['literal literal-string'], 'pop'],
			[/(\\.)/,        ['literal literal-string']],
			[/([^\\"]+)/,    ['literal literal-string']]
		]
	}, macro, myCallback, start);
}

function highlightType(type, callback) {
	return highlightFunction(type, callback, 'type');
}

function escapeHTML(unsafe) {
	var map = { "&": "&amp;", "<": "&lt;", ">": "&gt;",
		'"': '&quot;', "'": '&#39;', "/": '&#x2F;' };
	return String(unsafe).replace(/[&<>"'\/]/g, function(s){return map[s];});
}
