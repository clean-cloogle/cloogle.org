function highlight(type) {
	str = '';

	var lex = {
		start: [
			[/^(\s+)(.*)/, ['whitespace']],
			[/^(.*)(::.*)/, ['funcname'], 'type'],
			[/^(\S+)(.*)/, ['funcname']]
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
