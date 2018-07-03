var share_button;

function restoreShareUI() {
	share_button.disabled = false;
	share_button.type = 'button';
	share_button.value = 'Share';
}

function shareButtonClick() {
	if (share_button.value != 'Share')
		return;

	share_button.disabled = true;
	var url = window.location.pathname.substring(1) + window.location.search + window.location.hash;
	shortenURL('cloogle', url, function (type, msg) {
		share_button.value = msg;
		if (type == 'success') {
			share_button.type = 'text';
			share_button.disabled = false;
			share_button.select();
		}
	});
}

function selectLine(n) {
	browser.state.line = n;

	if (!isNaN(n)) {
		var hll = document.getElementsByClassName('hll');
		for (var i = 0; i < hll.length; i++)
			hll[i].classList.remove('hll');

		var linespan = document.getElementById('line-' + browser.state.line);
		if (linespan != null)
			linespan.classList.add('hll');
	}

	browser.triggerChange(false);
}

var tableLineNo = null;
function tableHighlightCallback(span, cls, str) {
	var html = '';
	if (tableLineNo == null) {
		html = '<tr id="line-1"><td onclick="selectLine(1);">1</td><td>';
		tableLineNo = 1;
	}
	var lines = str.split('\n');
	for (var i = 0; i < lines.length-1; i++) {
		tableLineNo++;
		html += highlightCallback(
				'<span class="' + cls + '">' + escapeHTML(lines[i]) + '</span>',
				cls, lines[i]) +
			'</td></tr><tr id="line-' + tableLineNo + '"><td onclick="selectLine(' + tableLineNo + ');">'
				+ tableLineNo + '</td><td>';
	}
	html += highlightCallback(
		'<span class="' + cls + '">' + escapeHTML(lines[lines.length-1]) + '</span>',
		cls, lines[lines.length-1]);
	return html;
}

var browser = null;
window.onload = function() {
	share_button = document.getElementById('share-button');
	var viewer = document.getElementById('viewer');
	var icl = document.getElementById('icl');

	browser = document.getElementsByClassName('browser')[0].browser({
		newPath: function (path) {
			this.state.mod = path.join('/');
			this.state.line = null;
			this.newState();
		},
		newHash: function (hash) {
			var hashelems = hash.split(';');
			var update = this.state.mod != hashelems[0];
			this.state.mod = hashelems[0];
			this.state.icl = false;
			this.state.line = null;
			for (var i = 1; i < hashelems.length; i++) {
				if (hashelems[i] == 'icl')
					icl.checked = this.state.icl = true;
				else if (hashelems[i].substring(0,5) == 'line=')
					this.state.line = hashelems[i].substring(5);
			}

			if (this.state.line != null)
				selectLine(this.state.line);

			browser.openPath(this.state.mod.split('/'));
			browser.triggerChange(update);
		},
		newState: function () {
			var hash = this.state.mod;
			if (this.state.icl)
				hash += ';icl';
			if (this.state.line != null)
				hash += ';line=' + this.state.line;

			browser.setHash(hash);
		},
		getUrl: function () {
			var url = 'src.php?mod=' + this.state.mod;
			if (this.state.icl)
				url += '&icl';
			if (this.state.line != null)
				url += '&line=' + this.state.line;
			return url;
		},
		viewer: viewer,
		onLoad: function(state, text) {
			tableLineNo = null;
			viewer.innerHTML = '<table class="source-code">' +
					highlightClean(text, tableHighlightCallback) +
				'</table>';
			viewer.scrollLeft = 0;
			if (state.line != null) {
				selectLine(state.line);
				browser.scrollTo(document.getElementById('line-' + state.line));
			} else {
				browser.scrollTo();
			}

			var modparts = state.mod.split('/');
			if (modparts.length > 1) {
				var lib = modparts.shift();
				document.title = modparts.join('.') + ' (' + lib + ') - Cloogle library browser';
			}

			restoreShareUI();
		},
		state: {
			icl: false
		}
	});

	browser.state.icl = icl.checked;
	icl.onchange = function() {
		browser.state.icl = this.checked;
		browser.state.line = null;
		browser.triggerChange();
	};

	browser.open();
};
