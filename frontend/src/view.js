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

window.onload = function() {
	share_button = document.getElementById('share-button');
	var viewer = document.getElementById('viewer');
	var icl = document.getElementById('icl');

	var selectLine = function() {
		var elem = this;

		if (typeof elem != 'number')
			elem = parseInt(elem.innerText);

		if (isNaN(elem)) {
			browser.state.line = null;
			return;
		}

		browser.state.line = elem;

		var hll = document.getElementsByClassName('hll');
		for (var i = 0; i < hll.length; i++)
			hll[i].parentNode.innerHTML = hll[i].innerHTML;

		var linespan = document.getElementById('line-' + browser.state.line);
		linespan.innerHTML = '<span class="hll">' + linespan.innerHTML + '</span>';

		browser.triggerChange(false);
	}

	var bindLinenos = function() {
		var linenos = viewer.getElementsByClassName('special');
		for (var i = 0; i < linenos.length; i++)
			linenos[i].onclick = selectLine;
	}

	var browser = document.getElementsByClassName('browser')[0].browser({
		newPath: function (path) {
			this.state.mod = path.join('/');
			this.state.line = null;
			this.newState();
		},
		newHash: function (hash) {
			var hashelems = hash.split(';');
			this.state.mod = hashelems[0];
			this.state.icl = false;
			this.state.line = null;
			for (var i = 1; i < hashelems.length; i++) {
				if (hashelems[i] == 'icl')
					icl.checked = this.state.icl = true;
				else if (hashelems[i].substring(0,5) == 'line=')
					this.state.line = hashelems[i].substring(5);
			}

			browser.openPath(this.state.mod.split('/'));
		},
		newState: function () {
			var hash = this.state.mod;
			if (this.state.icl)
				hash += ';icl';
			if (this.state.line != null)
				hash += ';line=' + this.state.line;
			document.location.hash = '#' + hash;
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
		onLoad: function(state) {
			viewer.scrollLeft = 0;
			if (state.line != null) {
				var l = document.getElementById('line-' + state.line).documentOffsetTop();
				viewer.scrollTop = l - window.innerHeight/4;
			} else {
				viewer.scrollTop = 0;
			}

			var modparts = state.mod.split('/');
			var lib = modparts.shift();
			document.title = modparts.join('.') + ' (' + lib + ') - Cloogle library browser';

			bindLinenos();
			restoreShareUI();
		},
		state: {
			icl: false
		}
	});
	browser.open();

	icl.onchange = function() {
		browser.state.icl = this.checked;
		browser.triggerChange();
	};
	icl.onchange();
};
