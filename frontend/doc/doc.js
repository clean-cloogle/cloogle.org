window.onload = function() {
	var viewer = document.getElementById('viewer');

	var browser = document.getElementsByClassName('browser')[0].browser({
		newPath: function (path) {
			path = path[0].split(';');
			this.state.loc = path[0];
			this.state.jump = null;
			for (var i = 1; i < path.length; i++)
				if (path[i].substring(0,5) == 'jump=')
					this.state.jump = path[i].substring(5);
			this.newState();
		},
		newHash: function (hash) {
			var hashelems = hash.split(';');
			this.state.loc = hashelems[0];
			this.state.jump = null;
			for (var i = 1; i < hashelems.length; i++)
				if (hashelems[i].substring(0,5) == 'jump=')
					this.state.jump = hashelems[i].substring(5);
			this.newState();
			browser.openTo(document.getElementById('doc-' + hash));
		},
		newState: function () {
			var hash = this.state.loc;
			if (this.state.jump != null)
				hash += ';jump=' + this.state.jump;
			document.location.hash = '#' + hash;
		},
		getUrl: function () {
			return 'src.php?loc=' + encodeURIComponent(this.state.loc);
		},
		viewer: viewer,
		onLoad: function(state) {
			if (state.jump != null) {
				var l = document.getElementsByName(state.jump)[0].documentOffsetTop();
				viewer.scrollTo(0, l - window.innerHeight/8);
			}
		},
		state: {
			jump: false
		}
	});
	browser.open();
	browser.triggerChange();

	var sidebar = document.getElementById('sidebar');
	var viewer = document.getElementById('viewer');
	if (window.innerWidth > 800) {
		var height = window.innerHeight;
		sidebar.style.height = (height - 20) + 'px';
		viewer.style.height = height + 'px';
	}

	window.onhashchange = function () {
		browser.open();
		browser.triggerChange();
	};
};
