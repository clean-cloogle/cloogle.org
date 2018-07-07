Element.prototype.documentOffsetTop = function() {
	return this.offsetTop +
		(this.offsetParent ? this.offsetParent.documentOffsetTop() : 0);
};

function extend(obj, def) {
	for (var prop in def) {
		if (!def.hasOwnProperty(prop))
			continue;
		if (obj.hasOwnProperty(prop))
			continue;
		obj[prop] = def[prop];
	}
	return obj;
}

Element.prototype.browser = function(opts) {
	if (typeof opts === 'undefined')
		opts = {};
	opts = extend(opts, {
		newPath:  function (path) {},
		newHash:  function (hash) {},
		newState: function () {},
		getUrl:   function () { return ''; },
		onLoad:   function (state, response) {},
		onError:  function (state, response, code) {},
		state:    {},
		viewer:   null
	});

	var root = this;
	var state = opts.state;

	var triggerChange = function(update) {
		opts.newState();

		if (typeof update != 'undefined' && !update)
			return;

		if (opts.viewer != null)
			opts.viewer.innerHTML = '<p id="loading">Loading...</p>';

		var url = opts.getUrl();

		var xmlHttp = new XMLHttpRequest();
		xmlHttp.onreadystatechange = function() { 
			if (xmlHttp.readyState == 4) {
				if (opts.viewer != null)
					opts.viewer.innerHTML = xmlHttp.response;
				if (xmlHttp.status == 200)
					opts.onLoad(state, xmlHttp.response);
				else
					opts.onError(state, xmlHttp.response, xmlHttp.status);
			}
		}
		xmlHttp.open("GET", url, true);
		xmlHttp.send(null);
	}

	togglers = this.getElementsByClassName('toggler');
	for (var i = 0; i < togglers.length; i++) {
		togglers[i].onclick = function() {
			toggle(this);
		};
	}

	items = this.getElementsByClassName('module');
	for (var i = 0; i < items.length; i++) {
		items[i].onclick = function(ev) {
			ev.preventDefault();
			ev.stopPropagation();

			var old = root.getElementsByClassName('active');
			for (var i = 0; i < old.length; i++)
				old[i].classList.remove('active');

			var e = this;
			e.classList.add('active');
			var path = [];
			while (e != root) {
				if ('name' in e.dataset)
					path.unshift(e.dataset.name)
				e = e.parentNode;
			}

			opts.newPath(path);
			triggerChange();

			return false;
		}
	}

	window.onhashchange = function() {
		opts.newHash(decodeURIComponent(window.location.hash.substring(1)));
	};

	return {
		state: state,
		triggerChange: triggerChange,
		openPath: function(path) {
			var old = root.getElementsByClassName('active');
			for (var i = 0; i < old.length; i++)
				old[i].classList.remove('active');

			var e = root;
			for (var i = 0; i < path.length && 'childNodes' in e; i++) {
				var children = e.childNodes;
				for (var k = 0; k < children.length; k++) {
					if (children[k].dataset.name == path[i]) {
						if (i < path.length - 1) {
							toggle(children[k], true);
							e = children[k].childNodes[1];
							break;
						} else if (!children[k].classList.contains('directory')) {
							children[k].classList.add('active');
							break;
						}
					}
				}
			}
		},
		openTo: function(elem) {
			if (elem == null)
				return;
			var path = [];
			if (!elem.classList.contains('directory'))
				elem.classList.add('active');
			while (elem != root && elem != null) {
				if (elem.classList.contains('toggle-container'))
					toggle(elem, true);
				elem = elem.parentNode;
			}
		},
		open: function() {
			opts.newHash(decodeURIComponent(window.location.hash.substring(1)));
		},
		scrollTo: function(element) {
			if (typeof element == 'undefined') {
				var to = opts.viewer.documentOffsetTop();
			} else {
				var to = element.documentOffsetTop() - window.innerHeight / 4;
			}

			if (opts.viewer != null) {
				if (opts.viewer.scrollHeight > opts.viewer.clientHeight) {
					viewer.scrollTop = to;
				} else {
					window.scrollTo(0, to);
				}
			}
		},
		setHash: function(hash) {
			window.location.hash = '#' + hash;
		}
	};
}
