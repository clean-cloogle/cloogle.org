function pluralise(n, what) {
	return n + ' ' + what + (n == 1 ? '' : 's');
}

function toggleElement(e, className) {
	e.classList.toggle(typeof className == 'undefined' ? 'visible' : className);
}

function toggle(toggler, open) {
	var e = toggler;
	while (!e.classList.contains('toggle-container'))
		e = e.parentNode;
	var es = e.getElementsByClassName('togglee');

	for (var i = 0; i < es.length; i++) {
		var p = es[i];
		while (!p.classList.contains('toggle-container'))
			p = p.parentNode;
		if (p != e)
			continue;

		if (typeof open == 'undefined')
			toggleElement(es[i], 'toggle-visible');
		else if (open)
			es[i].classList.add('toggle-visible');
		else
			es[i].classList.remove('toggle-visible');
	}

	var icons = e.getElementsByClassName('toggle-icon');
	for (var i = 0; i < icons.length; i++) {
		var p = es[i];
		while (!p.classList.contains('toggle-container'))
			p = p.parentNode;
		if (p != e)
			continue;

		if (typeof open == 'undefined')
			icons[i].innerHTML = icons[i].innerHTML == '\u229e' ? '&#x229f' : '&#x229e';
		else
			icons[i].innerHTML = open ? '&#x229f' : '&#x229e';
	}
}

function toggleById(name) {
	toggleElement(document.getElementById(name));
}

function shortenURL(type, url, onUpdate) {
	share_button.innerHTML = "Contacting cloo.gl server";
	onUpdate('update', 'Contacting cloo.gl server');

	var xmlHttp = new XMLHttpRequest();
	xmlHttp.onreadystatechange = function () {
		if (xmlHttp.readyState == 4) {
			var type = xmlHttp.status == 200 ? 'success' : 'error';
			onUpdate(type, xmlHttp.responseText);
		}
	};
	xmlHttp.open('POST', 'https://cloo.gl', true);
	xmlHttp.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
	xmlHttp.send('type=' + type + '&url=' + encodeURIComponent(url));
}

function highlightQuery(query) {
	var highlighter = highlightFunction;
	if (query == 'class' || query == 'type' || query == 'using') {
		return '<span class="keyword">' + query + '</span>';
	} else if (query.match(/^class\s/)) {
		highlighter = highlightFunction;
	} else if (query.match(/^type\s/)) {
		highlighter = function(q) {
			return '<span class="keyword">type</span><span class="whitespace">' + q.substring(4,5) + '</span>' +
				highlightType(q.substring(5));
		};
	} else if (query.match(/^using\s/)) {
		highlighter = function(q) {
			return '<span class="keyword">using</span><span class="whitespace">' + q.substring(5,6) + '</span>' +
				highlightToHTML({
					start: [
						[/(,)/,     ['punctuation']],
						[/([^,]+)/, ['funcname']],
					]
				}, q.substring(6));
		};
	}
	return highlighter(query);
}

var banners = document.getElementsByClassName('banner');
for (var i = 0; i < banners.length; i++) {
	var banner = banners[i];
	banner.dataset.index = i;

	var id = banner.dataset.id;
	var hidden = document.cookie.replace(/(?:(?:^|.*;\s*)hidden_banners\s*\=\s*([^;]*).*$)|^.*$/, "$1");
	if (hidden.split(' ').indexOf(id) >= 0)
		continue;

	var from = new Date(banner.dataset.from);
	var until = new Date(banner.dataset.until);
	var now = new Date();
	if (from <= now && now <= until)
		banner.style.display = 'block';

	var hidelink = document.createElement('a');
	hidelink.classList.add('hidelink');
	hidelink.innerHTML = '&times;';
	hidelink.setAttribute('href', '#');
	hidelink.setAttribute('title', 'Hide this banner forever');
	hidelink.onclick = function() {
		banner.remove();
		document.cookie = 'hidden_banners=' + hidden + ' ' + id;
	}
	banner.appendChild(hidelink);
}
