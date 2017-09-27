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
