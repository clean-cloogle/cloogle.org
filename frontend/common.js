function pluralise(n, what) {
	return n + ' ' + what + (n == 1 ? '' : 's');
}

function toggleElement(e) {
	e.style.display = e.style.display == 'block' ? 'none' : 'block';
}

function toggle(toggler) {
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

		toggleElement(es[i]);
	}

	var icons = e.getElementsByClassName('toggle-icon');
	for (var i = 0; i < icons.length; i++) {
		var p = es[i];
		while (!p.classList.contains('toggle-container'))
			p = p.parentNode;
		if (p != e)
			continue;

		switch (icons[i].innerHTML) {
			case '\u229e': icons[i].innerHTML = '&#x229f;'; break;
			case '\u229f': icons[i].innerHTML = '&#x229e;'; break;
		}
	}
}

function toggleById(name) {
	toggleElement(document.getElementById(name));
}
