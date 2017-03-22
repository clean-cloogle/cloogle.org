function pluralise(n, what) {
	return n + ' ' + what + (n == 1 ? '' : 's');
}

function toggleElement(e) {
	e.classList.toggle('visible');
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
			toggleElement(es[i]);
		else
			es[i].style.display = open ? 'block' : 'none';
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
