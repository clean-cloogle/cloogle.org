var sidebar = null;
var viewer = null;
var libselect = null;
var icl = null;

var curmod = null;
var refresh_on_hash = true;
var line = null;

function loadModule(elem) {
	if (typeof elem != 'undefined') {
		line = null;
		curmod = elem.dataset.module;
	}

	updateHash();
	updateLibraryPanel();

	viewer.innerHTML = '<p id="loading">Loading...</p>';

	var url = 'src.php';
	url += '?lib=' + libselect.value;
	url += '&mod=' + curmod;
	if (icl.checked)
		url += '&icl';
	var hashelems = decodeURIComponent(window.location.hash.substring(1)).split(';');
	for (var i in hashelems)
		if (hashelems[i].substring(0,5) == 'line=')
			url += '&line=' + hashelems[i].substring(5);

	var xmlHttp = new XMLHttpRequest();
	xmlHttp.onreadystatechange = function() { 
		if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
			viewer.innerHTML = xmlHttp.response;
			if (line != null)
				document.getElementById('line-' + line).scrollIntoView(true);
		}
	}
	xmlHttp.open("GET", url, true);
	xmlHttp.send(null);
}

function updateLibraryPanel() {
	var mods = document.getElementsByClassName('module');
	for (var i = 0; i < mods.length; i++)
		mods[i].style.fontStyle = '';

	var e = document.getElementById('sidebar');
	var modpath = curmod.split('.');
	for (var m = 0; m < modpath.length - 1; m++) {
		var dirs = e.getElementsByClassName('directory');
		for (var d = 0; d < dirs.length; d++) {
			var t = dirs[d].getElementsByClassName('title')[0];
			if (t.innerHTML == modpath[m]) {
				e = dirs[d];
				toggle(e.getElementsByClassName('toggler')[0], true);
			}
		}
	}

	var modules = e.getElementsByClassName('module');
	for (var m = 0; m < modules.length; m++)
		if (modules[m].innerHTML == modpath[modpath.length-1])
			modules[m].style.fontStyle = 'italic';
}

function updateHash() {
	var newhash = curmod;
	if (icl.checked)
		newhash += ';icl';
	if (line != null)
		newhash += ';line=' + line;

	if (window.location.hash.substring(1) != encodeURIComponent(newhash)) {
		refresh_on_hash = false;
		window.location.hash = '#' + newhash;
	}
}

window.onhashchange = function() {
	if (!refresh_on_hash) {
		refresh_on_hash = true;
	} else {
		var elems = decodeURIComponent(window.location.hash.substring(1)).split(';');
		curmod = elems[0];
		icl.checked = elems.indexOf('icl') != -1;
		for (var i in elems)
			if (elems[i].substring(0,5) == 'line=')
				line = elems[i].substring(5);
		loadModule();
	}
}

window.onload = function () {
	sidebar = document.getElementById('sidebar');
	viewer = document.getElementById('viewer');
	libselect = document.getElementById('select-lib');
	icl = document.getElementById('icl');

	if (window.innerWidth > 800) {
		var height = window.innerHeight;
		sidebar.style.height = (height - 20) + 'px';
		viewer.style.height = height + 'px';
	}

	libselect.onchange = function() {
		window.location.href = 'view.php?lib=' + this.value;
	}

	icl.onchange = function() {
		line = null;
		loadModule();
	}

	window.onhashchange();
}
