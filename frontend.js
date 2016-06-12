var form_str = document.getElementById('search_str');
var sform = document.getElementById('search_form');
var sresults = document.getElementById('search_results');

function toggle(name) {
	var e = document.getElementById(name);
	if (e.style.display=="block")
		e.style.display="none";
	else
		e.style.display="block";
}

function formsubmit() {
	if (form_str.value === '') {
		sresults.innerHTML = 'Can\'t search for the empty string';
	} else {
		sresults.innerHTML = '<div id="page-0"></div>';
		getResults(form_str.value, 0);
	}
	return false;
};

window.onload = function() {
	sform.onsubmit = formsubmit;
	var str = decodeURIComponent(document.location.hash);
	if(str !== ''){
		str = str.substring(1);
		form_str.value = decodeURIComponent(str);
		formsubmit();
	}

	document.getElementById('search_str').focus();

	var caretOffset = 0;
	
	function updateCaret() {
		var search = form_str;
		var caret = document.getElementById('caret');
		var caretOffset = search.selectionStart;
		if (search.selectionStart != search.selectionEnd) {
			caret.style.display = 'none';
		} else {
			caret.style.marginLeft = caretOffset * 7 + 'px';
			caret.style.display = 'inline';
		}
	}
	
	form_str.onfocus = updateCaret;
	form_str.onclick = updateCaret;
	
	form_str.onkeypress = function() {
		var caret = '<span id="caret"></span>';
		this.style.width = Math.max(240, this.value.length * 7 + 7) + 'px';
		document.getElementById('search_str_highlight').innerHTML =
			caret + highlightFunction(this.value);
		updateCaret();
	}
	form_str.onkeyup = form_str.onkeypress;
	form_str.onchange = form_str.onkeypress;
	form_str.onkeypress();
	
	form_str.onblur = function() {
		document.getElementById('caret').style.display = 'none';
	}
	form_str.onfocus = function() {
		document.getElementById('caret').style.display = 'inline';
	}
}

window.onhashchange = function() {
	if (!refresh_on_hash) {
		refresh_on_hash = true;
	} else {
		var str = decodeURIComponent(document.location.hash);
		form_str.value = str.substring(1);
		formsubmit();
	}
}
