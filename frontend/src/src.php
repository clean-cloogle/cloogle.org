<?php
define('CLEAN_HOME', '/opt/clean');

if (empty($_REQUEST['mod'])) {
	http_response_code(400);
	echo '<p>Select a module on the left.</p>';
	exit();
}

$mod = preg_replace('/[^\\w\\/\\. -`]/', '', $_REQUEST['mod']);
$iclordcl = isset($_REQUEST['icl']) ? 'icl' : 'dcl';
$fname = CLEAN_HOME . '/lib/' . $mod . '.' . $iclordcl;

if (!is_readable($fname)) {
	http_response_code(404);
	if ($iclordcl == 'dcl' && is_readable(substr_replace($fname, 'icl', strlen($fname)-3))) {
		echo '<p>There is no definition module. Tick "Show implementation" to see the implementation module.</p>';
	} else {
		echo '<p><code>' . $fname . '</code> does not exist.</p>';
		echo '<p>If you believe this is an error, please report it <a href="https://github.com/clean-cloogle/cloogle.org/issues/new">on GitHub</a>.';
	}
	exit();
}

echo file_get_contents($fname);
