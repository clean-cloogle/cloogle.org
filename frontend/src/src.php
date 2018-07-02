<?php
define('CLEAN_HOME', '/opt/clean');
header('Content-Type: text/plain');

if (empty($_REQUEST['mod'])) {
	echo '<p>Select a module on the left.</p>';
	exit();
}

$mod = preg_replace('/[^\\w\\/\\. -`]/', '', $_REQUEST['mod']);
$iclordcl = isset($_REQUEST['icl']) ? 'icl' : 'dcl';
$fname = CLEAN_HOME . '/lib/' . $mod . '.' . $iclordcl;

echo file_get_contents($fname);
