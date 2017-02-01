<?php
define('CLEANHOME', '/opt/clean');
error_reporting(E_ALL);
ini_set('display_errors', 1);

if (!isset($_REQUEST['lib']) || !isset($_REQUEST['mod'])) {
	die('Add ?lib and ?mod.');
}

$iclordcl = isset($_REQUEST['icl']) ? 'icl' : 'dcl';
$highlight = isset($_REQUEST['hl']) ? true : false;
$hl_lines = isset($_REQUEST['line']) ? escapeshellarg($_REQUEST['line']) : '';

$lib = preg_replace('/[^\\w\\/\\-]/', '', $_REQUEST['lib']);
$mod = str_replace('.', '/', $_REQUEST['mod']);
$mod = preg_replace('/[^\\w\\/]/', '', $mod);

$fname = CLEANHOME . '/lib/' . $lib . '/' . $mod . '.' . $iclordcl;
$efname = escapeshellarg($fname);

if ($highlight) {
	# Output is printed to stdout
	exec("python2 cloogle_pygments.py $hl_lines < $efname");
} else {
	header('Content-Type: text/plain');
	echo file_get_contents($fname);
}
