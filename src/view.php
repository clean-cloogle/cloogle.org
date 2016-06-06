<?php
define('CLEANHOME', '/opt/clean');
error_reporting(E_ALL);
ini_set('display_errors', 1);

if (!isset($_REQUEST['lib']) || !isset($_REQUEST['mod'])) {
	die('Add ?lib and ?mod.');
}

$iclordcl = isset($_REQUEST['icl']) ? 'icl' : 'dcl';

$highlight = isset($_REQUEST['hl']) ? true : false;

$lib = preg_replace('/[^\\w\\/\\-]/', '', $_REQUEST['lib']);
$mod = str_replace('.', '/', $_REQUEST['mod']);
$mod = preg_replace('/[^\\w\\/]/', '', $mod);

$fname = CLEANHOME . '/lib/' . $lib . '/' . $mod . '.' . $iclordcl;
$efname = escapeshellarg($fname);

if ($highlight) {
	$out = [];
	exec('pygmentize -l clean -f html -O full -O linenos ' . $efname, $out);
	$out = array_filter($out, function($str) { return $str != '<h2></h2>'; });
	echo implode("\n", $out);
} else {
	header('Content-Type: text/plain');
	echo file_get_contents($fname);
}
