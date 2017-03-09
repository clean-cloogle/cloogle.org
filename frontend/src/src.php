<?php
define('CLEANHOME', '/opt/clean');

if (!isset($_REQUEST['lib'])) :
	echo '<p>Add ?lib and ?mod.</p>';
elseif (!isset($_REQUEST['mod'])) :
	echo '<p>Select a module on the left.</p>';
else :

$lib = preg_replace('/[^\\w\\/\\-]/', '', $_REQUEST['lib']);
$mod = preg_replace('/[^\\w\\/\\.]/', '', $_REQUEST['mod']);
$mod = str_replace('.', '/', $mod);

$iclordcl = isset($_REQUEST['icl']) ? 'icl' : 'dcl';
$hl_lines = isset($_REQUEST['line']) ? escapeshellarg($_REQUEST['line']) : '';

$fname = CLEANHOME . '/lib/' . $lib . '/' . $mod . '.' . $iclordcl;
$efname = escapeshellarg($fname);

system("python3 cloogle_pygments.py $efname $hl_lines");

endif;
