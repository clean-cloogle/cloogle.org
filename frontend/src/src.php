<?php
define('CLEANHOME', '/opt/clean');

if (empty($_REQUEST['mod'])) :
	echo '<p>Select a module on the left.</p>';
else :

$mod = preg_replace('/[^\\w\\/\\. -]/', '', $_REQUEST['mod']);

$iclordcl = isset($_REQUEST['icl']) ? 'icl' : 'dcl';
$hl_lines = isset($_REQUEST['line']) ? escapeshellarg($_REQUEST['line']) : '';

$fname = CLEANHOME . '/lib/' . $mod . '.' . $iclordcl;
$efname = escapeshellarg($fname);

system("python3 cloogle_pygments.py $efname $hl_lines");

endif;
