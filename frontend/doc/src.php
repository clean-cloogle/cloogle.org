<?php
define('CLEANHOME', '/opt/clean');
error_reporting(0);

$loc = $_REQUEST['loc'] ?: 'CleanRep.2.2_1.htm;jump=_Toc311797959';
$loc = preg_replace('/\.\.+/', '.', $loc);
$loc = preg_replace('/[^\w\d.;=]+/', '', $loc);

$match = [];
$file = preg_match('/(.+);jump=(.+)/', $loc, $match) > 0 ? $file = $match[1] : $loc;

$doc = new DOMDocument;
$doc->loadHTMLFile(CLEANHOME . '/doc/CleanLangRep/' . $file);

function transformLink($orgfile, $a) {
	$href = $a->getAttribute('href');
	if (substr($href, 0, 4) == 'http') {
		$a->setAttribute('target', '_blank');
	} else {
		$match = [];
		if (preg_match('/^(.*)#(.*)$/', $href, $match) == 0)
			$a->setAttribute('href', '#' . $href);
		$file = $match[1] != '' ? $match[1] : $orgfile;
		$hash = $match[2];
		$a->setAttribute('href', '#' . $file . ';jump=' . $hash);
	}
}

foreach ($doc->getElementsByTagName('a') as $a) {
	transformLink($file, $a);
}

echo $doc->saveHtml($doc);
