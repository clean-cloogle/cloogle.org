<?php
define('CLEANHOME', '/opt/clean');

if (!isset($_REQUEST['lib'])) :
	echo 'Choose a library.';
else :

$lib = preg_replace('/[^\\w\\/\\-]/', '', $_REQUEST['lib']);

$ignored_files = [
	'_startup',
	'_library',
	'_startupProfile',
	'_startupTrace',
	'_system'];

function containsModules($dir) {
	global $ignored_files;
	try {
		$d = new DirectoryIterator($dir);
		foreach ($d as $f) {
			if ($f->isDot())
				continue;
			if ($f->isDir() && containsModules($dir . '/' . $f->getFilename()))
				return true;
			else if ($f->isFile() && $f->getExtension() == 'dcl' &&
					!in_array($f->getBasename('.dcl'), $ignored_files))
				return true;
		}
	} catch (Exception $e) { }
	return false;
}

function getDirsAndModules($dir) {
	global $ignored_files;
	global $ignored_dirs;
	$ds = [];
	$ms = [];

	try {
		$d = new DirectoryIterator($dir);
		foreach ($d as $f) {
			if ($f->isDot())
				continue;
			if ($f->isDir() && containsModules($dir . '/' . $f->getFilename()))
				$ds[] = $f->getFilename();
			else if ($f->getExtension() == 'dcl' &&
					!in_array($f->getBasename('.dcl'), $ignored_files))
				$ms[] = $f->getBasename('.dcl');
		}

		sort($ds);
		sort($ms);
	} catch (Exception $e) {
		echo 'Failed to get directory ' . $dir;
	}

	return ['dirs' => $ds, 'modules' => $ms];
}

function makeBrowser($dir, $basemodule) {
	echo '<div class="browser togglee">';
	$elems = getDirsAndModules($dir);

	foreach ($elems['dirs'] as $d) {
		echo '<div class="browser-item directory toggle-container">' .
				'<span class="toggler" onclick="toggle(this)">' .
					'<span class="toggle-icon">&#x229e</span>' .
					'<span class="title">' . $d . '</span></span>';
		makeBrowser($dir . '/' . $d, $basemodule . $d . '.');
		echo '</div>';
	}

	foreach ($elems['modules'] as $m) {
		$fullm = $basemodule . $m;
		echo '<div class="browser-item module" onclick="loadModule(this)" ' .
					'data-module="' . $fullm . '">' . $m . '</span>' .
			'</div>';
	}

	echo '</div>';
}

$dname = CLEANHOME . '/lib/' . $lib;

makeBrowser($dname, '');

endif;
