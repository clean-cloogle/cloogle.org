<?php
define('CLEANHOME', '/opt/clean');
define('CLEANLIB', CLEANHOME . '/lib');

$lib = isset($_GET['lib']) ? $_GET['lib'] : 'StdEnv';
$lib = preg_replace('/[^\\w\\/\\-]/', '', $lib);

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

		natcasesort($ds);
		natcasesort($ms);
	} catch (Exception $e) {
		echo 'Failed to get directory ' . $dir;
	}

	return ['dirs' => $ds, 'modules' => $ms];
}

function makeBrowser($dir, $basemodule) {
	echo '<div class="browser togglee">';
	$elems = getDirsAndModules($dir);

	foreach ($elems['dirs'] as $d) {
		echo '<div class="browser-item directory toggle-container" data-name="' . $d . '">' .
				'<span class="toggler">' .
					'<span class="toggle-icon">&#x229e;</span>' .
					'<span class="title">' . $d . '</span></span>';
		makeBrowser($dir . '/' . $d, $basemodule . $d . '.');
		echo '</div>';
	}

	foreach ($elems['modules'] as $m) {
		$fullm = $basemodule . $m;
		echo '<div class="browser-item module" data-name="' . $m . '">' . $m . '</div>';
	}

	echo '</div>';
}

$groups = json_decode(file_get_contents('/var/libs.json'), true);

echo '<div class="browser">';
foreach ($groups as $group => $libs) {
	echo '<h4 class="browser-header">' . $group . '</h4>';
	foreach ($libs as $lib) {
		echo '<div class="browser-item directory toggle-container" data-name="' . $lib['name'] . '">' .
				'<span class="toggler">' .
					'<span class="toggle-icon">&#x229e;</span>' .
					'<span class="title">' . $lib['name'] . '</span>';
		if (isset($lib['info_url'])) {
			echo '<a class="more-info" href="' . $lib['info_url'] . '" target="_blank" title="More information" onclick="arguments[0].stopPropagation();">&#x2197;</a>';
		}
		echo '</span>';
		makeBrowser(CLEANLIB . '/' . $lib['name'], '');
		echo '</div>';
	}
}
echo '</div>';
