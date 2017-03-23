<!DOCTYPE html>
<html>
<head>
	<title>Library browser</title>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	<meta name="viewport" content="width=device-width, initial-scale=1"/>
	<meta name="description" content="Cloogle is the unofficial Clean language search engine"/>
	<meta name="keywords" content="Clean,Clean language,Concurrent Clean,search,functions,search engine,programming language,clean platform,iTasks,cloogle,hoogle"/>
	<script src="../common.js" type="text/javascript" defer="defer"></script>
	<script src="view.js" type="text/javascript" defer="defer"></script>
	<link rel="stylesheet" href="../common.css" type="text/css"/>
	<link rel="stylesheet" href="view.css" type="text/css"/>
</head>
<body>
	<div id="sidebar">
		<a href="/"><img id="logo" src="../logo.png" alt="Cloogle logo"/></a>
		<h3>Library browser</h3>
		<select id="select-lib">
			<?php
			$alllibs = [
				'Clean 2.4' => [
					'StdEnv',
					'ArgEnv',
					'Directory',
					'Dynamics',
					'Gast',
					'Generics',
					'MersenneTwister',
					'StdLib',
					'TCPIP',
				],
				'Miscellaneous' => [
					'CleanPrettyPrint',
					'CleanSerial',
					'CleanTypeUnifier',
					'Cloogle',
					'GraphCopy',
					'ObjectIO',
					'Platform',
					'Sapl',
					'SoccerFun',
					'iTasks',
				]
			];

			foreach ($alllibs as $col => $libs) {
				echo '<optgroup label="' . $col . '">';
				foreach ($libs as $lib) {
					$selected = '';
					if ($lib == $_GET['lib'])
						$selected = ' selected="selected"';
					echo '<option value="' . $lib . '"' . $selected . '>' . $lib . '</option>';
				}
				echo '</optgroup>';
			}
			?>
		</select>
		<br/>
		<label for="icl"><input id="icl" type="checkbox"/> Show implementation</label>
		<hr/>
		<?php include_once('lib.php'); ?>
	</div><div id="viewer">
		<?php include_once('src.php'); ?>
	</div>
</body>
</html>
