<!DOCTYPE html>
<html lang="en">
<head>
	<title>Library browser</title>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	<meta name="viewport" content="width=device-width, initial-scale=1"/>
	<meta name="description" content="Cloogle is the unofficial Clean language search engine"/>
	<meta name="keywords" content="Clean,Clean language,Concurrent Clean,search,functions,search engine,programming language,clean platform,iTasks,cloogle,hoogle"/>
	<script src="../common.js" defer="defer"></script>
	<script src="../clean.js/clean.js" defer="defer"></script>
	<script src="../browser.js" defer="defer"></script>
	<script src="view.js" defer="defer"></script>
	<link rel="stylesheet" href="../common.css" type="text/css"/>
	<link rel="stylesheet" href="../clean.js/clean.css" type="text/css"/>
	<link rel="stylesheet" href="view.css" type="text/css"/>
</head>
<body class="framelike">
	<?php include('../banners.php'); ?>
	<div id="sidebar">
		<a href="/"><img id="logo" src="../logo.png" alt="Cloogle logo"/></a>
		<h3>Library browser</h3>
		<label for="icl"><input id="icl" type="checkbox"/> Show implementation</label><br/>
		<input id="share-button" type="button" value="Share" onclick="shareButtonClick()"/>
		<hr/>
		<?php include_once('lib.php'); ?>
		<br/>
	</div><div id="viewer">
		<?php include_once('src.php'); ?>
	</div>
</body>
</html>
