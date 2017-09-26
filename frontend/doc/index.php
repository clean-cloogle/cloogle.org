<!DOCTYPE html>
<html>
<head>
	<title>Documentation browser</title>
	<meta http-equiv="Content-Type" content="text/html; charset=windows-1252"/>
	<meta name="viewport" content="width=device-width, initial-scale=1"/>
	<meta name="description" content="Cloogle is the unofficial Clean language search engine"/>
	<meta name="keywords" content="Clean,Clean language,Concurrent Clean,search,functions,search engine,programming language,clean platform,iTasks,cloogle,hoogle"/>
	<script src="../common.js" type="text/javascript" defer="defer"></script>
	<script src="../src/view.js" type="text/javascript" defer="defer"></script>
	<link rel="stylesheet" href="../common.css" type="text/css"/>
	<link rel="stylesheet" href="../src/view.css" type="text/css"/>
	<style type="text/css">
		.Heading1Chapter img, .Newchapter img {
			display: none;
		}
	</style>
</head>
<body>
	<div id="sidebar">
		<a href="/"><img id="logo" src="../logo.png" alt="Cloogle logo"/></a>
		<h3>Documentation browser</h3>
		<hr/>
		<?php include_once('contents.php'); ?>
	</div><div id="viewer">
		<?php include_once('src.php'); ?>
	</div>
</body>
</html>
