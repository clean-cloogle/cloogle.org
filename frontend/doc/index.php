<!DOCTYPE html>
<html>
<head>
	<title>Documentation browser</title>
	<meta http-equiv="Content-Type" content="text/html; charset=windows-1252"/>
	<meta name="viewport" content="width=device-width, initial-scale=1"/>
	<meta name="description" content="Cloogle is the unofficial Clean language search engine"/>
	<meta name="keywords" content="Clean,Clean language,Concurrent Clean,search,functions,search engine,programming language,clean platform,iTasks,cloogle,hoogle"/>
	<script src="../common.js" type="text/javascript" defer="defer"></script>
	<script src="../browser.js" type="text/javascript" defer="defer"></script>
	<script src="doc.js" type="text/javascript" defer="defer"></script>
	<link rel="stylesheet" href="../common.css" type="text/css"/>
	<link rel="stylesheet" href="../src/view.css" type="text/css"/>
	<style type="text/css">
		#viewer {
			padding: 1em;
		}

		#sidebar h3 {
			background: none;
			font-family: serif;
			font-size: 1.17em;
			font-weight: bold;
			line-height: 22px;
			margin-bottom: 17.55px;
			margin-left: 0;
		}

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
