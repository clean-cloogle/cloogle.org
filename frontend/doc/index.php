<!DOCTYPE html>
<html lang="en">
<head>
	<title>Documentation browser</title>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	<meta name="viewport" content="width=device-width, initial-scale=1"/>
	<meta name="description" content="Cloogle is the unofficial Clean language search engine"/>
	<meta name="keywords" content="Clean,Clean language,Concurrent Clean,search,functions,search engine,programming language,clean platform,iTasks,cloogle,hoogle"/>
	<script src="../common.js" defer="defer"></script>
	<script src="../browser.js" defer="defer"></script>
	<script src="doc.js" defer="defer"></script>
	<link rel="stylesheet" href="../common.css" type="text/css"/>
	<style>
		#viewer {
			padding: 10px;
		}

		#viewer p {
			position: relative; /* For images, e.g. in 3.5.2 */
		}

		#sidebar {
			min-width: 300px;
		}

		#sidebar h3 {
			background: none;
			font-family: sans-serif;
			font-size: 1.17em;
			font-weight: bold;
			line-height: 22px;
			margin-bottom: 17.55px;
			margin-left: 0;
		}

		.Heading1Chapter img, .Newchapter img {
			display: none;
		}

		a:visited {
			color: blue !important;
		}
	</style>
</head>
<body class="framelike">
	<?php include('../banners.php'); ?>
	<div id="sidebar">
		<a href="/"><img id="logo" src="../logo.png" alt="Cloogle logo"/></a>
		<h3>Documentation browser</h3>
		<hr/>
		<?php include_once('contents.php'); ?>
		<hr/>
		The content of the language report is copyright &copy; 1987-2001, Hilt B.V., The Netherlands.
		See the <a style="padding-left:0;" href="#CleanRep.2.2_2.htm;jump=_Toc311797972">copyright notice</a>.
		<hr/>
		Download the language report as a <a style="padding-left:0;" href="http://clean.cs.ru.nl/download/doc/CleanLangRep.2.2.pdf">PDF</a>.
		<br/>
	</div><div id="viewer">
		<?php include_once('src.php'); ?>
	</div>
</body>
</html>
