<?php
define('SERVER_HOSTNAME', 'localhost');
define('SERVER_PORT', 31215);

if($_SERVER['REQUEST_METHOD'] !== 'GET'){
	echo json_encode(array(
		"return" => 1,
		"data" => array(),
		"msg" => "Can only be accessed by GET request"));
} else if(!isset($_GET['str'])){
	echo json_encode(array(
		"return" => 2,
		"data" => array(),
		"msg" => "GET variable 'str' should be set"));
} else {
	$str = array_map('trim', explode('::', $_GET['str']));
	$name = $str[0];
	$unify = isset($str[1]) ? $str[1] : '';
	$command = ['name' => $name, 'unify' => $unify];

	if (isset($_GET['mod'])) {
		$command['modules'] = explode(',', $_GET['mod']);
	}

	$skt = fsockopen(SERVER_HOSTNAME, SERVER_PORT);
	if (!$skt) {
		echo json_encode(array(
			"return" => 3,
			"data" => array(),
			"msg" => "Internal server error"));
	} else {
		fwrite($skt, json_encode($command));
		while (!feof($skt)) {
			echo fgets($skt, 128);
		}
		fclose($skt);
	}
}

