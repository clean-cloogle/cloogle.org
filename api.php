<?php
define('SERVER_HOSTNAME', 'localhost');
define('SERVER_PORT', 31215);

define('E_CLOOGLEDOWN', 150);
define('E_ILLEGALMETHOD', 151);
define('E_ILLEGALREQUEST', 152);

if($_SERVER['REQUEST_METHOD'] !== 'GET'){
	echo json_encode(array(
		"return" => E_ILLEGALMETHOD,
		"data" => array(),
		"msg" => "Can only be accessed by GET request"));
} else if(!isset($_GET['str'])){
	echo json_encode(array(
		"return" => E_ILLEGALREQUEST,
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

	if (isset($_GET['page'])) {
		$command['page'] = (int) $_GET['page'];
	}

	$skt = fsockopen(SERVER_HOSTNAME, SERVER_PORT);
	if (!$skt) {
		echo json_encode(array(
			"return" => E_CLOOGLEDOWN,
			"data" => array(),
			"msg" => "Cloogle server unreachable"));
	} else {
		fwrite($skt, json_encode($command));
		while (!feof($skt)) {
			$response = fgets($skt, 128);
			echo $response;
			if (strpos($response, "\n") !== false) {
				break;
			}
		}
		fclose($skt);
	}
}

