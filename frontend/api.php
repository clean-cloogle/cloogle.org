<?php
define('SERVER_HOSTNAME', 'localhost');
define('SERVER_PORT', 31215);

define('E_CLOOGLEDOWN', 150);
define('E_ILLEGALMETHOD', 151);
define('E_ILLEGALREQUEST', 152);

function log_request($code) {
	if (defined('CLOOGLE_KEEP_STATISTICS')) {
		$db = new mysqli(
			CLOOGLE_DB_HOST, CLOOGLE_DB_USER, CLOOGLE_DB_PASS, CLOOGLE_DB_NAME);
		if (!mysqli_connect_errno()) {
			$stmt = $db->prepare(
				'INSERT INTO `log` (`ip`,`query`,`responsecode`) VALUES (?,?,?)');
			$stmt->bind_param('ssi', $_SERVER['REMOTE_ADDR'], $_GET['str'], $code);
			$stmt->execute();
		}
		$db->close();
	}
}

function respond($code, $msg, $data=[]) {
	log_request($code);

	echo json_encode([
		'return' => $code,
		'data' => $data,
		'msg' => $msg
	]);
}

if (file_exists('conf.php')) {
	require_once('conf.php');
}

if($_SERVER['REQUEST_METHOD'] !== 'GET'){
	respond(E_ILLEGALMETHOD, 'Can only be accessed by GET request');
} else if(!isset($_GET['str'])){
	respond(E_ILLEGALREQUEST, 'GET variable "str" must be set');
} else {
	$str = array_map('trim', explode('::', $_GET['str']));
	$name = trim($str[0]);
	$unify = isset($str[1]) ? trim($str[1]) : '';
	$command = [];

	if (substr($name, 0, 6) == 'class ') {
		$command['className'] = substr($name, 6);
	} elseif (substr($name, 0, 5) == 'type ') {
		$command['typeName'] = substr($name, 5);
	} elseif ($name != '') {
		$command['name'] = $name;
	}

	if ($unify != '') {
		$command['unify'] = $unify;
	}

	if (isset($_GET['lib'])) {
		$command['libraries'] = [explode(',', $_GET['lib']), false];
	}

	if (isset($_GET['libs_builtin'])) {
		if (!isset($command['libraries'][0]))
			$command['libraries'][0] = [];
		$command['libraries'][1] = $_GET['libs_builtin'] == 'true';
	}

	if (isset($_GET['mod'])) {
		$command['modules'] = explode(',', $_GET['mod']);
	}

	if (isset($_GET['page'])) {
		$command['page'] = (int) $_GET['page'];
	}

	$skt = fsockopen(SERVER_HOSTNAME, SERVER_PORT);
	if (!$skt) {
		respond(E_CLOOGLEDOWN, 'Cloogle server unreachable');
	} else {
		$response = '';
		fwrite($skt, json_encode($command));
		while (!feof($skt)) {
			$_response = fgets($skt, 128);
			$response .= $_response;
			if (strpos($response, "\n") !== false) {
				break;
			}
		}
		fclose($skt);
		echo $response;

		$decoded = json_decode($response, true);
		log_request($decoded['return']);
	}
}
