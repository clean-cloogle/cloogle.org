<?php
define('SERVER_HOSTNAME', 'backend');
define('SERVER_PORT', 31215);
define('SERVER_TIMEOUT', 8);

define('E_CLOOGLEDOWN', 150);
define('E_ILLEGALMETHOD', 151);
define('E_ILLEGALREQUEST', 152);
define('E_TIMEOUT', 153);

$start_time = microtime(true);

function log_request($code) {
	if (defined('CLOOGLE_KEEP_STATISTICS')) {
		$db = new mysqli(
			CLOOGLE_DB_HOST, CLOOGLE_DB_USER, CLOOGLE_DB_PASS, CLOOGLE_DB_NAME);
		if (mysqli_connect_errno())
			return;

		$ua = $_SERVER['HTTP_USER_AGENT'];
		$ua_hash = md5($ua);

		$stmt = $db->prepare('SELECT `id` FROM `useragent` WHERE `ua_hash`=?');
		$stmt->bind_param('s', $ua_hash);
		$stmt->execute();
		$stmt->bind_result($ua_id);
		if ($stmt->fetch() !== true) {
			$stmt->close();
			$stmt = $db->prepare(
				'INSERT INTO `useragent` (`useragent`,`ua_hash`) VALUES (?,?)');
			$stmt->bind_param('ss', $ua, $ua_hash);
			$stmt->execute();
			$ua_id = $stmt->insert_id;
		}
		$stmt->close();

		global $start_time;
		$time = (int) ((microtime(true) - $start_time) * 1000);

		$stmt = $db->prepare('INSERT INTO `log`
			(`ip`,`useragent_id`,`query`,`responsecode`,`responsetime`)
			VALUES (?,?,?,?,?)');
		$stmt->bind_param('sisii',
			$_SERVER['HTTP_X_FORWARDED_FOR'],
			$ua_id,
			$_GET['str'],
			$code,
			$time);
		$stmt->execute();
		$stmt->close();

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

	$skt = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
	if (!socket_connect($skt, SERVER_HOSTNAME, SERVER_PORT)) {
		respond(E_CLOOGLEDOWN, 'Cloogle server unreachable');
	} else {
		$response = '';
		socket_write($skt, json_encode($command));
		$read = [$skt];
		if (socket_select($read, $w = null, $e = null, SERVER_TIMEOUT) !== 1) {
			respond(E_TIMEOUT, 'Connection to the Cloogle server timed out');
		} else {
			while (($_response = socket_read($skt, 128, PHP_NORMAL_READ)) !== false) {
				$response .= $_response;
				if (strpos($_response, "\n") !== false)
					break;
			}
			echo $response;
			$decoded = json_decode($response, true);
			log_request($decoded['return']);
		}
		socket_close($skt);
	}
}
