<?php
error_reporting(0);

define('SERVER_HOSTNAME', 'backend');
define('SERVER_PORT', 31215);
define('SERVER_TIMEOUT', 3);

define('DOS_INITIAL_BLOCK', 1);
define('DOS_BLOCK_MULTIPLY', 2);
define('DOS_MAX_BLOCK', 60);

if (file_exists('conf.php'))
	require_once('conf.php');

$common_problems = [];
if (file_exists('/var/common-problems.json'))
	$common_problems = json_decode(file_get_contents('/var/common-problems.json'), true);

$start_time = microtime(true);

if (defined('CLOOGLE_KEEP_STATISTICS')) {
	$db = new mysqli(
		CLOOGLE_DB_HOST, CLOOGLE_DB_USER, CLOOGLE_DB_PASS, CLOOGLE_DB_NAME);
	if (mysqli_connect_errno())
		$db = null;

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

	$ip = isset($_SERVER['HTTP_X_FORWARDED_FOR']) ?
		$_SERVER['HTTP_X_FORWARDED_FOR'] : $_SERVER['REMOTE_ADDR'];
	$ip = substr(md5($ip), 0, 12);
}

function has_database() {
	global $db;
	return defined('CLOOGLE_KEEP_STATISTICS') && !is_null($db);
}

function dos_protect() {
	global $db, $ua_id, $ip;

	if (!has_database())
		return false;

	$stmt = $db->prepare('SELECT COUNT(*) FROM `blacklist`
		WHERE `ip`=? AND `useragent_id`=? AND `end` >= NOW()');
	$stmt->bind_param('si', $ip, $ua_id);
	$stmt->execute();
	$stmt->bind_result($count);
	$stmt->fetch();
	$stmt->close();

	if ($count)
		return true;

	$stmt = $db->prepare('SELECT COUNT(*) FROM `log`
		WHERE `useragent_id`=? AND `ip`=? AND `date`>=NOW() - INTERVAL 1 SECOND');
	$stmt->bind_param('is', $ua_id, $ip);
	$stmt->execute();
	$stmt->bind_result($count);
	$stmt->fetch();
	$stmt->close();

	$protect = $count >= DOS_MAX_REQUESTS_PER_SECOND;

	if ($protect) {
		$stmt = $db->prepare('SELECT
			UNIX_TIMESTAMP(MAX(`start`)), UNIX_TIMESTAMP(MAX(`end`)) FROM `blacklist`
			WHERE `ip`=? AND `useragent_id`=? AND
				`end` >= DATE_ADD(NOW(), INTERVAL -10 MINUTE)');
		$stmt->bind_param('si', $ip, $ua_id);
		$stmt->execute();
		$stmt->bind_result($start, $end);
		$stmt->fetch();
		$stmt->close();

		if (is_null($start) || is_null($end))
			$block = DOS_INITIAL_BLOCK;
		else
			$block = ($end - $start) * DOS_BLOCK_MULTIPLY;
		if ($block > DOS_MAX_BLOCK)
			$block = DOS_MAX_BLOCK;

		$stmt = $db->prepare('INSERT INTO `blacklist`
			(`ip`, `useragent_id`, `end`) VALUES
			(?,?,DATE_ADD(NOW(), INTERVAL ' . $block . ' SECOND))');
		$stmt->bind_param('si', $ip, $ua_id);
		$stmt->execute();
		$stmt->close();
	}

	return $protect;
}

function log_request($code) {
	global $db, $ua_id, $ip;

	if (!has_database())
		return;

	global $start_time;
	$time = (int) ((microtime(true) - $start_time) * 1000);

	$stmt = $db->prepare('INSERT INTO `log`
		(`ip`,`useragent_id`,`query`,`responsecode`,`responsetime`)
		VALUES (?,?,?,?,?)');
	$stmt->bind_param('sisii',
		$ip,
		$ua_id,
		substr($_GET['str'], 0, 199),
		$code,
		$time);
	$stmt->execute();
	$stmt->close();

	$db->close();
}

function respond($code, $msg, $data=[]) {
	log_request($code);

	echo json_encode([
		'return' => $code,
		'data' => $data,
		'msg' => $msg
	]);
}

if ($_SERVER['REQUEST_METHOD'] !== 'GET'){
	respond(E_ILLEGALMETHOD, 'Can only be accessed by GET request');
} else if (!isset($_GET['str'])){
	respond(E_ILLEGALREQUEST, 'GET variable "str" must be set');
} else if (defined('CLOOGLE_KEEP_STATISTICS') && dos_protect()) {
	respond(E_DOSPROTECT, "Yes, cloogle is great, but you don't need it so badly.");
} else if (strlen($_GET['str']) >= 200) {
	respond(E_QUERYTOOLONG, 'Query too long');
} else {
	$str = array_map('trim', explode('::', $_GET['str']));
	$name = trim($str[0]);
	$unify = isset($str[1]) ? trim($str[1]) : '';
	$command = [];

	$extra_results = [];
	foreach ($common_problems as $problem) {
		$match = false;
		foreach ($problem['keywords'] as $kw) {
			if (preg_match("/(?i)$kw/", $name) === 1) {
				$match = true;
				break;
			}
		}

		if ($match) {
			$extra_results[] = ['ProblemResult',
				[ 'problem_key' => $problem['key']
				, 'problem_title' => $problem['title']
				, 'problem_description' => $problem['description']
				, 'problem_solutions' => $problem['solutions']
				, 'problem_examples' => $problem['examples']
				]
			];
		}
	}

	if ($unify != '') {
		$command['unify'] = $unify;
	}

	if (substr($name, 0, 6) == 'using ') {
		$command['using'] = [];
		$usingnames = explode(',', substr($name, 6));
		foreach ($usingnames as $usingname) {
			$command['using'][] = trim($usingname);
		}
	} elseif (substr($name, 0, 6) == 'class ') {
		$command['className'] = substr($name, 6);
	} elseif (substr($name, 0, 5) == 'type ') {
		$command['typeName'] = substr($name, 5);
	} elseif ($name != '') {
		$command['name'] = $name;
	}

	if (isset($_GET['lib'])) {
		$command['libraries'] = explode(',', $_GET['lib']);
	}

	if (isset($_GET['include_builtins'])) {
		$command['include_builtins'] = $_GET['include_builtins'] == 'true';
	}

	if (isset($_GET['include_core'])) {
		$command['include_core'] = $_GET['include_core'] == 'true';
	}

	if (isset($_GET['include_apps'])) {
		$command['include_apps'] = $_GET['include_apps'] == 'true';
	}

	if (isset($_GET['mod'])) {
		$command['modules'] = explode(',', $_GET['mod']);
	}

	if (isset($_GET['page'])) {
		$command['page'] = (int) $_GET['page'];
	}

	$skt = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
	if (!socket_connect($skt, SERVER_HOSTNAME, SERVER_PORT)) {
		respond(E_CLOOGLEDOWN, 'Cloogle server unreachable', $extra_results);
	} else {
		$response = '';
		socket_write($skt, json_encode($command));
		$read = [$skt];
		if (socket_select($read, $w = null, $e = null, SERVER_TIMEOUT) !== 1) {
			respond(E_TIMEOUT, 'Connection to the Cloogle server timed out', $extra_results);
		} else {
			while (($_response = socket_read($skt, 128, PHP_NORMAL_READ)) !== false) {
				$response .= $_response;
				if (strpos($_response, "\n") !== false)
					break;
			}
			$decoded = json_decode($response, true);
			log_request($decoded['return']);
			$decoded['data'] = array_merge($extra_results, $decoded['data']);
			echo json_encode($decoded);
		}
		socket_close($skt);
	}
}
