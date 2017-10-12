<?php
if (file_exists('../../conf.php'))
	require_once('../../conf.php');

if (!defined('CLOOGLE_KEEP_STATISTICS'))
	die('Statistics have not been enabled.');

$db = new mysqli(
	CLOOGLE_DB_HOST, CLOOGLE_DB_USER, CLOOGLE_DB_PASS, CLOOGLE_DB_NAME);
if (mysqli_connect_errno())
	return;

define('SQL_NOT_SILLYUSER', '`responsecode` not in (' . E_DOSPROTECT . ', ' . E_QUERYTOOLONG . ')');

$db->query("SET time_zone = '+00:00'");
date_default_timezone_set('UTC');

$callback = $_GET['callback'];
if (!preg_match('/^[a-zA-Z0-9_]+$/', $callback))
	die('Invalid callback name');

$start = @$_GET['start'];
if ($start && !preg_match('/^\d+$/', $start))
	die ("Invalid start parameter: $start");

if ($start == 0) {
	$stmt = $db->stmt_init();
	$stmt->prepare("SELECT unix_timestamp(min(`date`)) as mindate FROM `log`");
	$stmt->execute();
	$stmt->bind_result($start);
	$stmt->fetch();
	$stmt->close();
}

$end = @$_GET['end'];
if ($end && !preg_match('/^\d+$/', $end))
	die ("Invalid end parameter: $end");

if (!$end || $end > time())
	$end = time();

$startTime = gmstrftime('%Y-%m-%d %H:%M:%S', $start);
$endTime = gmstrftime('%Y-%m-%d %H:%M:%S', $end);

$user_agents =
	[ 'Linux'        => ['pattern' => '%Linux%']
	, 'Macintosh'    => ['pattern' => '%Macintosh%']
	, 'Windows'      => ['pattern' => '%Windows%']
	, 'CloogleBot'   => ['pattern' => 'CloogleBot',   'url' => 'https://telegram.me/CloogleBot']
	, 'vim-clean'    => ['pattern' => 'vim-clean',    'url' => 'https://github.com/camilstaps/vim-clean']
	, 'cloogle-cli'  => ['pattern' => 'cloogle-cli',  'url' => 'https://github.com/clean-cloogle/cloogle-cli']
	, 'CloogleMail'  => ['pattern' => 'CloogleMail',  'url' => 'mailto:query@cloogle.org']
	, 'cloogle-irc'  => ['pattern' => 'cloogle-irc',  'url' => 'https://github.com/clean-cloogle/clean-irc']
	, 'clean-vscode' => ['pattern' => 'clean-vscode', 'url' => 'https://github.com/W95Psp/CleanForVSCode']
	];
