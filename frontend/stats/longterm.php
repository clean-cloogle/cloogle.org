<?php
if (file_exists('../conf.php')) {
	require_once('../conf.php');
}

if (!defined('CLOOGLE_KEEP_STATISTICS'))
	die('Statistics have not been enabled.');

$db = new mysqli(
	CLOOGLE_DB_HOST, CLOOGLE_DB_USER, CLOOGLE_DB_PASS, CLOOGLE_DB_NAME);
if (mysqli_connect_errno())
	return;

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

if (!$end)
	$end = time();

$range = $end - $start;
$startTime = gmstrftime('%Y-%m-%d %H:%M:%S', $start);
$endTime = gmstrftime('%Y-%m-%d %H:%M:%S', $end);

if ($range < 24 * 3600) {
	// up to 1 day: minute data
	$group = 'DATE(`date`), HOUR(`date`), MINUTE(`date`)';
	$timestamp = 'floor(unix_timestamp(MIN(`date`)) / 60) * 60';
	$timemod = 60;
} elseif ($range < 7 * 24 * 3600) {
	// up to 7 days: hourly data
	$group = 'DATE(`date`), HOUR(`date`)';
	$timestamp = 'floor(unix_timestamp(MIN(`date`)) / 3600) * 3600';
	$timemod = 3600;
} elseif ($range < 2 * 30 * 24 * 3600) {
	// up to 2 months: daily data
	$group = 'DATE(`date`)';
	$timestamp = 'floor(unix_timestamp(MIN(`date`)) / 86400) * 86400';
	$timemod = 86400;
} else {
	// otherwise: monthly data
	$group = 'YEAR(`date`), MONTH(`date`)';
	$timestamp = "unix_timestamp(CONCAT(YEAR(`date`), '/', MONTH(`date`), '/', '1'))";
	$timemod = 'monthly';
}

$sql =
	"SELECT
		$timestamp as unixtime,
		count(*) as querycount,
		count(case when `responsecode`=0 then NULL else 1 end) as failedcount,
		count(distinct `ip`,`useragent_id`) as uniquecount
	FROM `log`
	WHERE `date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
	GROUP BY $group";

$stmt = $db->stmt_init();
if (!$stmt->prepare($sql))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($timestamp, $count, $failedcount, $uniquecount);
$results = [[], []];

$expected_timestamp = $timemod === 'monthly'
	? strtotime(date('Y-m-01 00:00:00', $start))
	: floor($start / $timemod) * $timemod;

function update_expected_timestamp() {
	global $expected_timestamp, $timemod;
	$expected_timestamp = $timemod === 'monthly'
		? strtotime(date('Y-m-01', strtotime('next month', $expected_timestamp)))
		: $expected_timestamp + $timemod;
}

while ($stmt->fetch()) {
	while ($expected_timestamp < $timestamp) {
		for ($i=0; $i<3; $i++)
			$results[$i][] = "[" . $expected_timestamp*1000 . ",0]";
		update_expected_timestamp();
	}
	$results[0][] = "[" . $timestamp*1000 . "," . ($count-$failedcount) . "]";
	$results[1][] = "[" . $timestamp*1000 . "," . ($failedcount > 0 ? $failedcount : 'null') . "]";
	$results[2][] = "[" . $timestamp*1000 . ",$uniquecount]";
	update_expected_timestamp();
}
while ($expected_timestamp <= $end) {
	for ($i=0; $i<3; $i++)
		$results[$i][] = "[" . $expected_timestamp*1000 . ",0]";
	update_expected_timestamp();
}

header('Content-Type: text/javascript');
echo "$callback([" .
	"[\n\t" . join(",\n\t", $results[0]) . "\n]," .
	"[\n\t" . join(",\n\t", $results[1]) . "\n]," .
	"[\n\t" . join(",\n\t", $results[2]) . "\n]" .
	"]);";
