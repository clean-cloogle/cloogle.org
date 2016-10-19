<?php
require_once('./conf.php');

$range = $end - $start;

if ($range < 24 * 3600) {
	// up to 1 day: minute data
	$group = 'date(`date`), hour(`date`), floor(minute(`date`)/10)';
	$timestamp = 'floor(unix_timestamp(MIN(`date`)) / 600) * 600';
	$timemod = 600;
} elseif ($range < 7 * 24 * 3600) {
	// up to 7 days: hourly data
	$group = 'date(`date`), hour(`date`)';
	$timestamp = 'floor(unix_timestamp(MIN(`date`)) / 3600) * 3600';
	$timemod = 3600;
} elseif ($range < 2 * 30 * 24 * 3600) {
	// up to 2 months: daily data
	$group = 'date(`date`)';
	$timestamp = 'floor(unix_timestamp(MIN(`date`)) / 86400) * 86400';
	$timemod = 86400;
} else {
	// otherwise: monthly data
	$group = 'year(`date`), month(`date`)';
	$timestamp = "unix_timestamp(concat(year(`date`), '/', month(`date`), '/', '1'))";
	$timemod = 'monthly';
}

$sql =
	"SELECT
		$timestamp as unixtime,
		count(*),
		count(case when `responsecode`>=150 then 1 else null end),
		count(case when `responsecode`>1 and `responsecode`<150 then 1 else null end),
		count(distinct `ip`,`useragent_id`)
	FROM `log`
	WHERE `date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
	GROUP BY $group";

$stmt = $db->stmt_init();
if (!$stmt->prepare($sql))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($timestamp, $count, $servererrcount, $usererrcount, $uniquecount);
$results = [[], [], [], []];

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
		for ($i=0; $i<4; $i++)
			$results[$i][] = "[" . $expected_timestamp*1000 . ",0]";
		update_expected_timestamp();
	}
	$results[0][] = "[" . $timestamp*1000 . "," . ($count-$servererrcount-$usererrcount) . "]";
	$results[1][] = "[" . $timestamp*1000 . ",$usererrcount]";
	$results[2][] = "[" . $timestamp*1000 . ",$servererrcount]";
	$results[3][] = "[" . $timestamp*1000 . ",$uniquecount]";
	update_expected_timestamp();
}
while ($expected_timestamp <= $end + ($timemod === 'monthly' ? 31 * 86400 : $timemod)) {
	for ($i=0; $i<4; $i++)
		$results[$i][] = "[" . $expected_timestamp*1000 . ",0]";
	update_expected_timestamp();
}

header('Content-Type: text/javascript');
echo "$callback([" .
	"[" . join(",", $results[0]) . "]," .
	"[" . join(",", $results[1]) . "]," .
	"[" . join(",", $results[2]) . "]," .
	"[" . join(",", $results[3]) . "]" .
	"]);";
