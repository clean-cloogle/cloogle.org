<?php
require_once('./conf.php');

$sql =
	"SELECT
		count(*),
		count(case when `responsecode`>=150 then 1 else null end),
		count(case when `responsecode`>1 and `responsecode`<150 then 1 else null end),
		sum(case when `query` LIKE '%::%' then 1 else null end),
		sum(case when `query` LIKE 'type %' then 1 else null end),
		sum(case when `query` LIKE 'class %' then 1 else null end)
	FROM `log`
	INNER JOIN `useragent` ON `log`.`useragent_id` = `useragent`.`id`
	WHERE
		" . SQL_NOT_SILLYUSER . " AND
		`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
		AND ";

$oss = [];
$oss_results = [];
$uas = [];
foreach ($user_agents as $k => $ua) {
	$oss[] = "`useragent` LIKE '" . $ua['pattern'] . "'";
	$oss_results[] = empty($ua['url']) ? $k : $k . ':' . $ua['url'];
	$uas[] = $ua['pattern'];
}
$oss[] = "(`useragent` NOT LIKE '" .
		implode("' AND `useragent` NOT LIKE '", $uas) .
		"') OR `useragent` IS NULL";
$oss_results[] = 'Other';

$results = [
	['name' => 'Success', 'data' => [], 'stack' => 'response'],
	['name' => 'Server error', 'data' => [], 'stack' => 'response'],
	['name' => 'User error', 'data' => [], 'stack' => 'response'],
	['name' => 'Name only', 'data' => [], 'stack' => 'search kind'],
	['name' => 'Unification', 'data' => [], 'stack' => 'search kind'],
	['name' => 'Type', 'data' => [], 'stack' => 'search kind'],
	['name' => 'Class', 'data' => [], 'stack' => 'search kind']
];

foreach ($oss as $os) {
	$stmt = $db->stmt_init();
	if (!$stmt->prepare($sql . $os))
		var_dump($stmt->error);
	$stmt->execute();
	$stmt->bind_result($total, $servererr, $usererr, $unify, $type, $class);
	$stmt->fetch();
	$stmt->close();

	$total = (int) $total;
	$servererr = (int) $servererr;
	$usererr = (int) $usererr;
	$unify = (int) $unify;
	$type = (int) $type;
	$class = (int) $class;

	$results[0]['data'][] = $total - $servererr - $usererr;
	$results[1]['data'][] = $servererr;
	$results[2]['data'][] = $usererr;

	$results[3]['data'][] = $total - $unify - $type - $class;
	$results[4]['data'][] = $unify;
	$results[5]['data'][] = $type;
	$results[6]['data'][] = $class;
}

$response = ['data' => $results, 'oss' => $oss_results];

header('Content-Type: text/javascript');
echo "$callback(" . json_encode($response) . ");";
