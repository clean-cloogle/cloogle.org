<?php
require_once('./conf.php');

$sqlavgreq =
	"SELECT AVG(`number`) as avgn FROM
	(SELECT COUNT(*) as number
		FROM `log`
		WHERE
			`responsecode` <> " . E_DOSPROTECT . " AND
			`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
		GROUP BY DATE(`date`)) counts";
$sqlmaxreq =
	"SELECT number as maxn, thedate as maxd FROM
	(SELECT DATE(`date`) as thedate, COUNT(*) as number
		FROM `log`
		WHERE
			`responsecode` <> " . E_DOSPROTECT . " AND
			`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
		GROUP BY DATE(`date`)
		ORDER BY number DESC, thedate DESC
		LIMIT 1) counts";
$sqlavgvis =
	"SELECT AVG(number) as avgn FROM
	(SELECT DATE(`date`) as thedate, COUNT(DISTINCT ip, useragent_id) as number
		FROM `log`
		WHERE
			`responsecode` <> " . E_DOSPROTECT . " AND
			`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
		GROUP BY DATE(`date`)) counts;";
$sqlmaxvis =
	"SELECT number as maxn, thedate as maxd FROM
	(SELECT DATE(`date`) as thedate, COUNT(DISTINCT ip, useragent_id) as number
		FROM `log`
		WHERE
			`responsecode` <> " . E_DOSPROTECT . " AND
			`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
		GROUP BY DATE(`date`)
		ORDER BY number DESC, thedate DESC
		LIMIT 1) counts;";

$stmt = $db->stmt_init();
if (!$stmt->prepare($sqlavgreq))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($avgr);
$stmt->fetch();
$stmt->close();

$stmt = $db->stmt_init();
if (!$stmt->prepare($sqlmaxreq))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($maxrn, $maxrd);
$stmt->fetch();
$stmt->close();

$stmt = $db->stmt_init();
if (!$stmt->prepare($sqlavgvis))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($avgv);
$stmt->fetch();
$stmt->close();

$stmt = $db->stmt_init();
if (!$stmt->prepare($sqlmaxvis))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($maxvn, $maxvd);
$stmt->fetch();
$stmt->close();

$results =
	[ 'requests' => ['avgn' => $avgr, 'maxn' => $maxrn, 'maxd' => $maxrd]
	, 'visitors' => ['avgn' => $avgv, 'maxn' => $maxvn, 'maxd' => $maxvd]
	];

header('Content-Type: text/javascript');
echo "$callback(" . json_encode($results) . ");";
