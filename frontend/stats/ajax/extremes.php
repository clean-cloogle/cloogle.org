<?php
require_once('./conf.php');

$sqlavg =
	"SELECT AVG(`number`) as avgn FROM
	(SELECT COUNT(*) as number
		FROM `log`
		WHERE
			`responsecode` <> " . E_DOSPROTECT . " AND
			`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
		GROUP BY DATE(`date`)) counts";
$sqlmax =
	"SELECT number as maxn, thedate as maxd FROM
	(SELECT DATE(`date`) as thedate, COUNT(*) as number
		FROM `log`
		WHERE
			`responsecode` <> " . E_DOSPROTECT . " AND
			`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
		GROUP BY DATE(`date`)
		ORDER BY number DESC, thedate DESC
		LIMIT 1) counts";

$stmt = $db->stmt_init();
if (!$stmt->prepare($sqlavg))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($avgn);
$stmt->fetch();
$stmt->close();

$stmt = $db->stmt_init();
if (!$stmt->prepare($sqlmax))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($maxn, $maxd);
$stmt->fetch();
$stmt->close();

$results =
	[ 'avgn' => $avgn
	, 'maxn' => $maxn
	, 'maxd' => $maxd
	];

header('Content-Type: text/javascript');
echo "$callback(" . json_encode($results) . ");";
