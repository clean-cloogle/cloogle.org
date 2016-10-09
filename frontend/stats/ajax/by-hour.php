<?php
require_once('./conf.php');

$sql =
	"SELECT dayofweek(`date`), hour(`date`), count(*) FROM `log`
	WHERE `date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')
	GROUP BY dayofweek(`date`), hour(`date`)
	ORDER BY dayofweek(`date`) asc, hour(`date`) ASC";

$stmt = $db->stmt_init();
if (!$stmt->prepare($sql))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($day, $hour, $querycount);
$_results = [];

while ($stmt->fetch())
	$_results[$day % 7][$hour] = $querycount;

$stmt->close();

$results = [];
for ($d = 0; $d <= 6; $d++)
	for ($h = 0; $h <= 23; $h++)
		if (isset($_results[$d][$h]))
			$results[] = "[$h,$d," . $_results[$d][$h] . "]";
		else
			$results[] = "[$h,$d,null]";

header('Content-Type: text/javascript');
echo "$callback([" . join(",", $results) . "]);";
