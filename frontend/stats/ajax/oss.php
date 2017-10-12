<?php
require_once('./conf.php');

$uacase = 'CASE';
foreach ($user_agents as $name => $ua)
	$uacase .= " WHEN `useragent` LIKE '" . $ua['pattern'] . "' THEN '" . $name . "'";
$uacase .= " ELSE 'Other' END";

$sql =
	"SELECT type, sum(cnt) as sumcnt FROM (
		select
			useragent,
			(SELECT count(*) FROM log
				WHERE useragent_id=useragent.id AND
				" . SQL_NOT_SILLYUSER . " AND
				`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')) as cnt,
			($uacase) as type
		FROM useragent
		ORDER BY cnt DESC
	) counts
	GROUP BY type
	ORDER BY sumcnt DESC";

$stmt = $db->stmt_init();
if (!$stmt->prepare($sql))
	var_dump($stmt->error);
$stmt->execute();
$stmt->bind_result($type, $count);

$results = [];
while ($stmt->fetch())
	$results[] = ['name' => $type, 'y' => (int) $count];

$stmt->close();

header('Content-Type: text/javascript');
echo "$callback(" . json_encode($results) . ");";
