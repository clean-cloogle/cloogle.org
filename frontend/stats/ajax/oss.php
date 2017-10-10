<?php
require_once('./conf.php');

$sql =
	"SELECT type, sum(cnt) as sumcnt FROM (
		select
			useragent,
			(SELECT count(*) FROM log
				WHERE useragent_id=useragent.id AND
				" . SQL_NOT_SILLYUSER . " AND
				`date` BETWEEN timestamp('$startTime') AND timestamp('$endTime')) as cnt,
			(CASE
				WHEN useragent LIKE '%Linux%' THEN 'Linux'
				WHEN useragent LIKE '%Macintosh%' THEN 'Macintosh'
				WHEN useragent LIKE '%Windows%' THEN 'Windows'
				WHEN useragent LIKE 'CloogleBot' THEN 'CloogleBot'
				WHEN useragent LIKE 'vim-clean' THEN 'vim-clean'
				WHEN useragent LIKE 'cloogle-cli' THEN 'cloogle-cli'
				WHEN useragent LIKE 'CloogleMail' THEN 'CloogleMail'
				WHEN useragent LIKE 'cloogle-irc' THEN 'cloogle-irc'
				ELSE 'Other'
			END) as type
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
