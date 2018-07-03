<?php
$banners = json_decode(file_get_contents(__DIR__ . '/banners.json'), true);
foreach ($banners as $banner) {
	$from = strtotime($banner['from']);
	$until = strtotime($banner['until']);
	if ($from > time() || $until < time())
		continue;
	if (isset($banner['useragent']) && !preg_match($banner['useragent'], $_SERVER['HTTP_USER_AGENT']))
		continue;
	echo "<div " .
		"class='banner' " .
		"data-id='{$banner['id']}' " .
		"data-from='{$banner['from']}' " .
		"data-until='{$banner['until']}'>" .
		"<span id='{$banner['id']}'>{$banner['text']}</span>" .
		"</div>";
}
