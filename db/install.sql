CREATE TABLE IF NOT EXISTS `log` (
	`date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	`ip` varchar(45) NOT NULL,
	`useragent_id` int(10) unsigned NOT NULL,
	`query` varchar(200) NOT NULL,
	`responsecode` tinyint(4) unsigned NOT NULL,
	`responsetime` smallint(5) unsigned NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

CREATE TABLE IF NOT EXISTS `useragent` (
	`id` int(10) unsigned NOT NULL,
	`useragent` text,
	`ua_hash` varchar(32) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

CREATE TABLE IF NOT EXISTS `blacklist` (
	`id` int(10) unsigned NOT NULL,
	`ip` varchar(45) NOT NULL,
	`useragent_id` int(10) unsigned NOT NULL,
	`start` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	`end` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

ALTER TABLE `log`
	ADD KEY `useragent_id` (`useragent_id`);

ALTER TABLE `useragent`
	ADD PRIMARY KEY (`id`), ADD UNIQUE KEY `ua_hash` (`ua_hash`);
ALTER TABLE `useragent`
	MODIFY `id` int(10) unsigned NOT NULL AUTO_INCREMENT;

ALTER TABLE `blacklist`
	ADD PRIMARY KEY (`id`);
ALTER TABLE `blacklist`
	MODIFY `id` int(10) unsigned NOT NULL AUTO_INCREMENT;

ALTER TABLE `log`
	ADD CONSTRAINT `log_ibfk_1` FOREIGN KEY (`useragent_id`) REFERENCES `useragent` (`id`);
ALTER TABLE `blacklist`
	ADD CONSTRAINT `blacklist_ibfk_1` FOREIGN KEY (`useragent_id`) REFERENCES `useragent` (`id`);
