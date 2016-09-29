CREATE TABLE IF NOT EXISTS `log` (
  `date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `ip` varchar(45) NOT NULL,
  `query` varchar(200) NOT NULL,
  `responsecode` tinyint(4) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
