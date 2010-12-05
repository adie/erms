CREATE TABLE `users` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `login` varchar(20) NOT NULL,
  `password` varchar(20) NOT NULL,
  `fullname` varchar(120) NOT NULL,
  `info` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `login_index` (`login`),
  KEY `fullname_index` (`fullname`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
