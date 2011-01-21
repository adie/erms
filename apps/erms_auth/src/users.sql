CREATE TABLE `users` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `login` varchar(40) NOT NULL,
  `password_hash` varchar(80) NOT NULL,
  `fullname` varchar(120) NOT NULL,
  `info` text NOT NULL,
  `private_key` text NOT NULL,
  `public_key` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `login_index` (`login`),
  KEY `fullname_index` (`fullname`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
