CREATE TABLE `actions_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `created_at` datetime NOT NULL,
  `ip` varchar(80) NOT NULL,
  `user_id` int(11) NOT NULL,
  `user_login` varchar(80) NOT NULL,
  `uri` varchar(400) NOT NULL,
  `post_params` text NOT NULL,
  `result` text,
  `message` varchar(200),
  PRIMARY KEY (`id`),
  KEY `ip_index` (`ip`),
  KEY `user_id_index` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8

