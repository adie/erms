CREATE TABLE `document` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(80) NOT NULL,
  `filename` varchar(80) NOT NULL,
  `doc_folder_id` int(11) NOT NULL,
  `user_id` int(11) NOT NULL,
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  `file_path` varchar(500),
  `file_size` int(11),
  PRIMARY KEY (`id`),
  KEY `name_index` (`name`),
  KEY `doc_folder_id_index` (`doc_folder_id`),
  KEY `user_id_index` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
