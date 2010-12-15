CREATE TABLE `doc_folder` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(40) NOT NULL,
  `parent_folder_id` int(11) NOT NULL,
  `created_at` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `parent_folder_id_index` (`parent_folder_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8

