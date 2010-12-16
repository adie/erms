CREATE TABLE `doc_folder_groups` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `doc_folder_id` int(11) NOT NULL,
  `group_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `doc_folder_id_index` (`doc_folder_id`),
  KEY `group_id_index` (`group_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8


