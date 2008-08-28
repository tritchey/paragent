-- MySQL dump 10.10
--
-- Host: localhost    Database: khala
-- ------------------------------------------------------
-- Server version	5.0.22

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `alert_computer_link`
--

DROP TABLE IF EXISTS `alert_computer_link`;
CREATE TABLE `alert_computer_link` (
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `ALERT_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  UNIQUE KEY `ALERT_ID_3` (`ALERT_ID`,`COMPUTER_ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`),
  KEY `ALERT_ID` (`ALERT_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `alert_event_link`
--

DROP TABLE IF EXISTS `alert_event_link`;
CREATE TABLE `alert_event_link` (
  `ID` int(11) NOT NULL auto_increment,
  `EVENT_ID` int(11) NOT NULL default '0',
  `ALERT_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  UNIQUE KEY `ALERT_ID_4` (`ALERT_ID`,`EVENT_ID`),
  KEY `EVENT_ID` (`EVENT_ID`),
  KEY `ALERT_ID` (`ALERT_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `alerts`
--

DROP TABLE IF EXISTS `alerts`;
CREATE TABLE `alerts` (
  `ENABLED` tinyint(1) NOT NULL default '0',
  `TYPE_ID` int(11) NOT NULL default '0',
  `EMAIL_TO` varchar(255) NOT NULL default '',
  `NOTE` varchar(255) NOT NULL default '',
  `SEVERITY` int(11) NOT NULL,
  `ARGS` varchar(255) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPANY_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPANY_ID` (`COMPANY_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `antivirus`
--

DROP TABLE IF EXISTS `antivirus`;
CREATE TABLE `antivirus` (
  `NAME` char(200) NOT NULL,
  `MANUFACTURER` char(200) default NULL,
  `COMPANY` char(200) default NULL,
  `VERSION` char(50) default NULL,
  `UP_TO_DATE` tinyint(1) default NULL,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `bios`
--

DROP TABLE IF EXISTS `bios`;
CREATE TABLE `bios` (
  `NAME` char(100) default NULL,
  `LANGUAGE` char(100) default NULL,
  `MANUFACTURER` char(100) default NULL,
  `RELEASE_DATE` date default NULL,
  `VERSION` char(100) default NULL,
  `SERIAL_NUMBER` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `cd_roms`
--

DROP TABLE IF EXISTS `cd_roms`;
CREATE TABLE `cd_roms` (
  `NAME` char(100) default NULL,
  `DRIVE` char(100) default NULL,
  `MANUFACTURER` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `companies`
--

DROP TABLE IF EXISTS `companies`;
CREATE TABLE `companies` (
  `ID` int(11) NOT NULL auto_increment,
  `NAME` varchar(255) NOT NULL default '',
  `SECRET` varchar(255) NOT NULL default '',
  `MSI` varchar(255) default NULL,
  `DISABLED` tinyint(1) NOT NULL,
  `CREATED` datetime default NULL,
  `LEVEL` int(11) NOT NULL,
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `company_ticket_ids`
--

DROP TABLE IF EXISTS `company_ticket_ids`;
CREATE TABLE `company_ticket_ids` (
  `COMPANY_ID` int(11) NOT NULL default '0',
  `NEXT_TICKET_ID` int(11) NOT NULL,
  PRIMARY KEY  (`COMPANY_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `computer_tag`
--

DROP TABLE IF EXISTS `computer_tag`;
CREATE TABLE `computer_tag` (
  `NAME` char(50) NOT NULL default '',
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`),
  KEY `NAME` (`NAME`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `computers`
--

DROP TABLE IF EXISTS `computers`;
CREATE TABLE `computers` (
  `ARCHON_CONNECTION` int(11) default NULL,
  `NAME` varchar(75) NOT NULL default '',
  `NOTE` varchar(255) default NULL,
  `ONLINE` tinyint(1) NOT NULL,
  `LAST_ONLINE` datetime default NULL,
  `WARRANTY` date default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPANY_ID` int(11) NOT NULL default '0',
  `ALIAS` varchar(75) default NULL,
  PRIMARY KEY  (`ID`),
  UNIQUE KEY `COMPANY_ID_2` (`COMPANY_ID`,`NAME`),
  UNIQUE KEY `COMPANY_ID_3` (`COMPANY_ID`,`ALIAS`),
  KEY `COMPANY_ID` (`COMPANY_ID`),
  KEY `ONLINE` (`ONLINE`),
  KEY `WARRANTY` (`WARRANTY`),
  KEY `NAME` (`NAME`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `email_smtp`
--

DROP TABLE IF EXISTS `email_smtp`;
CREATE TABLE `email_smtp` (
  `HOST` varchar(255) default NULL,
  `PORT` int(11) default NULL,
  `REPLY_TO` varchar(255) default NULL,
  `USERNAME` varchar(255) default NULL,
  `PASSWORD` varchar(255) default NULL,
  `AUTHP` tinyint(1) default NULL,
  `COMPANY_ID` int(11) NOT NULL,
  `ID` int(11) NOT NULL auto_increment,
  `SSLP` tinyint(1) default NULL,
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `event_types`
--

DROP TABLE IF EXISTS `event_types`;
CREATE TABLE `event_types` (
  `ID` int(11) NOT NULL default '0',
  `NAME` char(20) NOT NULL default '',
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `events`
--

DROP TABLE IF EXISTS `events`;
CREATE TABLE `events` (
  `SUMMARY` varchar(255) NOT NULL default '',
  `DESCRIPTION` varchar(255) NOT NULL default '',
  `TIMESTAMP` datetime NOT NULL default '0000-00-00 00:00:00',
  `SEVERITY_ID` int(11) NOT NULL default '0',
  `NOTE` varchar(255) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `COMPANY_ID` int(11) NOT NULL,
  `TYPE_ID` int(11) default NULL,
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`),
  KEY `SEVERITY_ID` (`SEVERITY_ID`),
  KEY `TIMESTAMP` (`TIMESTAMP`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `firewall`
--

DROP TABLE IF EXISTS `firewall`;
CREATE TABLE `firewall` (
  `NAME` char(200) NOT NULL,
  `COMPANY` char(200) default NULL,
  `VERSION` char(50) default NULL,
  `ENABLED` tinyint(1) default NULL,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `group_computer_links`
--

DROP TABLE IF EXISTS `group_computer_links`;
CREATE TABLE `group_computer_links` (
  `GROUP_ID` int(11) NOT NULL default '0',
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`),
  KEY `GROUP_ID` (`GROUP_ID`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `group_user_links`
--

DROP TABLE IF EXISTS `group_user_links`;
CREATE TABLE `group_user_links` (
  `GROUP_ID` int(11) NOT NULL,
  `USER_ID` int(11) NOT NULL,
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `USER_ID` (`USER_ID`),
  KEY `GROUP_ID` (`GROUP_ID`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `groups`
--

DROP TABLE IF EXISTS `groups`;
CREATE TABLE `groups` (
  `NAME` varchar(255) NOT NULL,
  `ALL_COMPUTERS` tinyint(1) NOT NULL,
  `REMOTE_PERMISSION` tinyint(1) NOT NULL,
  `SHUTDOWN_PERMISSION` tinyint(1) NOT NULL,
  `NOTE_PERMISSION` tinyint(1) NOT NULL,
  `COMPANY_ID` int(11) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `COMPANY_ID` (`COMPANY_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `hard_drives`
--

DROP TABLE IF EXISTS `hard_drives`;
CREATE TABLE `hard_drives` (
  `NAME` char(100) NOT NULL default '',
  `SIZE` bigint(20) default NULL,
  `INTERFACE_TYPE` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `FREE_SPACE` bigint(20) default NULL,
  `MANUFACTURER` char(100) default NULL,
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `hardware_errors`
--

DROP TABLE IF EXISTS `hardware_errors`;
CREATE TABLE `hardware_errors` (
  `DESCRIPTION` varchar(255) NOT NULL default '',
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `hotfixes`
--

DROP TABLE IF EXISTS `hotfixes`;
CREATE TABLE `hotfixes` (
  `NAME` char(100) NOT NULL default '',
  `DESCRIPTION` char(100) default NULL,
  `INSTALLED_BY` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `ip_address`
--

DROP TABLE IF EXISTS `ip_address`;
CREATE TABLE `ip_address` (
  `NAME` char(100) NOT NULL default '',
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `license_count`
--

DROP TABLE IF EXISTS `license_count`;
CREATE TABLE `license_count` (
  `ID` int(11) NOT NULL auto_increment,
  `NUM` int(11) NOT NULL default '0',
  `SOFTWARE_ID` int(11) NOT NULL default '0',
  `LICENSE_SCHEME_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  UNIQUE KEY `LICENSE_SCHEME_ID_1` (`LICENSE_SCHEME_ID`,`SOFTWARE_ID`),
  KEY `SOFTWARE_ID` (`SOFTWARE_ID`),
  KEY `LICENSE_SCHEME_ID` (`LICENSE_SCHEME_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `license_scheme`
--

DROP TABLE IF EXISTS `license_scheme`;
CREATE TABLE `license_scheme` (
  `NAME` char(100) NOT NULL default '',
  `ID` int(11) NOT NULL auto_increment,
  `COMPANY_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPANY_ID` (`COMPANY_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `logged_errors`
--

DROP TABLE IF EXISTS `logged_errors`;
CREATE TABLE `logged_errors` (
  `ID` int(11) NOT NULL auto_increment,
  `COMPANY_ID` int(11) NOT NULL,
  `BODY` text NOT NULL,
  `TIMESTAMP` datetime NOT NULL,
  PRIMARY KEY  (`ID`),
  KEY `COMPANY_ID` (`COMPANY_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `logical_drives`
--

DROP TABLE IF EXISTS `logical_drives`;
CREATE TABLE `logical_drives` (
  `NAME` char(100) default NULL,
  `SIZE` bigint(20) default NULL,
  `FREE_SPACE` bigint(20) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `memory`
--

DROP TABLE IF EXISTS `memory`;
CREATE TABLE `memory` (
  `CAPACITY` int(11) default NULL,
  `SPEED` int(11) default NULL,
  `FORM_FACTOR` varchar(100) default NULL,
  `MANUFACTURER` varchar(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `LOCATION` varchar(100) default NULL,
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `memory_arrays`
--

DROP TABLE IF EXISTS `memory_arrays`;
CREATE TABLE `memory_arrays` (
  `MAX_CAPACITY` int(11) default NULL,
  `NUM_SLOTS` int(11) default NULL,
  `ERROR_CORRECTION` int(11) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `motherboards`
--

DROP TABLE IF EXISTS `motherboards`;
CREATE TABLE `motherboards` (
  `NAME` char(100) default NULL,
  `MANUFACTURER` char(100) default NULL,
  `SERIAL_NUMBER` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `network_cards`
--

DROP TABLE IF EXISTS `network_cards`;
CREATE TABLE `network_cards` (
  `NAME` char(100) default NULL,
  `MAC_ADDRESS` char(100) default NULL,
  `MANUFACTURER` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `operating_systems`
--

DROP TABLE IF EXISTS `operating_systems`;
CREATE TABLE `operating_systems` (
  `NAME` char(50) default NULL,
  `VERSION` char(50) default NULL,
  `PRODUCT_ID` char(200) default NULL,
  `REGISTERED_USER` char(50) default NULL,
  `SERVICE_PACK` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `LICENSE_KEY` char(200) default NULL,
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `printers`
--

DROP TABLE IF EXISTS `printers`;
CREATE TABLE `printers` (
  `NAME` char(100) NOT NULL default '',
  `IS_DEFAULT` tinyint(1) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `processors`
--

DROP TABLE IF EXISTS `processors`;
CREATE TABLE `processors` (
  `NAME` char(100) default NULL,
  `INFO` char(200) default NULL,
  `ARCHITECTURE` char(100) default NULL,
  `L2_CACHE` int(11) default NULL,
  `CLOCK_SPEED` int(11) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `recent_tickets`
--

DROP TABLE IF EXISTS `recent_tickets`;
CREATE TABLE `recent_tickets` (
  `USER_ID` int(11) NOT NULL,
  `TICKET_ID` int(11) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `USER_ID` (`USER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `scarab_messages`
--

DROP TABLE IF EXISTS `scarab_messages`;
CREATE TABLE `scarab_messages` (
  `ID` int(11) NOT NULL auto_increment,
  `BODY` varchar(4096) default NULL,
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `service`
--

DROP TABLE IF EXISTS `service`;
CREATE TABLE `service` (
  `NAME` varchar(255) NOT NULL,
  `DESCRIPTION` varchar(255) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`),
  KEY `NAME` (`NAME`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `software`
--

DROP TABLE IF EXISTS `software`;
CREATE TABLE `software` (
  `NAME` text NOT NULL,
  `PUBLISHER` varchar(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPANY_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPANY_ID` (`COMPANY_ID`),
  KEY `NAME` (`NAME`(50))
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `software_computer_link`
--

DROP TABLE IF EXISTS `software_computer_link`;
CREATE TABLE `software_computer_link` (
  `SOFTWARE_ID` int(11) NOT NULL,
  `VERSION` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL,
  `PRODUCT_ID` char(100) default NULL,
  `LICENSE_KEY` char(100) default NULL,
  PRIMARY KEY  (`ID`),
  UNIQUE KEY `COMPUTER_ID_1` (`COMPUTER_ID`,`SOFTWARE_ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`),
  KEY `SOFTWARE_ID` (`SOFTWARE_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `software_event`
--

DROP TABLE IF EXISTS `software_event`;
CREATE TABLE `software_event` (
  `NAME` varchar(255) default NULL,
  `TIMESTAMP` datetime NOT NULL default '0000-00-00 00:00:00',
  `INSTALLED` tinyint(1) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `sound_devices`
--

DROP TABLE IF EXISTS `sound_devices`;
CREATE TABLE `sound_devices` (
  `NAME` char(100) default NULL,
  `MANUFACTURER` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `startups`
--

DROP TABLE IF EXISTS `startups`;
CREATE TABLE `startups` (
  `NAME` varchar(100) NOT NULL default '',
  `LOCATION` varchar(255) NOT NULL default '',
  `COMMAND` varchar(255) NOT NULL default '',
  `USER` varchar(100) NOT NULL default '',
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `subscriptions`
--

DROP TABLE IF EXISTS `subscriptions`;
CREATE TABLE `subscriptions` (
  `SUBSCRIPTION_ID` varchar(20) default NULL,
  `NAME` varchar(255) default NULL,
  `NUM_COMPUTERS` int(11) default NULL,
  `LEVEL` int(11) default NULL,
  `COMMITMENT` int(11) default NULL,
  `PHONE_NUMBER` varchar(20) default NULL,
  `COMPANY_ID` int(11) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  `TIMESTAMP` datetime default NULL,
  `STATUS` text,
  `FIRST_NAME` text,
  `LAST_NAME` text,
  `ADDRESS` text,
  `CITY` text,
  `STATE` text,
  `ZIP_CODE` text,
  `EMAIL` text,
  `DISCOUNT` int(11) default NULL,
  `AMOUNT` float default NULL,
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `system_notes`
--

DROP TABLE IF EXISTS `system_notes`;
CREATE TABLE `system_notes` (
  `ID` int(11) NOT NULL auto_increment,
  `TIMESTAMP` datetime default NULL,
  `NOTE` varchar(255) default NULL,
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `templar_updates`
--

DROP TABLE IF EXISTS `templar_updates`;
CREATE TABLE `templar_updates` (
  `ID` int(11) NOT NULL auto_increment,
  `BEGIN` varchar(255) default NULL,
  `END` varchar(255) default NULL,
  `FILES` text,
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `ticket_changes`
--

DROP TABLE IF EXISTS `ticket_changes`;
CREATE TABLE `ticket_changes` (
  `ID` int(11) NOT NULL auto_increment,
  `TICKET_ID` int(11) NOT NULL,
  `TIMESTAMP` datetime default NULL,
  `USER_ID` int(11) NOT NULL,
  `DESCRIPTION` varchar(255) default NULL,
  `NOTE` varchar(255) default NULL,
  PRIMARY KEY  (`ID`),
  KEY `TICKET_ID` (`TICKET_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `ticket_comments`
--

DROP TABLE IF EXISTS `ticket_comments`;
CREATE TABLE `ticket_comments` (
  `ID` int(11) NOT NULL auto_increment,
  `TICKET_ID` int(11) NOT NULL,
  `TIMESTAMP` datetime default NULL,
  `USER_ID` int(11) NOT NULL,
  `BODY` varchar(255) default NULL,
  PRIMARY KEY  (`ID`),
  KEY `TICKET_ID` (`TICKET_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `ticket_computers`
--

DROP TABLE IF EXISTS `ticket_computers`;
CREATE TABLE `ticket_computers` (
  `TICKET_ID` int(11) NOT NULL,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `TICKET_ID` (`TICKET_ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `ticket_emails`
--

DROP TABLE IF EXISTS `ticket_emails`;
CREATE TABLE `ticket_emails` (
  `HOST` varchar(255) default NULL,
  `PORT` int(11) default NULL,
  `USERNAME` varchar(255) default NULL,
  `PASSWORD` varchar(255) default NULL,
  `COMPANY_ID` int(11) NOT NULL default '0',
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `ticket_responses`
--

DROP TABLE IF EXISTS `ticket_responses`;
CREATE TABLE `ticket_responses` (
  `ID` int(11) NOT NULL auto_increment,
  `TICKET_ID` int(11) NOT NULL,
  `TIMESTAMP` datetime default NULL,
  `USER_ID` int(11) default NULL,
  `BODY` text,
  `SENDER` varchar(255) default NULL,
  PRIMARY KEY  (`ID`),
  KEY `TICKET_ID` (`TICKET_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `ticket_tags`
--

DROP TABLE IF EXISTS `ticket_tags`;
CREATE TABLE `ticket_tags` (
  `NAME` char(50) NOT NULL default '',
  `TICKET_ID` int(11) NOT NULL,
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `TICKET_ID` (`TICKET_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `tickets`
--

DROP TABLE IF EXISTS `tickets`;
CREATE TABLE `tickets` (
  `TICKET_ID` int(11) NOT NULL,
  `COMPANY_ID` int(11) NOT NULL,
  `ASSIGNED_USER_ID` int(11) default NULL,
  `STATE` int(11) NOT NULL default '0',
  `PRIORITY` int(11) NOT NULL default '0',
  `TIMESTAMP` datetime default NULL,
  `DUE_DATE` datetime default NULL,
  `SUBJECT` varchar(255) default NULL,
  `BODY` text,
  `RESPONSE_EMAIL` varchar(255) default NULL,
  `RATING` int(11) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `COMPANY_ID` (`COMPANY_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `user_accounts`
--

DROP TABLE IF EXISTS `user_accounts`;
CREATE TABLE `user_accounts` (
  `NAME` char(60) NOT NULL,
  `LOCKED` tinyint(1) default NULL,
  `DISABLED` tinyint(1) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `user_sessions`
--

DROP TABLE IF EXISTS `user_sessions`;
CREATE TABLE `user_sessions` (
  `SESSION_ID` char(100) NOT NULL,
  `EXPIRATION` datetime NOT NULL,
  `USER_ID` int(11) NOT NULL,
  `ID` int(11) NOT NULL auto_increment,
  PRIMARY KEY  (`ID`),
  KEY `USER_ID` (`USER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
CREATE TABLE `users` (
  `ID` int(11) NOT NULL auto_increment,
  `LEVEL` int(11) NOT NULL default '0',
  `COMPANY_ID` int(11) NOT NULL default '0',
  `NAME` varchar(100) default NULL,
  `EMAIL` varchar(50) default NULL,
  `USERNAME` varchar(50) NOT NULL default '',
  `PASSWORD` varchar(100) NOT NULL default '',
  `LAST_LOGIN` datetime default NULL,
  `WEEKLY_SOFTWARE_REPORT` tinyint(1) NOT NULL default '1',
  `RECENT_COMPUTERS` text,
  `TIMEZONE_PREFERENCE` int(11) default NULL,
  PRIMARY KEY  (`ID`),
  KEY `COMPANY_ID` (`COMPANY_ID`),
  KEY `USERNAME` (`USERNAME`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `video_controllers`
--

DROP TABLE IF EXISTS `video_controllers`;
CREATE TABLE `video_controllers` (
  `NAME` char(100) default NULL,
  `HORIZONTAL_RESOLUTION` int(11) default NULL,
  `VERTICAL_RESOLUTION` int(11) default NULL,
  `REFRESH_RATE` int(11) default NULL,
  `DRIVER` char(100) default NULL,
  `ID` int(11) NOT NULL auto_increment,
  `COMPUTER_ID` int(11) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `COMPUTER_ID` (`COMPUTER_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

