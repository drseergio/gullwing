-- phpMyAdmin SQL Dump
-- version 3.4.9
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: May 05, 2012 at 10:18 PM
-- Server version: 5.1.62
-- PHP Version: 5.3.11-pl0-gentoo

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";

--
-- Database: `gullwing`
--

-- --------------------------------------------------------

--
-- Table structure for table `breakouts`
--

CREATE TABLE IF NOT EXISTS `breakouts` (
  `symbol` varchar(255) NOT NULL,
  `date` date NOT NULL,
  `close` double NOT NULL,
  KEY `ticker` (`symbol`,`date`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `buys`
--

CREATE TABLE IF NOT EXISTS `buys` (
  `symbol` varchar(255) NOT NULL,
  `date` date NOT NULL,
  `ddate` date NOT NULL,
  `bdate` date NOT NULL,
  `price` double NOT NULL,
  KEY `ticker` (`symbol`,`date`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Stand-in structure for view `buy_symbols`
--
CREATE TABLE IF NOT EXISTS `buy_symbols` (
`symbol` varchar(255)
);
-- --------------------------------------------------------

--
-- Table structure for table `simulations`
--

CREATE TABLE IF NOT EXISTS `simulations` (
  `start_date` date NOT NULL,
  `end_date` date NOT NULL,
  `price_sma_short` int(11) NOT NULL,
  `price_sma_long` int(11) NOT NULL,
  `volume_sma` int(11) NOT NULL,
  `volume_multiplier` decimal(10,2) NOT NULL,
  `buy_after` int(11) NOT NULL,
  `macd_multiplier` decimal(10,2) NOT NULL,
  `lowest_multiplier` decimal(10,2) NOT NULL,
  `atr_days` int(11) NOT NULL,
  `atr_shift` int(11) NOT NULL,
  `atr_max` decimal(3,2) NOT NULL,
  `search_days` int(11) NOT NULL,
  `hold_days` int(11) NOT NULL,
  `profit_mult` decimal(3,2) NOT NULL,
  `score` decimal(10,2) NOT NULL,
  `trades` int(11) NOT NULL,
  `target` int(11) NOT NULL,
  `target_ratio` decimal(3,2) NOT NULL,
  `wins` int(11) NOT NULL,
  `win_ratio` decimal(3,2) NOT NULL,
  `lose` int(11) NOT NULL,
  `lose_ratio` decimal(3,2) NOT NULL,
  `sd` decimal(10,4) NOT NULL,
  `mean` decimal(10,4) NOT NULL,
  `median` decimal(10,4) NOT NULL,
  `IQR` decimal(10,2) NOT NULL,
  `run_time` decimal(10,2) NOT NULL,
  `timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `returns` text NOT NULL,
  `symbols_num` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 AUTO_INCREMENT=79 ;

-- --------------------------------------------------------

--
-- Table structure for table `transactions`
--

CREATE TABLE IF NOT EXISTS `transactions` (
  `symbol` varchar(255) NOT NULL,
  `date` date NOT NULL,
  `type` varchar(5) NOT NULL,
  `price` decimal(10,4) NOT NULL,
  `quantity` int(11) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Stand-in structure for view `transaction_symbols`
--
CREATE TABLE IF NOT EXISTS `transaction_symbols` (
`symbol` varchar(255)
);
-- --------------------------------------------------------

--
-- Table structure for table `watched`
--

CREATE TABLE IF NOT EXISTS `watched` (
  `ticker` varchar(255) NOT NULL,
  `name` varchar(255) NOT NULL,
  `exchange` varchar(255) NOT NULL,
  `marketcap` varchar(255) NOT NULL,
  PRIMARY KEY (`ticker`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Structure for view `buy_symbols`
--
DROP TABLE IF EXISTS `buy_symbols`;

CREATE ALGORITHM=UNDEFINED DEFINER=`root`@`localhost` SQL SECURITY DEFINER VIEW `buy_symbols` AS select distinct `buys`.`symbol` AS `symbol` from `buys`;

-- --------------------------------------------------------

--
-- Structure for view `transaction_symbols`
--
DROP TABLE IF EXISTS `transaction_symbols`;

CREATE ALGORITHM=UNDEFINED DEFINER=`root`@`localhost` SQL SECURITY DEFINER VIEW `transaction_symbols` AS select distinct `transactions`.`symbol` AS `symbol` from `transactions`;

