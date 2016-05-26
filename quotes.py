# -*- coding: utf-8 -*-
from csv import reader
from datetime import datetime
from datetime import timedelta
import logging
from MySQLdb import connect
from MySQLdb import Warning as MysqlWarning
from Queue import Queue
from re import compile
from re import match
from re import MULTILINE
from re import sub
from simplejson import loads
from threading import Thread
from time import sleep
from urllib2 import build_opener
from urllib2 import HTTPError
from urllib2 import Request
from warnings import filterwarnings


LOG_FILE = 'quotes_sync.log'

DB = {
  'host': 'localhost',
  'port': 3306,
  'user': 'gullwing',
  'pass': 'test',
  's_db': 'gullwing',
  'q_db': 'glquotes' }

SQL_SYMBOLS = 'SELECT ticker FROM watched'
SQL_CREATE = ('CREATE TABLE IF NOT EXISTS `%s` ('
    '`date` date NOT NULL,'
    '`o` double NOT NULL,'
    '`h` double NOT NULL,'
    '`l` double NOT NULL,'
    '`c` double NOT NULL,'
    '`v` int(11) NOT NULL,'
    '`a` double NOT NULL,'
    'PRIMARY KEY (`date`))')
SQL_INSERT = ('INSERT INTO `%s` '
    'VALUES("%s", "%s", "%s", "%s", "%s", "%s", "%s")')
SQL_DROP = 'DROP TABLE %s'

FETCH_URL = 'http://ichart.finance.yahoo.com/table.csv?s=%s'
DAYS_TO_FETCH = 7300

filterwarnings('ignore', category=MysqlWarning)


def update_prices(logger, db_config, url, days):
  conn, cursor = _db_connect(logger, db_config, db_config['s_db'])
  _drop_tables(logger, db_config, cursor)
  symbols = _get_symbols(logger, cursor)
  cursor.close()
  conn.close()
  _update_stock(logger, db_config, symbols, url, days)


def _get_symbols(logger, cursor):
  cursor.execute(SQL_SYMBOLS)
  rows = cursor.fetchall()
  symbols = [row[0] for row in rows]
  logger.info('Got list of symbols')
  return symbols


def _update_stock(logger, db_config, symbols, url, days):
  pool = ThreadPool(20)
  for symbol in symbols:
    pool.add_task(_sync_one, logger, symbol, db_config, url, days)
  pool.wait_completion()


def _sync_one(logger, symbol, db_config, url, days):
  conn, cursor = _db_connect(logger, db_config, db_config['q_db'])
  logger.debug('Creating table for %s', symbol)
  cursor.execute(SQL_CREATE % symbol)

  failures = 0
  req = Request(url % symbol)
  opener = build_opener()
  while True:
    try:
      f = opener.open(req)
      break
    except HTTPError, e:
      if failures > 3:
        logger.fatal('Not trying to fetch %s, 3 consecutive failures', symbol)
        cursor.execute(SQL_DROP % symbol)
        cursor.close()
        conn.close()
        return
      logger.error('Failed fetching data for %s, retrying in 3sec: %s' % (symbol, e))
      failures += 1
      sleep(3)
  f.next()
  lines = reader(f)
  counter = 0
  for data in lines:
    if counter > days:
      break
    cursor.execute(SQL_INSERT % (symbol,
        data[0], data[1], data[2], data[3], data[4], data[5], data[6]))
    counter += 1
  cursor.close()
  conn.close()
  logger.debug('Downloaded and saved prices for %s', symbol)


def _get_logger():
  logger = logging.getLogger('quotes_sync')
  handler =  logging.FileHandler(LOG_FILE)
  formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
  handler.setFormatter(formatter)
  logger.addHandler(handler)
  logger.setLevel(logging.DEBUG)
  return logger


def _db_connect(logger, config, name):
  conn = connect(
      host=config['host'],
      user=config['user'],
      passwd=config['pass'],
      db=name)
  cursor = conn.cursor()
  return (conn,cursor)


def _drop_tables(logger, config, cursor):
  cursor.execute('DROP DATABASE %s' % config['q_db'])
  cursor.execute('CREATE DATABASE %s' % config['q_db'])
  logger.info('Dropped all tables in quote DB')


class Worker(Thread):
  def __init__(self, tasks):
    Thread.__init__(self)
    self.tasks = tasks
    self.daemon = True
    self.start()

  def run(self):
    while True:
      func, args, kargs = self.tasks.get()
      try:
        func(*args, **kargs)
      except Exception, e:
        print e
      finally:
        self.tasks.task_done()


class ThreadPool:
  def __init__(self, num_threads):
    self.tasks = Queue(num_threads)
    for _ in range(num_threads): Worker(self.tasks)

  def add_task(self, func, *args, **kargs):
    self.tasks.put((func, args, kargs))

  def wait_completion(self):
    self.tasks.join()


if __name__ == '__main__':
  update_prices(_get_logger(), DB, FETCH_URL, DAYS_TO_FETCH)
