# -*- coding: utf-8 -*-
import logging
from MySQLdb import connect
from re import compile
from re import match
from re import MULTILINE
from re import sub
from simplejson import loads
from time import sleep
from urllib2 import build_opener
from urllib2 import HTTPError
from urllib2 import Request


LOG_FILE = 'stock_screener.log'

DB = {
  'host': 'localhost',
  'port': 3306,
  'user': 'gullwing',
  'pass': 'test',
  's_db': 'gullwing',
  'q_db': 'glquotes' }

SQL_CLEAR = 'DELETE FROM watched'
SQL_INSERT = 'INSERT INTO watched VALUES("%s", "%s", "%s", "%s")'
SQL_NOETF = ('DELETE FROM watched WHERE ticker != "NFLX" AND (name LIKE "%trust%" OR '
             'name LIKE "%etf%" OR '
             'name LIKE "%fund%" OR '
             'name LIKE "%bnd%" OR '
             'name LIKE "%ishares%" OR '
             'name LIKE "%ipath%" OR '
             'name LIKE "%index%")')

FETCH_URL = ('http://www.google.com/finance?start=0&num=4000&q=%5B('
    '(exchange%20%3D%3D%20%22NYSEARCA%22)%20%7C%20'
    '(exchange%20%3D%3D%20%22NYSEAMEX%22)%20%7C%20'
    '(exchange%20%3D%3D%20%22NYSE%22)%20%7C%20'
    '(exchange%20%3D%3D%20%22NASDAQ%22))%20%26%20'
    '(market_cap%20%3E%3D%20516750000)%20%26%20'
    '(market_cap%20%3C%3D%20420180000000)'
    '%5D&restype=company&output=json&noIL=1&')
REMOVE_JSON = compile(r'\\x[0-9]+', MULTILINE)


def screen_stocks(logger, db_config, url):
  conn, cursor = _db_connect(logger, db_config)
  data = _retrieve_json(logger, url)

  cursor.execute(SQL_CLEAR)
  logger.info('Dropped existing stocks from MySQL')
  inserts = 0
  for company in data['searchresults']:
    if (company['ticker'].find('-') == -1 and
        company['ticker'].find('.') == -1):
      cursor.execute(SQL_INSERT % (
          company['ticker'],
          company['title'],
          company['exchange'],
          company['columns'][0]['value']))
      inserts += 1
      logger.debug('Saving %s', company['ticker'])
  cursor.execute(SQL_NOETF)
  logger.info('Finished inserting %d stocks', inserts) 
  cursor.close()
  conn.close()


def _retrieve_json(logger, url):
  req = Request(url)
  opener = build_opener()
  while True:
    try:
      f = opener.open(req)
      break
    except HTTPError, e:
      logger.error('Failed to fetch data, trying again in 3sec: ' % e)
      sleep(3)
  data = f.read()
  logger.info('Fetched stock data from Google')
  return loads(sub(REMOVE_JSON, '', data))


def _db_connect(logger, config):
  conn = connect(
      host=config['host'],
      user=config['user'],
      passwd=config['pass'],
      db=config['s_db'])
  cursor = conn.cursor()
  return (conn,cursor)


def _get_logger():
  logger = logging.getLogger('stock_screener')
  handler =  logging.FileHandler(LOG_FILE)
  formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
  handler.setFormatter(formatter)
  logger.addHandler(handler)
  logger.setLevel(logging.DEBUG)
  return logger


if __name__ == '__main__':
  screen_stocks(_get_logger(), DB, FETCH_URL)
