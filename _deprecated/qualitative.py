# -*- coding: utf-8 -*-

# Ugly but simple script to retrieve sector, industry and industry %-held
# for a given list of .csv of trades (exported from gullwing)

from bs4 import BeautifulSoup
from csv import reader
from csv import writer
from quotes import ThreadPool
from quotes import Worker
import sys
from sys import argv
import traceback
from urllib2 import urlopen

BASE_URL = 'http://www.google.com/finance?q=%s'
OUT_ROWS = []


def _GetFinanceData(row):
  try:
    symbol = row[0]
    response = urlopen(BASE_URL % symbol)
    html = response.read()
    print(symbol)

    soup = BeautifulSoup(html, 'lxml')

    sector_link = soup.find(id='sector')
    if not sector_link:
      sector = 'NA'
      industry = 'NA'
    else:
      industry_link = sector_link.find_next_sibling('a')
      sector = sector_link.text
      industry = industry_link.text

    tds_values = soup.select('td[class="key"]')
    if len(tds_values) > 10:
      inst_own = tds_values[10].find_next_sibling('td').text.replace('\n', '').replace('%', '')
    else:
      inst_own = 'NA'

    OUT_ROWS.append([
        row[0], row[1], row[2], row[3], row[4], row[5], row[6],
        sector.encode('utf-8'), industry.encode('utf-8'), inst_own.encode('utf-8')])
  except:
    traceback.print_exc(file=sys.stdout)


def main():
  csv_filename = argv[1]
  data = reader(open(csv_filename, 'r'))
  out = writer(open('trades_out.csv', 'w'))

  data.next()
  out.writerow(["symbol","date","qty","price","val","pl","fees","sector","industry","inst_own"])

  pool = ThreadPool(10)
  for row in data:
    pool.add_task(_GetFinanceData, row)

  pool.wait_completion()

  for row in OUT_ROWS:
    out.writerow(row)

if __name__ == "__main__":
  main()
