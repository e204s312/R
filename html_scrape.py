# -*- coding: utf-8 -*-
"""
Created on Wed Jun 21 11:02:01 2017

@author: jrbrad
"""

import requests
from bs4 import BeautifulSoup as bsoup

    
my_wm_username = 'ebsmith01'
search_url = 'http://publicinterestlegal.org/county-list/'
response = requests.get(search_url, headers={
            "User-Agent": "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"}).content
        

parsed_html = bsoup(response,"lxml")
target_rows = parsed_html.find_all('tr')
all_counties = []
for row in target_rows:
    new_row = []
    for x in row.find_all('td'):
        new_row.append(x.text)    
        
    all_counties.append(new_row)

print(my_wm_username)
print(len(all_counties))
print(all_counties)

