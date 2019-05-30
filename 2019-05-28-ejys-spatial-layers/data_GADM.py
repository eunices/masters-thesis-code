"""Scrape GADM website for geospatial package link."""
from bs4 import BeautifulSoup
from datetime import datetime as dt
from selenium import webdriver
from selenium.webdriver.support.ui import Select
import requests 
import time 
import pandas

url = 'https://gadm.org/download_country_v3.html'
base_url = 'https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/'

def get_countries():
    """Get all countries and associated value."""
    page = requests.get(url)
    html = BeautifulSoup(page.text, 'html.parser')
    select = html.find('select', {'id': 'countrySelect'})
    options = select.find_all('option')
    country_values = [x['value'] for x in options]
    countries = [x.text for x in options]
    return country_values, countries


def click():
    """Get all the gpkg links (index of links)."""
    browser = webdriver.PhantomJS()
    browser.set_window_size(1120, 550)
    browser.get(url)

    dropdown = Select(browser.find_element_by_id('countrySelect'))
    li_text = [o.text for o in dropdown.options]
    li_link = []
    
    for v in li_text:
        # Click option
        dropdown.select_by_visible_text(v)

        # Get html link
        el = browser.find_elements_by_tag_name('a')
        links = [x.get_attribute('href') for x in el]
        link = links[5] # Get shp format
        li_link.append(link)

        print(f'{dt.now()} Click {v} {link}')

        # # Save screenshot (testing purposes)
        # browser.save_screenshot(f'test{v}.png')

    browser.quit() 
    return li_link[1:]


if __name__ == "__main__":
    # values, countries = get_countries()
    links = click()

    f = open('GADM-countries.txt', 'w+')
    for i in links:
        f.write(f'{i}\n')
    f.close()


