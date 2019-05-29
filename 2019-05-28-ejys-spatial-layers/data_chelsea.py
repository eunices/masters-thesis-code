"""Scrape CHELSA climate website for geoTIFF links."""
from bs4 import BeautifulSoup
from datetime import datetime as dt
from selenium import webdriver
from selenium.webdriver.support.ui import Select
import requests 
import time 
import pandas


url = 'http://chelsa-climate.org/downloads/'
download_url = ''

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
        link = links[4] # Get gpkg format
        li_link.append(link)

        print(f'{dt.now()} Click {v} {link}')

        # # Save screenshot (testing purposes)
        # browser.save_screenshot(f'test{v}.png')

    browser.quit() 
    return li_link[1:]


if __name__ == "__main__":
    # values, countries = get_countries()
    links = click()

    f = open('countries_GADM.txt', 'w+')
    for i in links:
        f.write(f'{i}\n')
    f.close()


