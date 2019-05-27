
from datetime import datetime as dt
from dotenv import load_dotenv
from os import mkdir
from os.path import join, dirname, isdir
import sys, re, os
import zipfile
import shutil

import pandas as pd

from dwca.read import DwCAReader
from pygbif import occurrences as occ


# Load file from the path
dotenv_path = join(dirname(__file__), '.env')
load_dotenv(dotenv_path)


# Params
logfile = 'datasets.log'
basepath = os.path.basename(os.path.dirname(os.path.realpath(__file__)))
dir_data = f'../data/{basepath}'
# Addition parameters to provide as the first argument include whether it is 
# 'dl' (= download) or 'q' (= query).


# Query
if sys.argv[1] == 'q':

    # Form query string
    # For more information: https://pygbif.readthedocs.io/en/latest/modules/occurrence.html

    # Multiple taxa [does not appear to work as dataset has 0 records!]
    # apoidea = {
    #     'apidae': 4334,
    #     'halictidae': 7908,
    #     'andrenidae': 7901,
    #     'megachilidae': 7911,
    #     'colletidae': 7905,
    #     'melittidae': 4345,
    #     'stenotritidae': 7916,
    # }
    # query = [f'taxonKey = {x}' for x in apoidea.values()]
    
    # Single taxa
    # species ='1335213' # Megachile sculpturalis
    # query = f'taxonKey = {species}'

    # Kick start process of requesting for dataset
    # Should see download at https://gbif.org/user/download
    print(f'{dt.now()} Query is {query}.')
    meta = occ.download(query)
    key = meta[0]

    # Log identifier
    f = open(logfile, "a")
    f.write(f'{key}: {query}\n')
    f.close()


# Download dataset
if sys.argv[1] == 'dl':
    f = open(logfile, "r")
    d = f.readlines()
    d = [re.sub('\n', '', line).split(': ') for line in d]

    if not isdir(dir_data):
        mkdir(dir_data)

    # Download dataset
    for idx, i in enumerate(d):
        zipf = occ.download_get(key=i[0], path=dir_data)

        # To extract the zipfile
        zip_ref = zipfile.ZipFile(zipf['path'], 'r')
        extract_to = re.sub('.zip', '', zipf['path'])
        zip_ref.extractall(extract_to)
        zip_ref.close()

        # Check if file contains .csv or darwin core archives (.xml)
        check_xml = [x.lower().endswith('.xml') for x in os.listdir(extract_to)]
        check_csv = [x.lower().endswith('.csv') for x in os.listdir(extract_to)]

        # Remove zip folder
        shutil.rmtree(extract_to)

        with DwCAReader(zipf['path']) as dwca:

            if any(check_xml):
                # For more information: https://python-dwca-reader.readthedocs.io/
                df = dwca.pd_read(f'occurrence.txt', parse_dates=True)

            if any(check_csv):
                df = pd.read_csv(f'{i}.csv')

            # Summarise df only if it exists
            try:
                df
            except NameError:
                df_exists = False
            else: 
                df_exists = True

        if df_exists:
            print(f'{dt.now()} File contains {len(df)} rows.')
            df.to_csv(f'{dir_data}/analysis.csv')
