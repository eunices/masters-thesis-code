import pandas as pd
import re

# Params
lookup = '../data/lookup/2019-06-01-spatial-layers-metadata.csv'
script = 'data_download_auto.sh'


df = pd.read_csv(lookup)

required_cols = [
    'title',
    'data_url',
    'dir',
    'zip'
]
rows = df.loc[df.direct_download == "yes", required_cols]
rows = rows.assign(variable = rows.dir.apply(lambda x: 'dir_' + re.sub('/', '', x)))
print(f'Script to download {len(rows)} datasets generated at {script}.')

f = open(script, "w")

f.write('#!/bin/bash\n')
f.write('# This script is automatically generated from {lookup} and data_downloads.py\n\n')

f.write('folder_dir=$(pwd)\n')
f.write('base_dir=${PWD%/*}\n')
f.write('geo_data_dir="$base_dir/data/geo"\n\n')

for idx, row in rows.iterrows():
    f.write(f'{row.variable}=$geo_data_dir{row.dir}\n')
f.write('\n\n')

f.write('echo "Activate conda environment"\n')
f.write('conda activate msc')
f.write('\n\n')

f.write('echo "Making all dirs"\n')
for idx, row in rows.iterrows():
    f.write(f'mkdir ${row.variable} -p\n')
f.write('\n\n')

for idx, row in rows.iterrows():
    f.write(f'echo "Download \'{row.title}\' to ${row.variable}..."\n')
    f.write(f'cd ${row.variable}\n')
    f.write(f'wget {row.data_url} -nc\n')
    if row.zip == "yes":
        if not '.zip' in row.data_url:
            f.write('mv $file $file.zip\n')
        f.write('unzip *.zip')
    f.write('\n\n')

f.close()