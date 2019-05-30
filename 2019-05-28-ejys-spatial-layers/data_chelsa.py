import json
import pandas as pd
import re
import sys

fdir = '../data/geo/chelsa'
base_url = 'https://www.wsl.ch/lud/chelsa/data'

if __name__ == "__main__":

    # First part to modify js file so that it dumps the js object as JSON
    if sys.argv[1] == 'part1':
        path = f'{fdir}/index.js'
        f = open(path, 'r')
        d = f.read()
        f.close()

        l1 = '''var fs = require('file-system');

'''

        l2 = '''

fs.writeFile("index.json", JSON.stringify(dirs), (err) => {
    if (err) console.log(err);
    console.log("JSON converted from js object and successfully Written to index.json.");
});
'''
        new = l1 + d + l2

        path = f'{fdir}/index.js'
        f = open(path, 'w+')
        f.write(new)
        f.close()

        f.close()

    # Second part to get all URLs from json object
    if sys.argv[1] == 'part2':
        path = f'{fdir}/index.json'

        print(f'{path}')

        f = open(path, 'r')
        d = f.read()
        js = json.loads(d)
        f.close()

        # Checking out structure of json
        # print(js.keys())
        # print(js['bioclim'].keys())
        # print(js['bioclim']['integer']['f']) # all bioclim10
        # print(js['climatologies'].keys())
        # print(js['climatologies']['prec']['f'])
        # print(js['climatologies']['temp']['integer'].keys())
        # print(js['climatologies']['temp']['integer']['tmax'])
        # print(js['climatologies']['temp']['integer']['tmin'])
        # print(js['climatologies']['temp']['integer']['temp'])
        
        df1 = pd.DataFrame(js['bioclim']['integer']['f'])
        df1['dir'] = 'bioclim/integer'

        df2 = pd.DataFrame(js['climatologies']['prec']['f'])
        df2['dir'] = 'climatologies/prec'

        df3 = pd.DataFrame(js['climatologies']['temp']['integer']['tmax']['f'])
        df3['dir'] = 'climatologies/temp/integer/tmax'

        df4 = pd.DataFrame(js['climatologies']['temp']['integer']['tmin']['f'])
        df4['dir'] = 'climatologies/temp/integer/tmin'

        df5 = pd.DataFrame(js['climatologies']['temp']['integer']['temp']['f'])
        df5['dir'] = 'climatologies/temp/integer/temp'

        df = pd.concat([df1, df2, df3, df4, df5])
        url = ''
        df['url'] = base_url + "/" + df['dir'] + "/" + df['n']
        print(df.head())

        df = df.loc[df.n.str.contains('.tif'), ]
        df.to_csv(f'{fdir}/CHELSA-datasets.csv', index=False)

    # Second part to download appropriate datasets
    if sys.argv[1] == 'part3':
        df = pd.read_csv(f'{fdir}/CHELSA-datasets.csv')

        requirement = 'bioclim'
        li = df.loc[df.dir.str.contains(requirement),]['url'].tolist()

        f = open('CHELSA-url.txt', 'w+')
        for i in li:
            f.write(f'{i}\n')
        f.close()