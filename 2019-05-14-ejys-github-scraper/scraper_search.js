var fs = require('fs');
var gs = require('github-scraper');

var keywords = [
    // 'species+distribution',
    // 'SDM',
    // 'niche',
    // 'species+range',
    // 'species+occupancy',
    // 'species+diversity+extrapolation'
]

var languages = ['R', 'python']
function createArray(N) {
    var a = [];
    for (var i = 1; i <= N; i++) {
        a.push(i);
    }
    return a;
}


var urls = []
keywords.forEach(function(q) {
    languages.forEach(function(l) {
        createArray(17).forEach(function(p) {
            queryString = `/search?q=${q}&l=${l}&p=${p}`
            urls.push(queryString)
        })
    })
})


var dir = '../data_in/github'
var today = new Date()
var dd = String(today.getDate()).padStart(2, '0');
var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
var yyyy = today.getFullYear();
today = `${yyyy}-${mm}-${dd}`;
var filedir = `${dir}/${today}/`

function processJson(url) {

    if (!fs.existsSync(filedir)) {
        fs.mkdir(filedir, { recursive: true })
    }

    query = `${url}&ref=advsearch&type=Repositories&utf8=%E2%9C%93`

    var filename = url.replace(/[^a-z0-9+]+/gi, '-')
    var filepath = `${filedir}${filename}.json`

    if (!fs.existsSync(filepath)) {
    
        gs(query, function(err, data) {
            var json = JSON.stringify(data)

            if (data){
                fs.writeFile(filepath, json, function (err) {
                    if (err) {
                        setTimeout(function () {processJson(query)},  Math.floor(Math.random() * Math.floor(5)) * 5000)
                    } 
                })
            }
        })
    }
}


urls.forEach(function(url) {
    timeout = Math.floor(Math.random() * Math.floor(5)) * 5000
    console.log(`Timeout: ${timeout}`)
    setTimeout(function () {processJson(url)}, timeout)
})

// processJson(urls[0])