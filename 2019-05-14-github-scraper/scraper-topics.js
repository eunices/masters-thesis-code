var fs = require('fs');
var gs = require('github-scraper');

var filedir = 'data-in/github/'

processJson('species-distribution-modelling')

function processJson(keywords) {

    query = `topics/${keywords}`

    var filename = keywords.replace(/[^a-z0-9+]+/gi, '-')
    var filepath = `../${filedir}github-${filename}-2019-05-14.json`

    // console.log(filepath, "FILE")

    if (!fs.existsSync(filepath)) {
    
        gs(query, function(err, data) {
            var json = JSON.stringify(data)
            if (json){
                console.log('test')
                fs.writeFile(filepath, json, function (err) {
                    if (err) {
                    } 
                })
            }
        })
    }
}