/**
 * repo method scrapes a given GitHub repository page
 * @param {Object} $ - cheerio object with DOM of page to be scraped
 * @param {string} project - a valid GitHub repository name
 * @param {function} callback - the callback we should call after scraping
 *  a callback passed into this method should accept two parameters:
 *  @param {object} error - an error object (set to null if no error occurred)
 *  @param {array} data - list of (Public) information on the GitHub repository
 */

function repo ($, url, callback) {
  var data = { "url" : url, type: 'repo', repos: []};
  // data.info = $('ul[class="repo-list"]').text().trim();
  data.numResults = $('h3').text().trim();

  $('.repo-list-item').each(function (i) {
    var repo = {
      title    : $(this).find('.v-align-middle').first().text().trim(),
      link     : `https://github.com${$(this).find('a[class="v-align-middle"]').attr('href')}`,
      desc     : $(this).find('p').first().text().trim(),
      progLang : $(this).find('span[itemprop="programmingLanguage"]').first().text().trim(),
      stars    : $(this).find('a[class="muted-link"]').text().trim(),
    }
    data.repos.push(repo);
  });
 
  return callback(null, data)
}

module.exports = repo;