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
  data.numResults = $('span[class="counter"]').text().trim();

  $('article').each(function (i) {
    var repo = {
      title    : $(this).find('a[data-ga-click="Explore, go to repository, location:topic repositories"]').first().text().trim(),
      link     : `https://github.com${$(this).find('a[data-ga-click="Explore, go to repository, location:topic repositories"]').attr('href')}`,
      desc     : $(this).find('div[class="text-gray mb-3 ws-normal"]').first().text().trim(),
      progLang : $(this).find('span[itemprop="programmingLanguage"]').first().text().trim(),
      stars    : $(this).find('a[class="d-inline-block link-gray"]').text().trim(),
    }
    data.repos.push(repo);
  }
  );

  return callback(null, data)
}

module.exports = repo;