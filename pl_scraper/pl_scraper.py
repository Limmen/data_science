# Web scraper for PL-tables. http://www.emfootball.co.uk/oldleaguetables.html

from bs4 import BeautifulSoup
import requests


def main():
    "Entrypoint of the scraper"
    baseUrl = "http://www.emfootball.co.uk"
    linkPage = "oldleaguetables.html"
    Links = list(map(lambda relLink: baseUrl + "/" + relLink.get("href"), urls(baseUrl + "/" + linkPage)))
    getTable(Links[0])


def getTable(url):
    "get the league tables from a url"
    print("Getting table from URL: " + url)
    tablePage = requests.get(url)
    tablePageSoup = BeautifulSoup(tablePage.text, "lxml")
    tables = tablePageSoup.find_all("table")
    print("Parsed " + str(len(tables)) + " tables")
    leagueTable = filter(isTable, tables)
    print("LeagueTable: " + LeagueTable.prettify())

def isTable(table):
    "check if the given table is a league-table"
    for tr in table.find_all("tr"):
        Texts = list(map(lambda th: th.text, tr.find_all("th")))
        if "Pos" in Texts and "Team" in Texts:
            return True


def urls(url):
    "Scrape the league urls of index"
    linksPageResponse = requests.get(url)
    linksSoup = BeautifulSoup(linksPageResponse.text, "lxml")
    allLinks = linksSoup.find_all('a')
    filterFun = lambda link: (("table" in link.get("href")) or ("leagetable" in link.get("href"))) and ("http://www.emfootball.co.uk/" not in link.get("href"))
    leagueTableLinks = filter(filterFun, allLinks)
    return leagueTableLinks


if __name__ == "__main__":
    main()
