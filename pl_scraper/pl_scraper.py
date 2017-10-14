# Author and Copyrights: Kim Hammar <kimham@kth.se> 2017
# Web scraper for PL-tables. http://www.emfootball.co.uk/oldleaguetables.html

from bs4 import BeautifulSoup
import requests
import re
import json


def main():
    "Entrypoint of the scraper"
    baseUrl = "http://www.emfootball.co.uk"
    linkPage = "oldleaguetables.html"
    Links = list(map(lambda relLink: baseUrl + "/" + relLink.get("href"), urls(baseUrl + "/" + linkPage)))
    parsedData = list(map(parseLeaguePage, Links))
    with open('result.json', 'w') as file:
        json.dump(parsedData, file)
    with open('test.json', 'w') as file:
        json.dump(parsedData[0], file)


def parseLeaguePage(url):
    "Parsres a league page"
    print("Parsing league page: " + url)
    leaguePage = requests.get(url)
    leaguePageSoup = BeautifulSoup(leaguePage.text, "lxml")
    tables = leaguePageSoup.find_all("table")
    filteredTables = list(filter(isTable, tables))
    titles = list(map(lambda h3: concatStrings(h3.strings), leaguePageSoup.find_all("h3")))
    filteredTitles = list(filter(isLeagueTitle, titles))
    year = parseYear(leaguePageSoup)
    print("parsed year: " + str(year))
    parsedTables = parseTables(filteredTables, filteredTitles, leaguePageSoup)
    return {
        "year": str(year),
        "tables": parsedTables
        }


def parseYear(leaguePageSoup):
    "parse year of page"
    h2 = leaguePageSoup.find("h2")
    if h2 is None:
        h3 = leaguePageSoup.find_all("h3")
        title = h3[0]  # First title
        regex = re.compile(r'\d{4}/\d{1,4}')  # YYYY/YYYY
        match = regex.search(title.font.string)
        return match.group(0)
    else:
        title = h2
        regex = re.compile(r'\d{4}/\d{1,4}')  # YYYY/YYYY
        string = concatStrings(h2.strings)
        match = regex.search(string)
        return match.group(0)


def isLeagueTitle(title):
    "check if the given title is a league title or something else"
    if title is None:
        return False
    regex = re.compile(r'\d{4}-\d{4}')
    match = regex.search(title)
    if match is None:
        return False
    return True


def isTable(table):
    "check if the given table is a league-table"
    if len(table.find_all("table")) == 0:  # No nested tables should match
        for tr in table.find_all("tr"):
            Texts = list(map(lambda td: td.string, tr.find_all("td")))
            if "Pts" in Texts and "Team" in Texts:
                return True
        for tr in table.find_all("tr"):
            Texts = list(map(lambda th: concatStrings(th.strings), tr.find_all("th")))
            if ("Pts" in Texts or "\nPts") and ("Team" in Texts or "\nTeam" in Texts):
                return True
    return False


def concatStrings(strings):
    "Helper function to concat a list of strings"
    string = ""
    for str in strings:
        string = string + str
    return string


def parseTables(tables, titles, leaguePageSoup):
    "Parses tables without titles"
    if(len(titles) == 0):
        return parseTablesWithTitles(tables, leaguePageSoup)
    parsedTables = list()
    for i in range(0, len(tables)-1):
        table = parseTable(tables[i], titles[i])
        parsedTables.append(table)
    return parsedTables


def parseTablesWithTitles(tables, leaguePageSoup):
    "parse tables which have titles"
    parsedTables = list()
    for table in tables:
        parsedTables.append(parseTableWithTitle(table, leaguePageSoup))
    return parsedTables


def parseTable(table, title):
    "parse HTML table into key-value map"
    rows = table.findAll("tr")
    rows.pop(0)  # First two rows are headers
    colRow = rows.pop(0)
    cols = list(map(lambda col: concatStrings(col.strings), colRow.find_all("td")))
    if(len(cols) == 0):
        cols = list(map(lambda col: concatStrings(col.strings), colRow.find_all("th")))
    parsedRows = list(map(parseRow, [(row, cols) for row in rows]))
    return {
        "title": title,
        "tableRows": parsedRows
    }


def parseTableWithTitle(table, leaguePageSoup):
    "parse HTML table with title into key-value map"
    rows = table.findAll("tr")
    Texts = list(map(lambda td: concatStrings(td.strings), rows[0].find_all("td")))
    title = "missing"
    if("Home" in Texts or "Away" in Texts or "\nHome" in Texts or "\nAway" in Texts):
            titlesh3 = list(map(lambda h3: h3.font, leaguePageSoup.find_all("h3")))
            filteredh3Titles = list(filter(isLeagueTitle, titlesh3))
            if(len(filteredh3Titles) == 0):
                titlesh2 = list(map(lambda h2: concatStrings(h2.strings), leaguePageSoup.find_all("h2")))
                title = titlesh2[0]
            else:
                title = filteredh3Titles[0]
    else:
        title = concatStrings(rows[0].strings)
    rows.pop(0)  # First two rows are headers
    colRow = rows.pop(0)
    cols = list(map(lambda col: concatStrings(col.strings), colRow.find_all("td")))
    if(len(cols) == 0):
        cols = list(map(lambda col: concatStrings(col.strings), colRow.find_all("th")))
    parsedRows = list(map(parseRow, [(row, cols) for row in rows]))
    return {
        "title": title,
        "tableRows": parsedRows
    }


def parseRow(rowCols):
    "parse row of HTML table into dict"
    row, cols = rowCols
    cells = row.findAll("td")
    return {cols[i]: concatStrings(cells[i].strings) for i in range(len(cells))}


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
