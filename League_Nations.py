#Scraper for League of Nations Data

import util
import PyPDF2
import urllib
import requests



import re
import util
import bs4
import queue
import json
import sys
import csv
################################################################################################################################################################

limiting_domain = "http://digital.library.northwestern.edu/league/stat.html"
 
def get_neighbors(node):
    print("      completed")
    neighbors = []
    test_link = []
    response = util.get_request(node)
    if response == None:
        print('No response')
        neighbors = None
    else:
        text = util.read_request(response)
        if text == "":
            print("No text read")
            neighbors = None
        else:
            soup = bs4.BeautifulSoup(text, "html5lib")
            for link in soup.find_all("a"):
                url_raw = link.get("href")
                url_rel = util.remove_fragment(url_raw)
                url = util.convert_if_relative_url(node, url_rel)
                print(url)
                if url != None:   
                    neighbors.append(url)
    return neighbors, response, soup

test = get_neighbors(limiting_domain)
link = test[0][5]




def read_link(node):
    neighbors = get_neighbors(limiting_domain)
    #for url in neighbors:
    #if 'html' not in url:
    #pdfFile = open(url.split('/')[-1], 'w')
    #pdfFile.write(webFile.read())
    #webFile.close()
    #pdfFile.close()

    #base = os.path.splitext(pdfFile)[0]
    #os.rename(pdfFile, base + ".pdf")
    #open_link = subprocess.Popen([node],shell=True)


    response = urllib.urlopen(node)
    file = open("document.pdf", 'wb')
    file.write(response.read())
    file.close()
    print("Completed")


    pdf = PyPDF2.PdfFileReader(open("test.pdf", "rb"))
    for page in pdf.pages:
        print(page.extractText())