
import requests
import pandas as pd
from bs4 import BeautifulSoup



lavori = 'carrozzeria'
zone = 'milano'
database_scar = []
for i in range(1, 2000):
    url = "https://www.paginegialle.it/ricerca/" + lavori.replace(" ","%20") + "/" + zone.replace(" ","%20")+"/p-"+str(i)
    try:
        response = requests.get(url)
    except:
        response = "<Response [403]>"
    if str(response) == "<Response [200]>":
        soup = BeautifulSoup(response.text, "html.parser")
        all_links = [a['href'] for a in soup.find_all('a', class_="btn btn-black icn-sitoWeb shinystat_ssxl")]
        for company_link in all_links:
            try:
                response = requests.get(company_link)
            except:
                response = "<Response [403]>"
            if str(response) == "<Response [200]>":
                soup = BeautifulSoup(response.text, "html.parser")
                try:
                    mail = str([a['href'][7:].lower() for a in soup.find_all('a') if a['href'][:7].lower()=='mailto:'][-1])
                    if len(mail) > 5:
                        database_scar.append([company_link, mail, lavori, zone])
                except:
                    pass

df = pd.DataFrame(database_scar, columns=["Sito-web","Email","Tipo di struttura","Luogo"])
df.to_csv('mail_scaricate.csv')