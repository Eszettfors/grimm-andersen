import re
import requests
from bs4 import BeautifulSoup
import pandas as pd

main_url = "http://hca.gilead.org.il/"

# Parse the main page HTML content
soup = BeautifulSoup(response.content, 'html.parser')
links = soup.find_all('a')
data = []

# Iterate over the links
for link in links:
    href = link.get('href')
    if href and href.endswith('.html'):
        book_url = main_url + href
        book_response = requests.get(book_url)
        book_response.raise_for_status()
        book_soup = BeautifulSoup(book_response.content, 'html.parser') #parse the links
        
        title = book_soup.title.string if book_soup.title else "No Title" # access title

        paragraphs = book_soup.find_all('p')
        text = "\n".join(paragraph.get_text() for paragraph in paragraphs) #extract texts
        data.append({
            'title': title,
            'text': text
        })

df = pd.DataFrame(data)

def clean_text(text):
    """
    Takes a string and cleanes dirt from html scraping
    """
    text = re.sub(r'<.*?>', '', text)  # Remove HTML tags
    text = re.sub(r'[^A-Za-z0-9\s.,]+', '', text)  # Remove non-alphanumeric characters except for periods and commas
    text = re.sub(r'\s+', ' ', text).strip()  # Replace multiple spaces with a single space
    return text

df.title = df.title.apply(clean_text)
df.text = df.text.apply(clean_text)

def remove_hca(text):
    """
    removes the string "Hans Christian Andersen" which is attached to every title
    """
    text = re.sub(r'Hans Christian Andersen', '', text)
    return text

df.title = df.title.apply(remove_hca)
df = df.drop(index = [i for i in range(9)])

df.to_excel('HansChristianAndersen.xlsx', index = False)
