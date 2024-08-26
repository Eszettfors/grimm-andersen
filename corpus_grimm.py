import pandas as pd
import requests

df = pd.read_excel("Corpus_grimm.xlsx") # can be changed to any excel file of choice, make sure the file contains a column with the same length as the number of texts to be scraped.

def create_indexes(x):
    """
    takes a integer x as input and creates a list of strings with the indexes of the files to be downloaded in the format 00x, 0xx or xxx.
    The result should then be used to download the files in the function download_txt_file.
    """
    list_of_indexes = []
    for i in range(1,x):
        if i < 10:
            list_of_indexes.append("00"+str(i))
        elif i < 100:
            list_of_indexes.append("0"+str(i))
        else:
            list_of_indexes.append(str(i))
    return list_of_indexes

def download_txt_file(index):
    """
    takes a string index as input and downloads the corresponding txt file from the website with the grimm stories.
    """
    url = "https://www.cs.cmu.edu/~spok/grimmtmp/" + index + ".txt"
    response = requests.get(url)
    return response

indexes = create_indexes(len(df)+1)
list_of_texts = []
for i in indexes:
        text = download_txt_file(i)
        print('downloaded ' + str(i))
        list_of_texts.append(text.text)

for i in range(len(df)):
    df.Text.iloc[i] = list_of_texts[i]

df.to_excel('Corpus_grimm_with_text.xlsx')




