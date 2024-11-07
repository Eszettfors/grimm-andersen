This repository was created for the course 'doing research with textcorpora' at the university of Vienna during the sommersemester 2024, and contains the code used to compare the language of the influental european fairytale authors H.C. Andersen and the Grimm Brothers.

The repository consists of two python scripts used to gather the stories of HC andersen and the grimm brothers by means of scraping, the results of which can be found in the data folder.
The R-script 'Measurements' creates a corpora for both authors and generates linguistic complexity measures as well as tags its lexicon with sentiments, exporting the results to the output folder.
In Analysis.R this data is then analyzed by means of NHST and a generalized linear model. 

We also deploy an interactive 3d scatterplot, visualizing the sentiment model, with sentiment values for all stories and the texts of the stories.
