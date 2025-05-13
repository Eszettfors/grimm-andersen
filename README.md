This repository was created for the course 'Doing Research with Text Corpora' at the University of Vienna during the sommersemester 2024, and contains the code used to compare the language of the influential European fairytale authors H.C. Andersen and the Grimm Brothers.

The repository consists of two python scripts used to gather the stories of HC andersen and the grimm brothers by means of scraping, the results of which can be found in the data folder. The R-script 'Measurements' creates corpora for both authors, generates linguistic complexity measures, and tags its lexicon with sentiments, exporting the results to the output folder. In Analysis.R, this data is then analyzed using NHST and a generalized linear model. 

An interactive 3d scatterplot is deployed, placing each text by the authors in their 3D sentiment spaces. It can be accessed through this link: https://eszettfors.github.io/grimm-andersen/3d_scatterplot.html
