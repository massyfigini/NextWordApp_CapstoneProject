The Next Word App
========================================================
author: Massimiliano Figini
date: 2017-06-18
autosize: true


Intro
========================================================

The "Next Word App" is the result of my Capstone Project of the Data Science Specialization by Johns Hopkins University on Coursera.
The goal of the Capstone project is to create an algorithm to predict a word from one or more words, and to provide an interface that can be accessed by others.


The app
========================================================

The "Next Word App" predict the next word given one or more words.
You can find the app [here](https://massyfigini.shinyapps.io/NextWordApp/).
In the input tab, you have to write one or more words and press the 'Go!' button, and the app will predict the next word.
Other possibile words are shown bottom.


Data
========================================================

The English 'Corpora' data are the starting point for the algorithm, you can find more information [here](https://web-beta.archive.org/web/20160930083655/http://www.corpora.heliohost.org/aboutcorpus.html). 


Algorithm
========================================================

The algorithm is created starting from the 'Corpora' data. The data are first divided in sentences, than I have made the bigram, trigram and 4-gram data. 
The n-grams are ranked on their importance by their frequencies.
Every phrase you insert, the algorithm take first the last three words and search in the 4-gram the most common fourth word. If there isn't, it take the last two words and search in the trigram the most common third word. If there isn't again, it takes the last and search in the bigram the most common second word. The result is shown in the "Top probability next word" area.
Other possibile words takes from the bigram, trigram and 4-gram are shown in the "Other possibly words" area.
