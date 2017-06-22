# Massimiliano Figini 2017-06-18
# The Next Word App

library(shiny)

shinyUI(fluidPage(
  
  tags$head(tags$style(
    HTML('
         #word {
            color: red;
            font-size: 300%;
         }

         #others {
            color: red;
            font-size: 200%;
         }

        tabPanel{ 
          font-family: "Calibri";
          color: red;
        }

        body, label, input, button, select { 
          font-family: "Calibri";
          background: #000000 url("http://massyfigini.github.io/assets/css/images/Mario.gif")  bottom left;
          background-repeat: no-repeat;
		      background-size: cover;
		      background-position: center center;
        }'))),
        #	background-attachment: fixed;  (da aggiungere se grandezza ok e non devo adattarlo)

  
  titlePanel(HTML("<font size=10 color=white><b><center>The Next Word App</center></b></font><br/>"),windowTitle="The Next Word App"),
  
  sidebarLayout(
    sidebarPanel(tags$style(".well {background-color:#d3d3d3;}"),
     tabsetPanel(
       
       # First tab (input)
       tabPanel(HTML("<font color=black>Input</font>"),
                textInput("Word", NULL, 
                          placeholder = "Write the words here...")
                ,actionButton("go","Go!", icon = icon("plane"))
                ,HTML("<br/><br/><br/>&copy;massyfigini")),
       
       # Second tab (Instruction)
       tabPanel(HTML("<font color=black>About the app</font>"),
                HTML("<br/><b>Instruction</b><br/>
This app predict the next word given one or more words.<br/>
In the input tab, you have to write one or more words and press the 'Go!' button, and the app will predict the next word.<br/>
<br/><b>Data</b><br/>
The English 'Corpora' data are the starting point for the algorithm, you can find more information 
<a href=https://web-beta.archive.org/web/20160930083655/http://www.corpora.heliohost.org/aboutcorpus.html>here</a>.
<br/><br/><b>Algorithm</b><br/>
The algorithm is created starting from the 'Corpora' data. The data are first divided in sentences, than I have
made the bigram, trigram and 4-gram data.
Every words you insert, the algorithm choose the most probabilities next word. You can find more information 
<a href=http://rpubs.com/massyfigini/NextWordApp>here</a>.
<br/><br/>&copy;massyfigini")))
      
    ,width=4
    ),
    
    mainPanel(
      HTML("<font size=5 color=white>Top probability next word</font><br/>"),
      htmlOutput("word"),
      HTML("<br/><br/>"),
      HTML("<font size=5 color=white>Other possibly words</font><br/>"),
      htmlOutput("others")
      )
  )
))
      