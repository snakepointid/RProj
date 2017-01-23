library(shiny)
library(rpart)
library(rattle)
# Define server logic for slider examples

shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  output$Rules <- renderPrint({
    cp=0.1^input$cps
    maxdep=input$depth
    mins=input$minm
    imp=switch(input$imp,
               yes = T,
               no = F)
    if(imp)
      df<-train.df else
        df<-train.df.m
    
    useful<-useful[useful!=input$var1]
    useful<-useful[useful!=input$var2]
    tree<-rpart(def~.,df[,c(useful,'def')],na.action=na.rpart,
                control=rpart.control(minsplit = mins, maxdepth = maxdep, cp = cp, 
                                      maxcompete = 1, maxsurrogate = 0, usesurrogate = 0, xval =4,
                                      surrogatestyle = 0))
    asRules(tree)
  }) 
  output$cap <- renderText({
    cp=0.1^input$cps
    maxdep=input$depth
    mins=input$minm
    paste('[cp]:',cp,'    [max depth]:',maxdep,'       [minimun observations]:',mins)
  }) 
})