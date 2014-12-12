library(shiny)
dataset <- df_f
shinyUI(pageWithSidebar(
  
  headerPanel("Attendance rate correlated with scores"),
  
  sidebarPanel(  
    selectInput('x', 'X', names(df_f)[8]),
    selectInput('y', 'Y', names(df_f)[c(7,9,10)]),    
    selectInput('facet_row', 'Facet Row', c(None='.',names(df_f)[c(16:37)])),
    selectInput('facet_col', 'Facet Column', c(None='.',names(df_f)[2]),names(df_f)[[2]])
  ),
  
  mainPanel(
    plotOutput('plot')
  )
))

  