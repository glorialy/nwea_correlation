library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  dataset <- reactive({
    df_f
  })
  
  output$plot <- renderPlot({
    correlation <- ddply(df_f, .(topic), function(x){cor(x[,input$x], x[,input$y])})
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()

    p <- p + geom_point(aes_string(color = 
                                     "topic"))
    p <- p + geom_smooth(aes_string(color = "topic"), method = 'lm')
    
    p <- p + scale_x_continuous(labels = percent)
    p <- p + scale_colour_brewer(type = 'div', palette = 'Set1')
    p <- p + theme(legend.position = 'none',
                   axis.title = element_text(face = 'bold'),
                   strip.text = element_text(face = 'bold'))
    
    v <- c(input$facet_row,"topic")
    x <- input$x
    y <- input$y
    facets <- paste(input$facet_row, '~', input$facet_col)
    if(facets == '. ~ .'){
      correlation$V1 <- cor(df_f[,x], df_f[,y])
    } else
    
    {p <- p + facet_grid(facets)
     if(input$facet_col == '.'){
       correlation <- ddply(df_f, v[1],.fun= function(z){cor(z[,x], z[,y])})
     }else if (facets != '. ~ topic'){
       correlation <- ddply(df_f, v,.fun= function(z){cor(z[,x], z[,y])})
     }
     print(correlation)
    }
    
    
    if(y=="score_max")
    {p <- p + geom_text(data = correlation,aes(x = 0.5, y= max(df_f[,"score_max"])*0.95,
                           label = paste0('corr: ',round(V1*100, digits = 1), '%')))} else {
     p <- p + geom_text(data = correlation,aes(x = 0.5, y= max(df_f[,"change_max"])*0.95,
                        label = paste0('corr: ',round(V1*100, digits = 1), '%'))) 
    }
    print(p)  
    
  }, height=700)
})

