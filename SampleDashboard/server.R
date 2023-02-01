shinyServer(
  function(input, output, session){
    output$myplot <- renderPlot({
      disType <- input$Distribution
      size <- input$SampleSize
      if(disType == "Normal"){
        randonVec <- rnorm(size, mean = as.numeric(input$Mean), sd = as.numeric(input$StandardDeviation))
      }else{
        randonVec <- rexp(size, rate = 1/as.numeric(input$lamda))
      }
      hist(randonVec, col="blue")
    })
  }
)