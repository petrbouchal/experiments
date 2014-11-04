shinyUI(pageWithSidebar(
  headerPanel('Download Example'),
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars")),
    downloadLink('downloadData', 'Download')
  ),
  mainPanel(
    tableOutput('table')
  )
))