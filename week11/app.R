#shiny web app for homework week 11
#created by: diana gao
#created on: 2024-11-18

#load libraries
library(shiny)
library(tidyverse)
library(janitor)

# clean the data real quick, using intertidal data -----------
mussels <- read_csv("data/intertidaldata.csv")
clean <- mussels %>%
  clean_names() %>%
  select(site, quadrat, mussels) %>%
  mutate(quadrat = str_replace_all(quadrat, "\\.", " ")) %>%
  mutate(quadrat = str_replace_all(quadrat, "1", " ")) %>%
  mutate(quadrat = str_trim(quadrat)) %>%
  group_by(site, quadrat) %>%
  summarise(mussels = sum(mussels))

# Define UI -----------
ui <- fluidPage(
  textOutput("title"),
  selectInput("quadrat", label = "Quadarat type", choices = clean$quadrat), # make drop down menu to select what type of quadrat
  plotOutput("plot", width = "1000px") # final plot
)

# Define server ------------
server <- function(input, output, session) {
    output$title <- renderText("Mussels at each site based on quadrat type") # just a title 
    filtered <- reactive({
      filt <- clean %>%
      filter(quadrat == input$quadrat)
      #print(filt)
      #filt # this was is just for me to see if this part worked lol
    })
    output$plot <- renderPlot(
      ggplot(filtered(), aes(x = site, y = mussels)) + 
        geom_col() + # plot the mussels per site on a histogram, filtered by the selection
      labs(title = "Mussels per Site", 
           subtitle = paste0("Quadrat type: ", input$quadrat),
           x = "Site Name",
           y = "Number of Mussels")
      )
}


# Run the application ----------
shinyApp(ui = ui, server = server)
