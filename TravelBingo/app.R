#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggimage)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Travel Bingo Generator - Keep the kids entertained"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("seed_slide", "Randomize", min = 1, max = 1000, value = 500),
            submitButton("Refresh")#,
            # downloadButton("dl", "Like it? Save as pdf")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot(width = 800, height = 800, {
        set.seed(input$seed_slide)
        bingo_items <- read_csv("DATA/bingo_items.csv") %>%
            mutate(id = sample(nrow(.)), # randomly assign row numbers 
                   x = ceiling(id / 5), # assign to one of 5 columns
                   y = id %% 5, #assign to one of 5 rows
                   image_url = if_else(id == 12, NA_character_, image_url), # middle square should have no image
                   free = if_else(id == 12, "FREE", NA_character_)) %>% # middle square should say FREE
            filter(id <= 25) # keep only 25 of the items

        ggplot(bingo_items, aes(x = x, y = y)) +
            geom_text(aes(label = free)) +
            geom_image(aes(image = image_url), size = 0.15) +
            scale_x_continuous(limits = c(0.5, 5.5)) +
            scale_y_continuous(limits = c(-0.8, 4.8)) +
            geom_vline(aes(xintercept = 4.5)) + 
            geom_vline(aes(xintercept = 1.5)) +
            geom_vline(aes(xintercept = 2.5)) +
            geom_vline(aes(xintercept = 3.5)) +
            geom_hline(aes(yintercept = 0.5)) + 
            geom_hline(aes(yintercept = 1.5)) +
            geom_hline(aes(yintercept = 2.5)) +
            geom_hline(aes(yintercept = 3.5)) +
            theme_void() +
            theme(panel.border = element_rect(colour = "black", fill=NA, size=3)) +
            ggtitle("Travel Bingo\n")
    })
    
    # output$dl <- downloadHandler(
    #     filename = ("bingo_card.pdf"),
    #     content = function(file) {
    #         device <- function(..., width, height) {
    #             pdf(..., width = 8, height = 8, res = 300, units = "in")
    #         }
    #         ggsave(file, plot = plotInput(), device = device)
    #     })
}

# Run the application 
shinyApp(ui = ui, server = server)