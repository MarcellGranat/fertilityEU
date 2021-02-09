library(shiny)
library(glue)
library(colourpicker)
library(tidyverse)


dat <- rbind(read_csv("demo_r_frate2_1_Data.csv", locale = readr::locale(encoding = "UTF-8"))%>% 
               transmute(time = TIME, geo = GEO, variable = "Fertility", value = Value),
             read_csv("edat_lfse_16_1_Data.csv", locale = readr::locale(encoding = "UTF-8")) %>% 
               filter(SEX == "Total") %>% 
               transmute(time = TIME, geo = GEO, 
                         variable = "Early leavers from education and training (%)", value = Value)) %>% 
  filter(value != ":") %>% 
  mutate(
    value = as.numeric(value)
  )

eu_map <- eurostat::get_eurostat_geospatial(nuts_level = 2)



ui <- fluidPage(
  titlePanel("Appendix to our study"),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose your map!"),
      
      selectInput("variable", "Select variable!", selected = "Fertility",
                  c("Fertility", "Early leavers from education and training (%)")),
      sliderInput("time", "Year:", 2018, min = 1990, max = 2018, sep = "")
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    
    used_dat <- dat %>% 
      filter(variable == input$variable)
    
    used_dat %>% 
      filter(time == input$time) %>%
      {merge(eu_map, ., all.x = T)} %>% 
      ggplot(aes(fill = as.numeric(value))) +
      geom_sf(color = "black", size = .1) +
      scale_fill_viridis_c(option = "magma", limits = c(min(used_dat$value), max(used_dat$value)),
                           guide = guide_colorbar(frame.colour = "black", 
                                                  ticks.colour = "black"), 
                           na.value = "white") +
      theme_minimal() +
      theme(
        text  = element_text(family = "Impact"),
        axis.text = element_blank()
      ) +
      xlim(c(-30, 44)) +
      ylim(c(35, 75)) +
      labs(
        fill = input$variable
      )
  })
  
}

shinyApp(ui, server)