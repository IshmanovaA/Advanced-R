library(shiny)
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)

prihlasky_2020 <- read_csv("C:/R/lec/R/prihlasky-2020-21.csv")
prihlasky_2021 <- read_csv("C:/R/lec/R/prihlasky-2021-22.csv")
prihlasky_2022 <- read_csv("C:/R/lec/R/prihlasky-2022-23.csv")

zajemci_2020 <- read_csv("C:/R/lec/R/zajemci-2020-21.csv")
zajemci_2021 <- read_csv("C:/R/lec/R/zajemci-2021-22.csv")
zajemci_2022 <- read_csv("C:/R/lec/R/zajemci-2022-23.csv")
zajemci_2023 <- read_csv("C:/R/lec/R/zajemci-2023-24.csv")

prihlasky <- rbind(prihlasky_2020,prihlasky_2021,prihlasky_2022)
zajemci <- rbind(zajemci_2020,zajemci_2021,zajemci_2022, zajemci_2023)

merged_data <-
  full_join(prihlasky, zajemci, by = c('Reg. č.' = "Spec. symbol", "Program zkratka")) %>%
  subset(., is.na(`Datum podání`) == F) %>%
  mutate(date = `Datum podání` %>% gsub(" ", '', .) %>% parse_datetime(., format = "%d.%m.%Y")) %>%
  mutate(year = year(date)) %>%
  janitor::clean_names()


ui <- fluidPage(

    titlePanel("Přihlášky studentů"),

    sidebarLayout(
        sidebarPanel(
            selectInput("course_input",
                        "Prosím, vyberte kurz:",
                        choices = unique(merged_data$program_zkratka)),
            selectInput("year_input",
                        "Prosím, vyberte rok:",
                        choices = unique(merged_data$year)),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("basic_stats"),
           dataTableOutput("applications_by_time")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$basic_stats <- renderPrint({
      merged_data %>%
        dplyr::filter(year == input$year_input & program_zkratka == input$course_input) %>%
        summarize(
          How_many_students_applied = n(),
          How_many_students_accepted = sum(rozh_o_prijeti_nazev == "10 - Přijat", na.rm = T),
          How_many_students_enrolled = sum(zaplaceno == "ano" & rozh_o_prijeti_nazev == "10 - Přijat", na.rm = T)) %>%
        gather(key = "type", value = "quantity")
    })

    output$applications_by_time <- renderDataTable({
      datatable(merged_data %>%
        dplyr::filter(year == input$year_input & program_zkratka == input$course_input) %>%
        count(date, sort = T) %>%
        mutate(date = date %>% as.character())
      )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
