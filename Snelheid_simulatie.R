# Test om te laten zien wat de snelheid is van een Shiny dashboard 
# Drie opties
# Lokaal via  C:
# Netwerk via T: hogerop
# Netwerk via T: lager 

# Gebruik je dit script voor de eerste keer; installeer dan de benodigde packages (# weghalen hieronder)
#install.packages("openxlsx","dplyr","shiny","ggplot","shinyFiles")

# Laad de benodigde pakketten
library(shiny)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(shinyFiles)

# Keuze maken lokaal wegschrijven (C) of op de server (T)
# Haal de # weg en zet een # waar nodig om te switchen
#output_dir <- paste0('C:/Users/', Sys.getenv('username'), '/Documents/')
#output_dir <- "T:/K en I/Primaire processen/Gezondheidsatlas Zuid-Limburg/15. Nieuwe atlas 2023/Swing/Data/Output/Test"
output_dir <- "T:/K en I/Primaire processen/R_kubus_output"

# Maak map aan met de naam Test, als deze nog niet aanwezig is.
test_dir <- "Test"
ifelse(!dir.exists(file.path(output_dir, test_dir)), dir.create(file.path(output_dir, test_dir)), FALSE)

# Functie om een neppe dataset te genereren
generate_data <- function(rows) {
  data.frame(
    ID = 1:rows,
    Name = sample(c("John Doe", "Jane Doe", "Chuck Norris", "Elon Musk", "Ada Lovelace"), rows, replace = TRUE),
    Joke = sample(c("Why don't scientists trust atoms? Because they make up everything!", 
                    "I told my computer I needed a break, and now it won't stop sending me Kit-Kats.", 
                    "Why do programmers prefer dark mode? Because light attracts bugs!"), rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# UI
ui <- fluidPage(
  titlePanel("Excel File Write Time Tester"),
  sidebarLayout(
    sidebarPanel(
      numericInput("rows", "Aantal rijen:", 1000, min = 1),
      numericInput("files", "Aantal bestanden:", 10, min = 1),
      shinyDirButton("directory", "Selecteer map", "Selecteer de map om bestanden op te slaan"),
      textOutput("selectedDir"),
      actionButton("generate", "Schrijf bestanden")
    ),
    mainPanel(
      plotOutput("timePlot"),
      textOutput("averageTime")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Map selectie
  shinyDirChoose(input, "directory", roots = c(home = output_dir), session = session)
  
  # Toon de geselecteerde map
  output$selectedDir <- renderText({
    dir_path <- parseDirPath(roots = c(home = output_dir), input$directory)
    paste("Geselecteerde map:", dir_path)
  })
  
  observeEvent(input$generate, {
    # Genereer dataset
    data <- generate_data(input$rows)
    
    # Verkrijg het pad van de geselecteerde map
    dir_path <- parseDirPath(roots = c(home = output_dir), input$directory)
    
    # Functie om een Excel-bestand weg te schrijven en de tijd te meten
    write_excel_with_time <- function(data, file_name) {
      start_time <- Sys.time()
      write.xlsx(data, file = file_name)
      end_time <- Sys.time()
      time_taken <- end_time - start_time
      return(time_taken)
    }
    
    # Schrijf het opgegeven aantal Excel-bestanden weg en meet de tijd
    time_taken_list <- sapply(1:input$files, function(i) {
      file_name <- file.path(dir_path, paste0("data_", i, ".xlsx"))
      write_excel_with_time(data, file_name)
    })
    
    # Toon de tijd die nodig was voor elk bestand in een lijngrafiek
    output$timePlot <- renderPlot({
      time_data <- data.frame(Bestand = 1:input$files, Tijd = time_taken_list)
      ggplot(time_data, aes(x = Bestand, y = Tijd)) +
        geom_line(color = "blue") +
        geom_point(color = "red") +
        theme_minimal() +
        labs(title = "Tijd om Excel-bestanden weg te schrijven", x = "Bestand", y = "Tijd (seconden)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Gemiddelde tijd berekenen
    average_time <- mean(time_taken_list)
    output$averageTime <- renderText({
      paste("Gemiddelde tijd om een Excel-bestand weg te schrijven:", round(average_time, 3), "seconden")
    })
  })
}

# Run de app
shinyApp(ui = ui, server = server)
