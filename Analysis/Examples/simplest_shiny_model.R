library(shiny)

#Podsumowując, kod ten tworzy prosty interfejs użytkownika aplikacji Shiny, 
#który składa się z tytułu na górze, paska bocznego z suwakiem do wyboru 
#wartości oraz głównego panelu, który wyświetla wybraną wartość suwaka.
# Najlepiej odpalić i zobaczyć

# Define User Interface
ui <- fluidPage(  # <- do tworzenia responsywnej strony, która automatycznie dostosowuje się do rozmiaru okna przeglądarki
  
  
  titlePanel("Simple Shiny App"), # <- Dodaje panel tytułowy na górze stron
  sidebarLayout(  # <- Dzieli stronę na dwie główne sekcje: paski boczny i główny panel.
    
    sidebarPanel(   # <- Definiuje panel boczny
      sliderInput("slider", "Select a value:", min = 1, max = 100, value = 50) # <- zawatość suwaka
    ),
    
    mainPanel(  # <- Definiuje panel główny
      textOutput("value")
      
      
    )
  )
)



# Define server logic
server <- function(input, output) {  
  # <- Tworzymy funkcję serwera 
  # (input wartości wprowadzone przez użytkownika w interfejsie (np. wartość suwaka)
  #  input zawiera wartości wprowadzone przez użytkownika w interfejsie (np. wartość suwaka)
  output$value <- renderText({ # generowania tekstu + wart z inputu trafia do output$value
    paste("Selected value is", input$slider)
  })
}

# Run the application
shinyApp(ui = ui, server = server) # <-  uruchamia aplikację Shiny, łącząc interfejs użytkownika (ui) 
# z logiką serwera (server) i wyświetlając aplikację w przeglądarce
