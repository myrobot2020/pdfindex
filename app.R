library(shiny)
library(pdftools)

ui <- fluidPage(
  titlePanel("PDF Word Search"),
  sidebarLayout(
    sidebarPanel(
      fileInput("pdf_file", "Upload PDF file", accept = ".pdf", multiple = FALSE),
      textInput("search_word", "Enter word to search:", value = ""),
      numericInput("subtract_value", "Number to subtract from page numbers:", value = 12, min = 0),
      numericInput("upper_limit", "Upper limit for page numbers:", value = 189, min = 1),
      actionButton("search_button", "Search")
    ),
    mainPanel(
      # Adding custom CSS style for adjusting the maximum width
      tags$head(
        tags$style(
          HTML("#result {
                 max-width: 200px; /* Adjust the width as per your requirement */
                 overflow-wrap: break-word; /* To allow long words to wrap */
                 }")
        )
      ),
      textOutput("result")
    )
  )
)


server <- function(input, output) {
  observeEvent(input$search_button, {
    if (is.null(input$pdf_file)) {
      output$result <- renderPrint("Please upload a PDF file.")
      return()
    }
    
    withProgress(
      message = 'Searching...',
      value = 0,
      {
        pdf_text <- pdf_text(input$pdf_file$datapath)
        pdf_text <- tolower(pdf_text)
        
        page_numbers <- c()
        
        for (i in 1:length(pdf_text)) {
          if (grepl(input$search_word, pdf_text[i])) {
            page_numbers <- c(page_numbers, i)
          }
          
          incProgress(1 / length(pdf_text))
        }
        
        # Subtract user-provided value from page numbers
        page_numbers <- page_numbers - input$subtract_value
        page_numbers <- page_numbers[page_numbers >= 1 & page_numbers <= input$upper_limit]
        
        output$result <- renderPrint({
          if (length(page_numbers) > 0) {
            paste(tools::toTitleCase(input$search_word), paste(page_numbers, collapse = ", "), sep = ", ")
          } else {
            "The word was not found in the PDF."
          }
        })
      }
    )
  })
}

options(shiny.maxRequestSize = 25*1024^2)

shinyApp(ui = ui, server = server)
