library(shiny)
library(tidyverse)
library(DT)
library(fs)
library(stringr)

dir <- "C:/Users/pebenbow/Documents/repos/ds/dat153/dat153-202402/"

  # Function to get files info recursively
  get_files_info <- function(path) {
    files <- fs::dir_ls(path, recurse = TRUE, type = "file", glob = "*.qmd")
    data <- data.frame(
      FilePath = files,
      FileName = str_replace(files, dir, ""),
      Size = fs::file_info(files)$size,
      ModificationTime = fs::file_info(files)$modification_time,
      FileType = tools::file_ext(files),
      Copy = '<button class="copyButton" style="color: black;">Copy</button>',
      stringsAsFactors = FALSE
    )
    return(data)
  }
  
  # Retrieve files info once
  file_data <- get_files_info(dir) %>%
    select(
      FileName,
      FilePath,
      ModificationTime,
      FileType,
      Copy
    )
  
  ui <- fluidPage(
    titlePanel("Recursive File Browser with Copy Functionality"),
    fluidRow(
      column(12,
             DTOutput("fileTable")
      )
    ),
    tags$script(
      HTML("
      Shiny.addCustomMessageHandler('copyFile', function(message) {
        var button = $(message.selector);
        button.css('color', 'green');
        button.text('Copied');
      });
    ")
    )
  )
  
  server <- function(input, output, session) {
    
    output$fileTable <- renderDT({
      datatable(file_data, escape = FALSE, options = list(pageLength = 5), callback = JS(
        "table.on('click', 'button.copyButton', function() {
        var row = $(this).closest('tr');
        var data = table.row(row).data();
        Shiny.setInputValue('copy', {path: data[2], target: $('#target_dir').val(), selector: this});
      });"
      ))
    }, server = FALSE)
    
    observeEvent(input$copy, {
      req(input$copy)
      try({
        file_copy(input$copy$path, "C:/Users/pebenbow/Desktop", overwrite = TRUE)
        #file.copy(input$copy$path, file.path(input$copy$target, basename(input$copy$path)))
        session$sendCustomMessage(type = 'copyFile', message = list(selector = as.character(input$copy$selector)))
      })
    })
  }
  
  shinyApp(ui = ui, server = server)