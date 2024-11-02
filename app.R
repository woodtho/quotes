library(shiny)
library(httr)
library(jsonlite)
library(shinyWidgets)
library(shinyjs)
library(shinyanimate)

httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))

# Function to fetch available authors from the API
fetch_authors <- function() {
  base_url <- "http://api.quotable.io/authors?sortBy=quoteCount&order=desc&limit=150"
  response <- GET(base_url)
  if (status_code(response) == 200) {
    data <- content(response, "parsed", simplifyVector = TRUE)
    authors <- data$results$name
    return(authors)
  } else {
    return(NULL)
  }
}

# Function to fetch available tags from the API
fetch_tags <- function() {
  base_url <- "http://api.quotable.io/tags"
  response <- GET(base_url)
  if (status_code(response) == 200) {
    data <- content(response, "parsed", simplifyVector = TRUE)
    tags <- unique(data[data$quoteCount >= 10, ]$name)
    return(tags)
  } else {
    return(NULL)
  }
}

ui <- fluidPage(
  useShinyjs(),
  withAnim(),  # Include shinyanimate dependencies
  # Include custom CSS and fonts
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=IM+Fell+English&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body {
        background-color: #f0e4d7; /* Base parchment color */
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center center;
        font-family: 'IM Fell English', serif;
        color: #3e2723; /* Dark brown color for text */
      }
      .book {
        display: flex;
        justify-content: center;
        align-items: center;
        perspective: 1000px;
        position: relative;
        width: 100%;
        max-width: 800px;
        margin: 0 auto;
      }
      .book-page {
        width: 50%;
        height: 80vh;
        padding: 30px;
        font-size: 22px;
        line-height: 1.8;
        box-sizing: border-box;
        position: relative;
        overflow: hidden;

        /* Flex layout for centering */
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .left-page {
        background-image: url('page-left.png');
        background-size: cover;
        border-right: none;
        z-index: 1;
        transform-origin: right;
        backface-visibility: hidden;
      }
      .right-page {
        background-image: url('page-right.png');
        background-size: cover;
        border-left: none;
        z-index: 2;
        transform-origin: left;
        backface-visibility: hidden;
      }
      .page-content {
        text-align: center;

        /* Flex layout for centering content */
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }
      .quote-text, .author-text {
        word-wrap: break-word;
        max-height: 100%;
        max-width: 100%;
        overflow: hidden;
      }
      .quote-text {
        font-style: italic;
      }
      .author-text {
        margin-top: 20px;
        font-weight: bold;
      }
      .title-panel {
        text-align: center;
        font-size: 48px;
        margin-bottom: 20px;
        text-shadow: 2px 2px 5px #aaa;
      }
      .btn-custom {
        font-size: 18px;
        padding: 10px 30px;
        background-color: #8B4513; /* SaddleBrown color */
        color: white;
        border: none;
        border-radius: 5px;
        margin-top: 20px;
      }
      .btn-custom:hover {
        background-color: #A0522D; /* Sienna color */
      }
      .page-number {
        position: absolute;
        bottom: 10px;
        font-size: 16px;
        font-weight: bold;
      }
      .page-number.left {
        left: 10px;
        padding-bottom: 50px;
        padding-left: 50px;

      }
      .page-number.right {
        right: 10px;
        padding-bottom: 50px;
        padding-right: 50px;

      }
    ")),
    tags$script(HTML("
      function adjustFontSize(elementId) {
        var element = document.getElementById(elementId);
        if (!element) return;
        var parentHeight = element.parentElement.clientHeight - 60; // Adjust for padding/margins
        var parentWidth = element.parentElement.clientWidth - 60;
        var fontSize = 100; // Start with 100%
        element.style.fontSize = fontSize + '%';
        while ((element.scrollHeight > parentHeight || element.scrollWidth > parentWidth) && fontSize > 10) {
          fontSize -= 1;
          element.style.fontSize = fontSize + '%';
        }
      }

      Shiny.addCustomMessageHandler('adjustFontSize', function(message) {
        adjustFontSize(message.elementId);
      });
    "))
  ),
  # Title panel
  div(class = "title-panel", "Wisdom of the Ancients"),
  # Main content
  fluidRow(
    column(
      width = 4,
      # Sidebar for filters
      div(
        style = "background-color: rgba(255, 248, 220, 0.9); padding: 20px; border-radius: 10px; box-shadow: 3px 3px 10px rgba(0,0,0,0.2);",
        pickerInput(
          "author", "Author:", choices = c("All", fetch_authors()),
          options = list(`live-search` = TRUE)
        ),
        pickerInput(
          "tags", "Tags:", choices = c("All", fetch_tags()),
          options = list(`live-search` = TRUE)
        ),
        actionButton("new_quote", "Turn Page", class = "btn-custom"),
        actionButton("prev_quote", "Previous Page", class = "btn-custom")
      )
    ),
    column(
      width = 8,
      # Book display
      div(
        class = "book",
        # Left page with page number
        div(
          id = "left_page",
          class = "book-page left-page",
          div(
            id = "page_content_left",
            class = "page-content",
            span(textOutput("quote_left"), class = "quote-text"),
            span(textOutput("author_left"), class = "author-text")
          ),
          span(textOutput("page_number_left"), class = "page-number left")
        ),
        # Right page with page number
        div(
          id = "right_page",
          class = "book-page right-page",
          div(
            id = "page_content_right",
            class = "page-content",
            span(textOutput("quote_right"), class = "quote-text"),
            span(textOutput("author_right"), class = "author-text")
          ),
          span(textOutput("page_number_right"), class = "page-number right")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Function to fetch a random quote based on user inputs
  fetch_quote <- function(author, tags) {
    base_url <- "http://api.quotable.io/random"
    params <- list()
    if (author != "All") {
      params$author <- author
    }
    if (tags != "All") {
      params$tags <- tags
    }
    response <- GET(base_url, query = params)
    if (status_code(response) == 200) {
      content(response, "parsed", simplifyVector = TRUE)
    } else {
      NULL
    }
  }
  
  # Initialize with two quotes
  quote_history <- reactiveValues(
    quotes = list(),
    current_index = 2
  )
  
  # Fetch initial two quotes
  observeEvent(TRUE, {
    if (length(quote_history$quotes) < 2) {
      new_quotes <- list()
      for (i in 1:2) {
        new_quote <- fetch_quote("All", "All")
        if (!is.null(new_quote)) {
          new_quotes <- append(new_quotes, list(new_quote))
        }
      }
      quote_history$quotes <- append(quote_history$quotes, new_quotes)
    }
  }, once = TRUE)
  
  # Update the quotes when the "Turn Page" button is clicked
  observeEvent(input$new_quote, {
    new_quotes <- list()
    for (i in 1:2) {
      new_quote <- fetch_quote(input$author, input$tags)
      if (is.null(new_quote)) {
        showNotification("No quote found. Please adjust your filters.", type = "warning")
      } else {
        new_quotes <- append(new_quotes, list(new_quote))
      }
    }
    # Add to history
    quote_history$quotes <- append(quote_history$quotes, new_quotes)
    quote_history$current_index <- length(quote_history$quotes)
    
    # Delay updating content until after animation
    delay(10, {
      output$quote_left <- renderText({
        idx <- quote_history$current_index - 1
        if (idx >= 1 && idx <= length(quote_history$quotes)) {
          data <- quote_history$quotes[[idx]]
          paste0("\"", data$content, "\"")
        } else {
          ""
        }
      })
      
      output$author_left <- renderText({
        idx <- quote_history$current_index - 1
        if (idx >= 1 && idx <= length(quote_history$quotes)) {
          data <- quote_history$quotes[[idx]]
          paste0("- ", data$author)
        } else {
          ""
        }
      })
      
      output$quote_right <- renderText({
        idx <- quote_history$current_index
        if (idx >= 1 && idx <= length(quote_history$quotes)) {
          data <- quote_history$quotes[[idx]]
          paste0("\"", data$content, "\"")
        } else {
          ""
        }
      })
      
      output$author_right <- renderText({
        idx <- quote_history$current_index
        if (idx >= 1 && idx <= length(quote_history$quotes)) {
          data <- quote_history$quotes[[idx]]
          paste0("- ", data$author)
        } else {
          ""
        }
      })
      
      output$page_number_left <- renderText({
        idx <- quote_history$current_index - 1
        if (idx >= 1) {
          idx
        } else {
          ""
        }
      })
      
      output$page_number_right <- renderText({
        idx <- quote_history$current_index
        if (idx >= 1) {
          idx
        } else {
          ""
        }
      })
      
      # Adjust font size
      session$sendCustomMessage('adjustFontSize', list(elementId = 'page_content_left'))
      session$sendCustomMessage('adjustFontSize', list(elementId = 'page_content_right'))
    })
  })
  
  # Update the quotes when the "Previous Page" button is clicked
  observeEvent(input$prev_quote, {
    if (quote_history$current_index > 2) {
      quote_history$current_index <- quote_history$current_index - 2
    } else {
      quote_history$current_index <- 2
    }
    
    # Delay updating content until after animation
    delay(10, {
      output$quote_left <- renderText({
        idx <- quote_history$current_index - 1
        if (idx >= 1 && idx <= length(quote_history$quotes)) {
          data <- quote_history$quotes[[idx]]
          paste0("\"", data$content, "\"")
        } else {
          ""
        }
      })
      
      output$author_left <- renderText({
        idx <- quote_history$current_index - 1
        if (idx >= 1 && idx <= length(quote_history$quotes)) {
          data <- quote_history$quotes[[idx]]
          paste0("- ", data$author)
        } else {
          ""
        }
      })
      
      output$quote_right <- renderText({
        idx <- quote_history$current_index
        if (idx >= 1 && idx <= length(quote_history$quotes)) {
          data <- quote_history$quotes[[idx]]
          paste0("\"", data$content, "\"")
        } else {
          ""
        }
      })
      
      output$author_right <- renderText({
        idx <- quote_history$current_index
        if (idx >= 1 && idx <= length(quote_history$quotes)) {
          data <- quote_history$quotes[[idx]]
          paste0("- ", data$author)
        } else {
          ""
        }
      })
      
      output$page_number_left <- renderText({
        idx <- quote_history$current_index - 1
        if (idx >= 1) {
          idx
        } else {
          ""
        }
      })
      
      output$page_number_right <- renderText({
        idx <- quote_history$current_index
        if (idx >= 1) {
          idx
        } else {
          ""
        }
      })
      
      # Adjust font size
      session$sendCustomMessage('adjustFontSize', list(elementId = 'page_content_left'))
      session$sendCustomMessage('adjustFontSize', list(elementId = 'page_content_right'))
    })
  })
  
  # Initial rendering of quotes and page numbers
  output$quote_left <- renderText({
    idx <- quote_history$current_index - 1
    if (idx >= 1 && idx <= length(quote_history$quotes)) {
      data <- quote_history$quotes[[idx]]
      paste0("\"", data$content, "\"")
    } else {
      ""
    }
  })
  
  output$author_left <- renderText({
    idx <- quote_history$current_index - 1
    if (idx >= 1 && idx <= length(quote_history$quotes)) {
      data <- quote_history$quotes[[idx]]
      paste0("- ", data$author)
    } else {
      ""
    }
  })
  
  output$quote_right <- renderText({
    idx <- quote_history$current_index
    if (idx >= 1 && idx <= length(quote_history$quotes)) {
      data <- quote_history$quotes[[idx]]
      paste0("\"", data$content, "\"")
    } else {
      ""
    }
  })
  
  output$author_right <- renderText({
    idx <- quote_history$current_index
    if (idx >= 1 && idx <= length(quote_history$quotes)) {
      data <- quote_history$quotes[[idx]]
      paste0("- ", data$author)
    } else {
      ""
    }
  })
  
  output$page_number_left <- renderText({
    idx <- quote_history$current_index - 1
    if (idx >= 1) {
      idx
    } else {
      ""
    }
  })
  
  output$page_number_right <- renderText({
    idx <- quote_history$current_index
    if (idx >= 1) {
      idx
    } else {
      ""
    }
  })
  
  # Adjust font size on initial load
  observe({
    session$sendCustomMessage('adjustFontSize', list(elementId = 'page_content_left'))
    session$sendCustomMessage('adjustFontSize', list(elementId = 'page_content_right'))
  })
}

shinyApp(ui = ui, server = server)
