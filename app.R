# purpose: build random workouts from list of exercises
# load libraries ----
library(shiny)
library(rsconnect)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(gt)
# library(downloadthis)
library(webshot)
library(writexl)
# library(shinythemes)
library(bslib)
library(shinyjs)
library(httr)

phantomjs_path <- webshot:::find_phantom()
if (is.null(phantomjs_path)){
  webshot::install_phantomjs()
  FlgJS <- F
} else{
  FlgJS <- T
}0
# get data ----
gdocs_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQliF1fPTXNk6b4cVwbkD7GmYFmyNKkG2GrzYcr21d-C02L61gZZNrk3beBEf95mQ-doWd3MweAyZKH/pub?gid=0&single=true&output=csv"
#read data from url
exercise_data <- read_csv(gdocs_url)

#set dynamic themes
light <- bs_theme(version = 4, bootswatch = "minty")

# define UI ----
ui <- fluidPage(
  # theme = shinytheme("lumen"), 
  theme = light,
  h4("a simple workout plan generator"),
  titlePanel("Mix Things Up"),
                # Layout a sidebar and main area
                sidebarLayout(
                  #side bar here ----
                  sidebarPanel(
                    useShinyjs(),
                    
                    #define input sliders ----
                    sliderInput("n_exercises", "Step 1: Number of Exercises", value=5, min=1, max=10, 1),
                    sliderInput("n_sets", "Step 2: Number of Sets", value = 1, min=1, max=5, 1),
                    sliderInput("n_reps", "Step 3a: Number of Reps", value = 5, min=5, max=20, 5),
                    sliderInput("n_time", "Step 3b: Number of Minutes", valu = 2, min=1, max=10, 1),
                    #action button to run setup ----
                    actionButton("goButton", "Get after it!", btn_type = "button", class = "btn-secondnary"),
                    ),
                  # main area here ----
                  mainPanel(
                    br(),
                    gt_output(outputId = "gt_table"),
                    br(),
                    column(12, downloadButton(outputId = "downloadXLSX", label = "Download XLSX"), align='center'),
                    br(),
                    column(12, downloadButton(outputId = "downloadPNG", label = "Download PNG"), align='center')
                  )
                )
)

server <- function(input,output, session) {
  
  vals <- reactiveValues()
  
  # define set_or_sets variable
  set_or_sets <- reactive({ 
    if(input$n_sets > 1){"sets"}
    else {"set"}
  })
  
  observe({
    hide("downloadXLSX")
    if(input$goButton)
      show("downloadXLSX")
  })
  
  observe({
    hide("downloadPNG")
    if(input$goButton)
      show("downloadPNG")
  })
  
  data_extr <- eventReactive(input$goButton, { 
    exercise_data %>% 
    sample_n(input$n_exercises) %>%
    mutate(`#` = row_number(), 
           `Sets/Reps/Time` = 
             case_when(
               `Rep Or Time` == "Rep" ~ str_c(input$n_sets, set_or_sets(), "of", input$n_reps, "reps", sep=" "),
               `Rep Or Time` == "Time" ~ str_c(input$n_sets, set_or_sets(), "of", input$n_time, "min", sep=" ")
             )) %>%
    select(`#`, Exercise, `Sets/Reps/Time`)
  })
  
  #show gt table ----
  
  gt_table <- function()({
    vals$gg <- data_extr() %>% 
    gt() %>%
      tab_header(
        title = md("Simple workout plan"),
        subtitle = md("what are you waiting for?")
      ) %>%
      tab_source_note(
        source_note = md('Click `Get  after it!` if changing <br>parameters. Made by <b><a href="https://github.com/parmsam">@parmsam</a></b>.')
      ) %>%
      tab_source_note(
        source_note = md("Consult health professional before <br>starting a fitness program. Use the free <br>weight you're comfortable with.")
      )
    vals$gg
  })
  
  output$gt_table <-
    render_gt(
      expr = gt_table()
    )
  
  output$downloadXLSX <- downloadHandler(
    filename = function() {
      str_c('rand_workout_plan_', Sys.Date(), '.xlsx', sep='')
      },
    content = function(con) {
      write_xlsx(data_extr(), con)
      }
    )
  
  output$downloadPNG <- downloadHandler(
    filename = function() { 
      str_c('rand_workout_plan_', Sys.Date(), '.png', sep='')
      },
    contentType = "image/png",
    content = function(con) {
      gtsave(vals$gg, expand = 50, filename =  con, path = NULL)
      # png(con)
      # print(vals$gg)
      # dev.off()
    }
  )
  
}

shinyApp(ui, server)

