# purpose: build random workouts from list of exercises
# load libraries ----
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(gt)
library(downloadthis)
library(webshot)
# library(shinythemes)
library(bslib)

# get data ----
gdocs_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQliF1fPTXNk6b4cVwbkD7GmYFmyNKkG2GrzYcr21d-C02L61gZZNrk3beBEf95mQ-doWd3MweAyZKH/pub?gid=0&single=true&output=csv"
#read data from url
exercise_data <- read_csv( url(gdocs_url) )

# define UI ----
ui <- fluidPage(
  # theme = shinytheme("lumen"),
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  h4("a simple workout generator"),
  titlePanel("Muscle Confusion"),
                # Layout a sidebar and main area
                sidebarLayout(
                  #side bar here ----
                  sidebarPanel(
                    
                    #define input sliders ----
                    sliderInput("n_exercises", "Step 1: Number of Exercises", value=5, min=1, max=10, 1),
                    sliderInput("n_sets", "Step 2: Number of Sets", 1, 5, 1),
                    sliderInput("n_reps", "Step 3: Number of Reps", 5, 20, 5),
                    sliderInput("n_time", "Step 4: Number of Minutes", 2, 10, 2),
                    #action button to run setup ----
                    actionButton("goButton", "Get after it!", btn_type = "button", class = "btn-warning"),
                    ),
                  # main area here ----
                  mainPanel(
                    gt_output(outputId = "gt_table")
                  )
                )
)

server <- function(input,output) {
  
  # define set_or_sets variable
  set_or_sets <- reactive({ 
    if(input$n_sets > 1){"sets"}
    else {"set"}
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
  
  gt_table <- reactive({
    data_extr() %>% 
    gt()
  })
  
  output$gt_table <-
    render_gt(
      expr = gt_table(),
      height = px(600),
      width = px(600)
    )
  
  # # provide download button functionality ----
  # gtsave(gt_table,
  #        "images", expand = 50,
  #        file =  "temp.png")
  # 
  # # download button for table to xlsx 
  # renderUI({
  #   data_extr %>%
  #     download_this(
  #       output_name = str_c("rand_workout_plan_", today()),
  #       output_extension = ".xlsx",
  #       button_label = "Download XLSX",
  #       button_type = "primary",
  #     )
  # })
  # 
  # # download button for table to png
  # renderUI({
  #   download_file(
  #     path = "images/temp.png",
  #     output_name = str_c("rand_workout_plan_", today()),
  #     output_extension = ".png",
  #     button_label = "Download PNG",
  #     button_type = "success",
  #     has_icon = TRUE,
  #     icon = "fa fa-save",
  #     self_contained = FALSE
  #   )
  # })
  
}

if (interactive()) {
  shinyApp(ui, server)
}

