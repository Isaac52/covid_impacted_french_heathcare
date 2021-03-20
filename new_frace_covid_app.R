# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - LAYOUT -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(googlesheets4)
library(plotly)
library(tidyquant)
library(tidyverse)
library(keyring)
library(shinyjs)
library(esquisse)
library(DataExplorer)
library(tidyverse)
library(fs)
library(thematic)
library(bslib)
# Getting all file locations
source('france_covid_functions.R')
source('info_card.R')



# UI ----

#thematic::thematic_shiny(font = "auto")
ui <- navbarPage(
    
    title = HTML("COVID-19 Dashboard - <small>Data From data.gouv.fr </small>"),
    collapsible = TRUE,
    
    #shinytheme('superhero')
    theme = bslib::bs_theme(version = 3,bootswatch = 'superhero'),
    
    tabPanel(
        title = 'France Maps',
        
    # CSS ----
    
    #shinythemes::themeSelector(),
    
     
    # tags$head(
    #     tags$link(rel = 'stylesheet',type = 'text/css', href = 'www/styles.css')
    # ),
    
   
    
    
    # JS ----
    shinyjs::useShinyjs(),
    
    # 1.0 HEADER ----
    div(
        class = 'container-fluid',
        id = 'fr_header',
        
         column(width = 1,tags$img(class = "img-responsive",src = "circle_flag.png", style = "width: 100px; height:100px;"))
        ,h1(class = 'page-header',HTML('Anaylzing the Impacted of COVID-19 - <small>French heathcare system</small>'))
        
        
       
    ),
    
    
    
    
    
    
    
    
    
    # Value Boxes
    div(
        class = 'container-fluid hidden-sm hidden-xs',
        id = 'Favorite-container',
        
        
        
        div(
            class = 'container-fluid',
            id = 'cards',
            
            fluidRow(
            column(
                width = 3,
                
                div(
                    class = "panel panel-default",
                    style = 'padding:0px;',
                    div(
                        class = str_glue("panel-body bg-default text-default"),
                        p(class = "pull-right", icon(class = "fa-4x", 'fas fa-street-view')),
                        valueBoxOutput(width = NULL,outputId = 'box_dep'),
                        
                    ) 
                )
                
                
                
                
            ),
            
           
            column(
                
                width = 3,
                div(
                    class = "panel panel-default",
                    style = 'padding:0px;',
                    div(
                        class = str_glue("panel-body bg-default text-default"),
                        p(class = "pull-right", icon(class = "fa-4x", 'fas fa-users')),
                        valueBoxOutput(outputId = 'box_pop')
                        
                       ) 
                   )
                ),
            
            
            column(
                
                width = 3,
                div(
                    class = "panel panel-default",
                    style = 'padding:0px;',
                    div(
                        class = str_glue("panel-body bg-default text-default"),
                        p(class = "pull-right", icon(class = "fa-4x", "fas fa-procedures")),
                        valueBoxOutput(outputId = 'box_rea')
                        
                       ) 
                   )
                ),
            
            
           
            column(
               
                width = 3,
                div(
                    class = "panel panel-default",
                    style = 'padding:0px;',
                    div(
                        class = str_glue("panel-body bg-default text-default"),
                        p(class = "pull-right", icon(class = "fa-4x", "far fa-dizzy")),
                        valueBoxOutput(outputId = 'box_dc')
                        
                                     
                                
                            
                        )
                    )
                )
            )
        )
    
),
        
   
    
    
    
    #verbatimTextOutput(outputId = 'my_list'),
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 3.0 APPLICATION UI ----
    div(
        class = 'container-fluid',
        id = 'Favorite-container_2',
        
    div(class = 'container-fluid',
    fluidRow(
       
        
        
        column(
            width = 2,
            
            wellPanel(
                div(
                    id ="date_range",
                    dateRangeInput(
                        inputId   = "date_range",                   # input id key
                        label     = h4("Date Range"),               # h4 header
                        start     = min(memoised_all_gender$jour), # starting date is min in jour column
                        end       = max(memoised_all_gender$jour), # ending date is max in jour column
                        min       = min(memoised_all_gender$jour), # not allow lower selection of min date
                        max       = max(memoised_all_gender$jour), # not allow higher selection of max date
                        startview = "month"),
                    
                    br(),
                    # rest button with sync icon
                    actionButton(inputId = "reset",label = "Reset Date", icon = icon("sync"))
                ),
                br(),
                hr(),
                div(
                    id ='input_main',
                    pickerInput(
                    inputId = 'picker_dep',
                    label = "France Department List (Pick One to Analyze)",
                    choices = memoised_all_gender %>% mutate(name = str_to_title(name)) %>% distinct(name) %>% pull(),
                    multiple = FALSE,
                    selected = memoised_all_gender %>% pull(name) %>% pluck(1) ,
                    options = pickerOptions(
                        actionsBox = FALSE,
                        liveSearch = TRUE,
                        size = 15,
                    )
                    
                )
               
                ),
                div(
                    id = 'fr_input_buttons',
                    shiny::actionButton(inputId = 'analyze',label = 'Analyze',icon = shiny::icon('download'))
                    
            )
                
        )
    
    ),
    
    
    
    
    
    column(
        width = 5,
        
        div(
            class = 'panel panel-default',
            shinyWidgets::radioGroupButtons(
                  inputId   = "time_unit",                                 # creating a time_unit input id
                                                                         # creating label name for radio group buttons
                  choices   = c("Day"="day","Week"="week","Month"="month"),# Naming the radio group buttons
                  selected  = "week",                                       # Day will be selected when app is first opens
                  status    = "primary", 
                  justified = TRUE,                                        # Color of radio buttons (primary = Blue)
                  individual = TRUE,                                 # will make the radio buttons span the full width of the box
                  checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"),            # checkIcom puts a check mark if button is selected
                                                                    # Google glyphicon for more detail
                    no  = NULL
                  )
            ),
    
            div(
                class = 'panel-body'
                #,textOutput(outputId = 'line_plot_head')
                ,plotlyOutput(outputId = 'my_map_1')
                #,verbatimTextOutput(outputId = 'datrange')
                #,verbatimTextOutput(outputId = 'my_list')
                
            )
        )
    ),  
    
    
    
    
    
        
    
    column(
        width = 5,
        div(
            class = 'panel panel-default',
            shinyWidgets::radioGroupButtons(
                inputId   = "diff_covid_plots",
                choices  = c(
                     'Deaths'          = 'incid_dc'
                    ,'Recovered'       = 'incid_rad'
                    ,'ICU'             = 'incid_rea'
                    ,'Hospitaliztions' = 'incid_hosp'),
                selected  = 'incid_dc', 
                
                individual = TRUE,                                    # Day will be selected when app is first opens
                status    = "primary",                                                  # Color of radio buttons (primary = Blue)
                justified = TRUE,                                                        # will make the radio buttons span the full width of the box
                checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"),                                  # checkIcom puts a check mark if button is selected
                                                                                              # Google glyphicon for more detail
                        no  = NULL
                        )
                    
                    ),
                
                
                
        div(
             class = 'panel-body'
            ,leafletOutput(outputId = 'my_map_2')
            #textOutput(outputId = 'my_list')
                    
                    
                   
                
            )
        )
    )

# ,
# fluidRow(
#     column(
#         width = 3,
#         DTOutput(outputId = 'DT_1')
#     ),
# fluidRow(
#     column(
#         width = 3,
#         DTOutput(outputId = 'DT_2')
#     ),
# fluidRow(
#     column(
#         width = 3,
#         DTOutput(outputId = 'DT_3')
#     ),
# fluidRow(
#     column(
#         width = 3,
#         DTOutput(outputId = 'DT_4')
#            )
#         )
#      )
#    )
# )   
    
    
               )
            )
        )
    )
)

# SERVER ----
server <- function(input,output,session){
  
   
    # eventReactive
   data <- eventReactive(input$my_map_2,{
        memoised_all_gender
    },ignoreNULL = FALSE)
    
    data_1 <- eventReactive(input$analyze,{
        memoised_all_gender
    },ignoreNULL = FALSE)
    
    
    line_plot_header <- eventReactive(input$analyze,{
        input$picker_dep
    }, ignoreNULL = FALSE)
    
     output$line_plot_head <- renderText({
        line_plot_header()
    })
     
     
     
     
     
    processed_data_filter_tbl <- eventReactive(
        eventExpr = input$analyze,
        
          valueExpr = {
            data_1() %>%
              mutate(name = name %>% str_to_title()) %>% 
              filter(jour %>% dplyr::between(left  = input$date_range[1],             
                                      right = input$date_range[2])) %>%
              filter(name %in% input$picker_dep)                           
            },ignoreNULL = FALSE                                         
        )
    #output$my_list <- renderPrint({data_1()})
    #output$datrange <- renderPrint({processed_data_filter_tbl()})
    







    time_plot_tbl <- reactive({
        time_unit <- input$time_unit 
        
        processed_data_filter_tbl() %>%
              mutate(jour = floor_date(jour, unit = time_unit)) %>% 
              group_by(name,jour) %>% 
              summarise(total_rea = sum(incid_rea)) %>% 
              mutate(label_text = str_glue("Date: {jour}
                                            ICU Patient Total: {total_rea}"))      

    })

    #output$my_list <- renderPrint({time_plot_tbl()})
   
    # rendering the plotly to flexdashboard
    output$my_map_1 <- renderPlotly({



    g <- time_plot_tbl() %>%  
        ggplot(aes(jour,total_rea))+
        geom_point(aes(text = label_text), color = "#2c3e50", size = 0.5)+
        geom_smooth(method = "loess",span = 0.2,size = 0.8,se = FALSE) +
        theme_light() +
        expand_limits(y = 0) +
    
        labs(
          x = 'Date',
          y = "Total ICU Count"
        )
    
    ggplotly(g, tooltip = "text") %>%
      layout(margin = list(b = 100))        # adjusts the margins of the plot b = bottom of plot and 100 is pixels
    })

  
    
    
    
    
    
    
    
    
   
   
   
     
   incid_var <- reactive({
       incid_varable <- input$diff_covid_plots
       
   })
    
    #output$my_list <- renderText({incid_var()})
   
    # Leaflet Plots rea
    output$my_map_2 <- renderLeaflet({

      leaflet_plot_func(data()
            ,cols = incid_var())


   })
        
   
   
   
    
    
   
   
# making the department drop down and date range reset when reset button is clicked
observeEvent(eventExpr = input$reset,handlerExpr = {

  

  updateDateRangeInput(
    session = session,                            # session = session not sure why
    inputId = "date_range",                       # calling date_range id from shinyWidgets::pickerInput
    start   = min(memoised_all_gender$jour),     # resets to min jour date
    end     = max(memoised_all_gender$jour)      # resets to max jour date
  )

  # updateRadioGroupButtons(
  #   session  = session,
  #   inputId  = "diff_covid_plots",
  #   selected = "Deaths"
  #   )


   shinyjs::delay(ms = 300,expr = {                # The delay function delays an action in milliseconds so updates can be
    shinyjs::click(id = "analyze")                  # preformed before the apply bottom is clicked

  })
   
  
   
   
   
   
   

})

   
   # output$DT_1 <- DT::renderDT({
   #     fr_covid_data_tables(data_1(),cases_or_deaths = 'dc')
   # })
   # output$DT_2 <- DT::renderDT({
   #     fr_covid_data_tables(data_1(),cases_or_deaths = 'rad')
   # })
   # output$DT_3 <- DT::renderDT({
   #     fr_covid_data_tables(data_1(),cases_or_deaths = 'rea')
   # })
   # output$DT_4 <- DT::renderDT({
   #     fr_covid_data_tables(data_1(),cases_or_deaths = 'hosp')
   # })
   # 
   # 



summary_values_tbl <- eventReactive(
  eventExpr = input$analyze,                                        # getting the inputID from the play action button

  valueExpr = {                                                   # inside the valueExpr is code that will populate the                                                                            # valueBoxes
    valuebox_tbl <- data_1()  %>%

      filter(jour %>%  dplyr::between(left = input$date_range[1],
                                      right = input$date_range[2])) %>%
        mutate(name = name %>% str_to_title()) %>% 
        filter(name %in% input$picker_dep) %>%             
        mutate(
         total_rea = sum(incid_rea)
        ,total_hosp = sum(incid_hosp)
        ,total_dc = sum(incid_dc)
        ,total_rad = sum(incid_rad)
        ) %>% 
        select(dep,jour,name,inhabitant,total_rea,total_hosp,total_dc,total_rad) %>% 
        mutate(
            
             inhabitant = inhabitant %>% scales::comma()
            ,total_dc   = total_dc %>% scales::comma()
            ,total_rad  = total_rad %>% scales::comma()
            ,total_rea  = total_rea %>% scales::comma()
               
               )
    
    
    
    
    
    
   

         valuebox_tbl %>%
          summarise(
            state_metric      = input$picker_dep,
            population_metric = inhabitant[name == input$picker_dep][1],
            icu_total_metric  = total_rea[name  == input$picker_dep][1],
            hosp_total_metric = total_hosp[name == input$picker_dep][1],
            dc_total_metric   = total_dc[name   == input$picker_dep][1],
            rad_total_metric  = total_rad[name  == input$picker_dep][1],
    


    )


  },
  ignoreNULL = FALSE)

# output$box_pop <- renderText({
#     summary_values_tbl()$population_metric %>% pluck()
# })

output$box_dep <- renderValueBox({
  valueBox(value = summary_values_tbl()$state_metric,
           subtitle = 'Departments')                         
})



output$box_pop <-  renderValueBox({
  valueBox(
    value = summary_values_tbl()$population_metric,
    subtitle = 'Population'
   
    )
})



output$box_rea <- renderValueBox({
  valueBox(
    value    = summary_values_tbl()$icu_total_metric, 
    subtitle = "ICU")                                  
})




output$box_hosp <- renderValueBox({
  valueBox(
    value    = summary_values_tbl()$hosp_total_metric,
    subtitle = 'Hospitalized')
})
"fas fa-h-square"
output$box_dc <- renderValueBox({
  valueBox(
    value    = summary_values_tbl()$dc_total_metric,  
    subtitle = 'Deahts')                          
})



output$box_rad <- renderValueBox({
  valueBox(
    value    = summary_values_tbl()$rad_total_metric,  
    subtitle = "Recoverd")                          
})
"fas fa-house-user"

#output$my_list <- renderPrint({summary_values_tbl()})





                       



   
   
   
   
}
# RUN APP ----
#thematic::thematic_shiny()
shinyApp(ui=ui,server = server)