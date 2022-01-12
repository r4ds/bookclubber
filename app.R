
library(dplyr)
library(shiny)
library(DT)
library(gt)
library(rhandsontable)
library(shinyWidgets)
library(googlesheets4)

approved_books <- c("r4ds","advanced-r","feat","ggplot2","r-packages")

days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

time_slots <- data.frame(time_slot = seq(from = 0, to = 23, by =1))
time_slots <- time_slots %>% mutate(
    time_slot = if_else(time_slot == 12, paste(12,"PM"),
                        if_else(time_slot > 12, paste(time_slot - 12, "PM"),
                                if_else(time_slot == 0, paste(12,"AM"), paste(time_slot, "AM")))))

sl  <-  data.frame(sno = seq_len(nrow(time_slots)))

running_book_clubs <- matrix(F, nrow = 24, ncol = 7) 
# creating dummy data to test the concept of removing unavailable times. To be replaced with actual data from Jon.
running_book_clubs[1,] <- TRUE
running_book_clubs[,1] <- TRUE

cal <- (running_book_clubs - 
            matrix(F, nrow = 24, ncol = 7, dimnames = list(time_slots$time_slot, days)))  %>% 
    data.frame() %>% 
    mutate(across(c(Monday:Sunday), na_if, TRUE)) %>% 
    mutate_at(vars(Monday:Sunday),  as.logical)

cbox_names = rep(paste0("cbox-",seq_len(nrow(time_slots))))

x <- purrr::map(.x = sl$s, .y = days, .f = ~ paste0("cbox-", .x,"-", .y)) %>%
    data.frame() %>% 
    t() %>%
    data.frame() %>% 
    setNames(days)

row.names(x) <- time_slots$time_slot

calendar_view <- do.call(cbind, apply(data.frame(time_slots), 2, function(x) data.frame(x,x,x,x,x,x,x))) %>%
    rename_with( ~ days, names(.))

# Google login ( for the maintainer, most likely 1 time login setup)
gs4_auth(
    email = gargle::gargle_oauth_email(),
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("R4DS Book Club Planner"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            shiny::textInput(inputId = "username", label = "Name", value = ""),
            shiny::selectInput(inputId = "bookname", label = "Select Book", choices = approved_books, selected = "r4ds"),
            selectInput(inputId = "timezone", label = "Select Your Time Zone", choices = OlsonNames()),
            shiny::actionButton(inputId = "submit", label = "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel( 
           rHandsontableOutput("time_table"),
           
           h4("Your Selections"),
           tableOutput("selected"),
           
           # h4("Your Submitted Info "),
           # # tableOutput("text"),
           # tableOutput("text2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # browser()
    # time_selection_df <- reactiveValues(data = cal)
    
    # display the week calendar
    output$time_table <- renderRHandsontable({
        rhandsontable(cal) #, width = 550, height = 300)
    })
    
    observeEvent(
        input$time_table$changes$changes, # observe if any changes to the cells of the rhandontable
        {
        time_selections <- reactive({
            hot_to_r(input$time_table)
        })
        
        time_selections_long <- reactive({
            hot_to_r(input$time_table) %>% 
                rownames_to_column(var = "time") %>% 
                tidyr::pivot_longer(cols = Monday:Sunday, names_to = "day", values_to = "availability") %>% 
                identity()
        })
            
        output$selected <- renderTable({
            
            time_selections_long() %>% 
                filter(availability == TRUE) %>%
                group_by(day) %>% 
                mutate(availability = stringr::str_flatten(time, collapse = ", ")) %>% 
                # tidyr::unnest_wider(time) %>% 
                select(-time) %>% # -availability,
                distinct() %>% 
                identity()
        }) 
    })
    
    # output$text <- renderText({
    #     str(time_selections())
    # })
    
    # Save the user details
    user_info <-  reactive({
        data.frame(
        book_name            = input$bookname,
        name                 = input$username,
        tz                   = input$timezone,
        submission_timestamp = as.character(Sys.time())
    )
    })
    
    user_availability_df <- eventReactive(input$submit,{
       
       cbind(
           # Repeat user details for so many rows
           # rep(user_info(), times = 24*7) , # %>% data.frame(),
           user_info() , # %>% data.frame(),
           # Save the availability details join it with
           hot_to_r(input$time_table))
     })
    
    # output$text <- renderTable({
    #     user_info()[,1:2]
    #     # head(user_availability_df())
    # })
     
    output$text2 <- renderTable({
        user_availability_df()
    })

    observeEvent(input$submit,{
        # Submit & Save the response on the googlesheets file
        sheet_append("https://docs.google.com/spreadsheets/d/1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc/edit#gid=0",
                     user_availability_df(), sheet = 1)
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

