
library(dplyr)
library(shiny)
library(DT)
library(rhandsontable)
library(shinyWidgets)

approved_books <- c("r4ds","advanced-r","feat","ggplot2","r-packages")

days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

time_slots <- data.frame(time_slot = seq(from = 0, to = 23, by =1))
time_slots <- time_slots %>% mutate(
    time_slot = if_else(time_slot == 12, paste(12,"PM"),
                        if_else(time_slot > 12, paste(time_slot - 12, "PM"),
                                if_else(time_slot == 0, paste(12,"AM"), paste(time_slot, "AM")))))

sl  <-  data.frame(sno = seq_len(nrow(time_slots)))

cal <- matrix(F, nrow = 24, ncol = 7, dimnames = list(time_slots$time_slot, days))  %>% 
    data.frame()

cbox_names = rep(paste0("cbox-",seq_len(nrow(time_slots))))

x <- purrr::map(.x = sl$s, .y = days, .f = ~ paste0("cbox-", .x,"-", .y)) %>%
    data.frame() %>% 
    t() %>%
    data.frame() %>% 
    setNames(days)

row.names(x) <- time_slots$time_slot

calendar_view <- do.call(cbind, apply(data.frame(time_slots), 2, function(x) data.frame(x,x,x,x,x,x,x))) %>%
    rename_with( ~ days, names(.))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("R4DS Book Club Planner"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "username", label = "Name", value = ""),
            selectInput(inputId = "bookname", label = "Select Book", choices = approved_books),
            #selectInput(inputId = "timezone", label = "Select Your Time Zone", choices = OlsonNames()),
            actionButton(inputId = "submit", label = "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel( 
           #DT::dataTableOutput("mytable"), #, width = "1%"),
           rHandsontableOutput("dt"),
           # <br>
           
           h4("Your Selections"),
           tableOutput("selected")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # browser()
    
    time_selection_df <- reactiveValues(data = cal)
    
    # display the week calendar
    output$dt <- renderRHandsontable({
        rhandsontable(time_selection_df$data) #, width = 550, height = 300)
    })
    
    # time_selection_df <-  calendar_view  # needs to be reactive & shd include the status
    
    observeEvent(
        input$table$changes$changes, # observe if any changes to the cells of the rhandontable
        {
            
            xi=input$table$changes$changes[[1]][[1]] # capture the row of the cell which changed
            yi=input$table$changes$changes[[1]][[2]] # capture the column of the cell which changed
            old = input$table$changes$changes[[1]][[3]] # fetches the old values of the cell
            new = input$table$changes$changes[[1]][[4]] # fetches the new value of the cell
            
            datavalues$data <- hot_to_r(input$table) # convert the rhandontable to R data frame object so manupilation / calculations could be done
        }
        
    )
    
    observeEvent(input$submit,{
       user_df <-  data.frame(
            book_name            = input$bookname,
            name                 = input$name,
            # tz                   = input$timezone,
            submission_timestamp = Sys.time()
        )
       
       user_df <- cbind(user_df, datavalues$data())
       
    }
  )
    
    #output$selected <- renderTable( head(user_df()))
       
    # find a way to save and print the dataframes at diff levels. 
    # print(head(user_df))
    
    # cbind( the user_df with time_selection_df ... 
    # the version that has changed to get he latest selections)
    
}

# Run the application
shinyApp(ui = ui, server = server)

