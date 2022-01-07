
library(shiny)
library(shinyWidgets)
library(dplyr)

approved_books <- c("r4ds","advanced-r","feat","ggplot2","r-packages")

days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
time_slots <- data.frame(time_slot = seq(from = 0, to = 23, by =1))

time_slots <- time_slots %>% mutate(
    time_slot = if_else(time_slot == 12, paste(12,"PM"),
                        if_else(time_slot > 12, paste(time_slot - 12, "PM"),
                                if_else(time_slot == 0, paste(12,"AM"), paste(time_slot, "AM")))))

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
            selectInput(inputId = "bookname", label = "Select Book", choices = approved_books, selected = "None"),
            selectInput(inputId = "timezone", label = "Select Your Time Zone", choices = OlsonNames(), selected = "None"),
            actionButton(inputId = "submit", label = "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
           DT::dataTableOutput("mytable", width = "1%")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # browser()
    # helper function for making checkbox
    shinyInput <- function(FUN, len, id, ...) { 
        inputs <- character(len) 
        for (i in seq_len(len)) { 
            inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...)) 
        } 
        inputs 
    } 
    
    # datatable with checkbox
    output$mytable <- DT::renderDataTable( 
        expr = {
            df <- data.frame(
                #  my_df,
                time_slots,
                #Favorite1 = shinyInput(checkboxInput, nrow(time_slots), "cbox1"), 
                Monday = shinyInput(checkboxInput, nrow(time_slots), "cbox2")
            )
            # names(df)[1] <- " "
            df
        }, 
        rownames = FALSE,
        server = FALSE, 
        escape = FALSE, 
        options = list(
            ordering = FALSE,
            searching = FALSE,
            paging = FALSE,
            info = FALSE,
            preDrawCallback = JS("function() { 
          Shiny.unbindAll(this.api().table().node()); }"
            ), 
            drawCallback = JS("function() { 
          Shiny.bindAll(this.api().table().node()); } "
            ) 
        )
    )
    
    
    # helper function for reading checkbox
    shinyValue <- function(id, len) { 
        unlist(
            x = lapply(
                X = seq_len(len), 
                FUN = function(i) { 
                    value = input[[paste0(id, i)]] 
                    if (is.null(value)) {
                        NA
                    } else {
                        value
                    }  
                }
            )
        ) 
    } 
    
    
    
    # output read checkboxes
    output$checked <- renderTable({
        data.frame(
            #Favorite1 = shinyValue("cbox1", nrow(time_slots)),
            Monday = shinyValue("cbox2", nrow(time_slots))
        )
    }
    )
    
    time_selection_df <-  calendar_view  # needs to be reactive & shd include the status
    
    observeEvent(input$submit,{(
       user_df <-  data.frame(
            book_name            = input$bookname,
            name                 = input$name,
            tz                   = input$timezone,
            submission_timestamp = Sys.time()
        )
    )}
                 
                 )
}

# Run the application
shinyApp(ui = ui, server = server)

