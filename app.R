
library(shiny)
library(shinyWidgets)
library(dplyr)
library(DT)
library(rhandsontable)

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
            selectInput(inputId = "bookname", label = "Select Book", choices = approved_books, selected = "None"),
            selectInput(inputId = "timezone", label = "Select Your Time Zone", choices = OlsonNames(), selected = "None"),
            actionButton(inputId = "submit", label = "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
           #DT::dataTableOutput("mytable"), #, width = "1%"),
           rHandsontableOutput("dt")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # browser()
    # helper function for making checkbox
    # shinyInput <- function(FUN, len, id, ...) { 
    #     inputs <- character(len) 
    #     for (i in seq_len(len)) { 
    #         inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...)) 
    #     } 
    #     inputs 
    # } 
    
    shinyInput2 <- function() { 
        
        inputs <- purrr::map_dfr(.x = sl$sno ,
                                .f = ~ checkboxGroupInput(paste0("cboxgrp-", .x), label = NULL) )
        inputs 
    }
    
    # datatable with checkbox
    output$mytable <- DT::renderDataTable( 
        expr = {
            # browser()
            df <- data.frame(
                #  my_df,
                
                # time_slots ,
                # Favorite1 = shinyInput(checkboxInput, nrow(time_slots), "cbox1")#, 
                # Monday = shinyInput(checkboxInput, nrow(time_slots), "cbox2")# ,
                # purrr::pmap(paste0("cbox",i), nrow(time_slots), checkboxInput,  ~ shinyInput(.x,.y))
                # pmap(~fcn(.x, .y))
                
                # purrr::pmap(.x = checkboxInput,
                #             .y = nrow(time_slots),
                #             .z = paste0("cbox",seq(1:7)),
                #             .f = ~ shinyInput
                # )
                # shinyInput2(nrow(time_slots), days)
               
                Monday = shinyInput2()# ,
               #  purrr::map_dfc(.x = cbox_names, .y = days,
               #                 .f = ~ checkboxInput(inputId = paste0(.x,"-", .y), label = NULL))  %>%
               #      
               #      # data.frame() %>% 
               #      # t() %>%
               #      # data.frame() %>% 
               #      # setNames(days)
               # identity()
                # bind_cols(time_slots, .)
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
    
    output$dt <- renderRHandsontable({
        rhandsontable(cal, width = 550, height = 300)
    })
    
    # helper function for reading checkbox
    # shinyValue <- function(id, len) { 
    #     unlist(
    #         x = lapply(
    #             X = seq_len(len), 
    #             FUN = function(i) { 
    #                 value = input[[paste0(id, i)]] 
    #                 if (is.null(value)) {
    #                     NA
    #                 } else {
    #                     value
    #                 }  
    #             }
    #         )
    #     ) 
    # } 
    
    
    
    # output read checkboxes
    # output$checked <- renderTable({
    #     data.frame(
    #         #Favorite1 = shinyValue("cbox1", nrow(time_slots)),
    #         Monday = shinyValue("cbox2", nrow(time_slots))
    #     )
    # }
    # )
    
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

