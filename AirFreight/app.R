#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(viridis)
library(readr)


# Generate sample data
pos = data.table(whse = sample(c('CA1', 'CA2', 'CA3'), size = 4000, replace = TRUE),
                 po = as.character(seq(from = 2001, to = 6000)),
                 units_remain = sample(0:4000, size = 4000, replace = TRUE),
                 date_received = sample(seq(from = as.Date('2017-07-01'), 
                                            to = as.Date('2018-03-31'), by = "day"), size = 4000, replace = TRUE),
                 shipping_instructions = sample(c('Company Paid', 'Vendor Paid'), size = 4000, replace = TRUE)) 

pos = pos %>%
  mutate(orig_po_qty = units_remain + sample(1:4000, size = 4000, replace = TRUE),
         month_received = factor(months.Date(date_received),
                                 levels = c('July', 'August', 'September', 'October',
                                            'November', 'December', 'January', 'February', 'March'),
                                 ordered = TRUE),
         percent_remain = 100 * round(units_remain / orig_po_qty, 4))


# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Air Freight Dashboard"),
   
   # Sidebar with a slider input for filtering PO percent remaining
   # Checkboxes for selecting warehouses and Month Received
   
   sidebarLayout(
      sidebarPanel(width = 2,
         checkboxGroupInput(inputId = "whse", label = "Warehouse",
                            choices = c('CA1', 'CA2', 'CA3'),
                            selected = c('CA1', 'CA2', 'CA3')),
         br(),
         checkboxGroupInput(inputId = "month", label = "Month Received",
                            choices = levels(pos$month_received),
                            selected = levels(pos$month_received)),
         br(),
         sliderInput(inputId = "percent", label = "Percent of PO Remaining",
                     min = 0, max = 100, value = c(0, 100))
         ),
      
      # Show a plot of the generated distribution and corresponding data
      mainPanel(width = 10,
         plotOutput("plot"),
         br(),
         dataTableOutput("dt")
      )
   )
)

# Define server logic required to draw chart and table
server <- function(input, output) {
   
  # Create reactive data object for use in table and chart
   data = reactive({
     data = pos %>%
              filter(whse %in% input$whse & 
                       month_received %in% input$month & 
                       percent_remain >= input$percent[1] & 
                       percent_remain <= input$percent[2]) %>%
              select(whse, po, units_remain, orig_po_qty, percent_remain, shipping_instructions, month_received)
     
     return(data)
   })
   
   output$plot <- renderPlot({
       ggplot(data(), aes(x = percent_remain, fill = month_received)) +
       geom_histogram(binwidth = 5, col = "black") +
       scale_fill_viridis(option = "inferno", discrete = TRUE) +
       facet_wrap(~shipping_instructions) +
       labs(title = "Air Freight POs in Reserve Racks",
            subtitle = "Comparing units remaining in warehouse vs. original PO quantity. Binwidth is 5%",
            x = 'Percent of PO Remaining',
            y = 'Number of POs') +
       guides(fill = guide_legend(title = "Month Received"))
   })
   
   output$dt = renderDT({
     data()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

