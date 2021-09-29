library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  theme = "cerulean",  
                  title = "Best Market Fit Offer",
                  sidebarPanel(
                    tags$h3("Input Detail New Package:"),
                    textInput("package_name_detail", label = h3("Package Name :"), ""),
                    radioButtons("status", label = h3("Status"),
                                 choices = list("Active", "Not Active" ), 
                    ),
                    selectInput("pc", label = h3("Package Category"), 
                                choices = list("Prepaid" , "Postpaid", "Both"), 
                    ),
                    dateInput("date", label = h3("Launch Date")),
                    radioButtons("pas", label = h3("Package Activity Status"),
                                 choices = list("Yes" , "No"), 
                    ),
                    numericInput("package_price", label = h3("Package Price"), value = 0),
                    radioButtons("pt1", label = h3("Package Type 1 (with per day data limit till validity)"),
                                 choices = list("Yes" , "No" ), 
                    ),
                    radioButtons("pt2", label = h3("Package Type 2 (package with unlimited usage data per day)"),
                                 choices = list("Yes" , "No" ), 
                    ),
                    numericInput("package_validity", label = h3("Package Validity"), value = 0),
                    radioButtons("vas", label = h3("VAS (Value Added Service)"),
                                 choices = list("Yes" , "No" ), 
                    ),
                    radioButtons("tvas", label = h3("Type of VAS"),
                                 choices = list("VAS" , "Yes"), 
                    ),
                    selectInput("pbc", label = h3("Package Business Category"), 
                                choices = list("BB" , "CLM" , "Corporate" , "International" , "My Special Plan", "MySmartVALUE",
                                               "Paket Video Harian", "PUAS Plan", "QOS", "Service Plan", "Simas Plan", "Switch", "VAS", "Voice"), 
                    ),
                    selectInput("category_business", label = h3("Category Business"), 
                                choices = list("Validity + quota", "Roaming" , "VAS" , "Unlimited" , "Voice", "Sms",
                                               "Bonus/promotion"), 
                    ),
                    selectInput("SOP", label = h3("SOP (Scheme of Package)"), 
                                choices = list("1" , "2" , "3" , "4" , "5", "6")
                    ),
                    submitButton("Check"),
                    
                    
                    
                  ), # sidebarPanel
                  mainPanel(
                    h1("Result"),
                    
                    h4("Output 1"),
                    uiOutput('table')
                    
                  ) # mainPanel
                  
                  
                  
                ) # navbarPage
) # fluidPage
#myFunction <- function(input, output){
 # output$result <- renderText({
    
  #})
#}

# Define server function  
server <- function(input, output) {
  
  Data = reactive({
    
    df <- data.frame("Package Name"=input$package_name_detail, "Status"=input$status, "Package Category"=input$pc, "Launch Date"=input$date, "Package Activity Status"=input$pas, "Package Price"=input$package_price,
                     "Package Type 1"=input$pt1, "Package Type 2"=input$pt2, "Package Validity"=input$package_validity, "VAS"=input$vas, "Type of VAS"=input$tvas, "Package Business Category"=input$pbc,
                     "Category Business"=input$category_business, "SOP"=input$SOP)
    return(list(df=df))
    
  })
  
  output$table <- renderTable({
    print(Data()$df)
  }
  )
} # server


# Create Shiny object

shinyApp(ui = ui, server = server)







