library(shiny)
library(shinythemes)
library(tidyverse)
OC <- read.csv("OC.csv", header = T, sep = ";")
#head(OC)
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
                    actionButton("Check", "COMPARE"),
                    
                    
                    
                  ), # sidebarPanel
                  mainPanel(
                    h1("Result"),
                    
                    h4("Data Input"),
                    uiOutput('table'),
                    
                    h4("Result Output 2"),
                    uiOutput('result')
                  ) # mainPanel
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  
  
  SF = reactive({
    
    data <- data.frame("Package Name"=input$package_name_detail, "Status"=input$status, "Package Category"=input$pc, "Launch Date"=input$date, "Package Activity Status"=input$pas, "Package Price"=input$package_price,
                     "Package Type 1"=input$pt1, "Package Type 2"=input$pt2, "Package Validity"=input$package_validity, "VAS"=input$vas, "Type of VAS"=input$tvas, "Package Business Category"=input$pbc,
                     "Category Business"=input$category_business, "SOP"=input$SOP)
    return(list(data=data))
    
  })
  rv <- reactiveValues(
    case = NULL
  )
 
  rv$case <- eventReactive(input$Check,{
    OC_1 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Validity + quota")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_2 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Roaming")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_3 <- OC %>% filter(Package.Price <=20000 & Category.Business =="VAS")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_4 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Unlimited")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_5 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Voice")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_6 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Sms")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_49 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Bonus/promotion")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    
    OC_7 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Validity + quota")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_8 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Roaming")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_9 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="VAS")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_10 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Unlimited")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_11 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Voice")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_12 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Sms")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_50 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Bonus/promotion")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    
    OC_13 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Validity + quota")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_14 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Roaming")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_15 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="VAS")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_16 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Unlimited")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_17 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Voice")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_18 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Sms")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_51 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Bonus/promotion")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    
    OC_19 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Validity + quota")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_20 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Roaming")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_21 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="VAS")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_22 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Unlimited")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_23 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Voice")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_24 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Sms")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_52 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Bonus/promotion")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    
    OC_25 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Validity + quota")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_26 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Roaming")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_27 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="VAS")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_28 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Unlimited")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_29 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Voice")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_30 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Sms")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_53 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Bonus/promotion")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    
    OC_31 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Validity + quota")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_32 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Roaming")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_33 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="VAS")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_34 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Unlimited")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_35 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Voice")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_36 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Sms")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_54 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Bonus/promotion")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    
    OC_37 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Validity + quota")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_38 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Roaming")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_39 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="VAS")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_40 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Unlimited")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_41 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Voice")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_42 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Sms")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_55 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Bonus/promotion")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    
    OC_43 <- OC %>% filter(Package.Price >400000 & Category.Business =="Validity + quota")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_44 <- OC %>% filter(Package.Price >400000 & Category.Business =="Roaming")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_45 <- OC %>% filter(Package.Price >400000 & Category.Business =="VAS")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_46 <- OC %>% filter(Package.Price >400000 & Category.Business =="Unlimited")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_47 <- OC %>% filter(Package.Price >400000 & Category.Business =="Voice")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_48 <- OC %>% filter(Package.Price >400000 & Category.Business =="Sms")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    OC_56 <- OC %>% filter(Package.Price >400000 & Category.Business =="Bonus/promotion")%>% select(c(Package.Price, Package.Validity, Value.added.Service.VAS.., SOP))
    
    if(input$package_price <=20000 & input$category_business == "Validity + quota"){
      b = OC_1
    }else if(input$package_price <=20000 & input$category_business =="Roaming"){
      b = OC_2
    }else if(input$package_price <=20000 & input$category_business =="VAS"){
      b = OC_3
    }else if(input$package_price <=20000 & input$category_business =="Unlimited"){
      b = OC_4
    }else if(input$package_price <=20000 & input$category_business =="Voice"){
      b = OC_5
    }else if(input$package_price <=20000 & input$category_business =="Sms"){
      b = OC_6
    }else if(input$package_price <=20000 & input$category_business =="Bonus/promotion"){
      b = OC_49
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Validity + quota"){
      b = OC_7
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Roaming"){
      b = OC_8
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="VAS"){
      b = OC_9
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Unlimited"){
      b = OC_10
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Voice"){
      b = OC_11
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Sms"){
      b = OC_12
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Bonus/promotion"){
      b = OC_50
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Validity + quota"){
      b = OC_13
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Roaming"){
      b = OC_14
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="VAS"){
      b = OC_15
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Unlimited"){
      b = OC_16
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Voice"){
      b = OC_17
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Sms"){
      b = OC_18
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Bonus/promotion"){
      b = OC_51
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Validity + quota"){
      b = OC_19
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Roaming"){
      b = OC_20
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="VAS"){
      b = OC_21
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Unlimited"){
      b = OC_22
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Voice"){
      b = OC_23
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Sms"){
      b = OC_24
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Bonus/promotion"){
      b = OC_52
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Validity + quota"){
      b = OC_25
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Roaming"){
      b = OC_26
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="VAS"){
      b = OC_27
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Unlimited"){
      b = OC_28
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Voice"){
      b = OC_29
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Sms"){
      b = OC_30
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Bonus/promotion"){
      b = OC_53
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Validity + quota"){
      b = OC_31
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Roaming"){
      b = OC_32
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="VAS"){
      b = OC_33
    }else if(inpu$package_price >150000 & input$package_price <=200000 & input$category_business =="Unlimited"){
      b = OC_34
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Voice"){
      b = OC_35
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Sms"){
      b = OC_36
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Bonus/promotion"){
      b = OC_54
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Validity + quota"){
      b = OC_37
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Roaming"){
      b = OC_38
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="VAS"){
      b = OC_39
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Unlimited"){
      b = OC_40
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Voice"){
      b = OC_41
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Sms"){
      b = OC_42
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Bonus/promotion"){
      b = OC_55
    }else if(input$package_price >400000 & input$category_business =="Validity + quota"){
      b = OC_43
    }else if(input$package_price >400000 & input$category_business =="Roaming"){
      b = OC_44
    }else if(input$package_price >400000 & input$category_business =="VAS"){
      b = OC_45
    }else if(input$package_price >400000 & input$category_business =="Unlimited"){
      b = OC_46
    }else if(input$package_price >400000 & input$category_business =="Voice"){
      b = OC_47
    }else if(input$package_price >400000 & input$category_business =="Sms"){
      b = OC_48
    }else if(input$package_price >400000 & input$category_business =="Bonus/promotion"){
      b = OC_56
    }
    if(#dim(b)[1]==0 #using dimension
      length(b[,1])==0){#using length
      paste("There are No Similiar Competitor Package")
    }else{
      total_poin = 0;
      for(i in 1: length(b[,1])){
        temp_poin = 0
        #print(b[i,]$Package.Price)
        #print(length(b$Package.Price))
        if(input$package_price <= b[i,]$Package.Price){
          temp_poin = temp_poin+1
        }
        else{
          temp_poin = temp_poin+0
        }
        if(input$package_validity >= b[i,]$Package.Validity){
          temp_poin = temp_poin+1
        }
        if(input$vas=="Yes"){
          temp_poin = temp_poin +1
        }else if(inputvas=="No" & b[i,]$Value.added.Service.VAS..=="No"){
          temp_poin = temp_poin +1
        }
        if(input$SOP == b[i,]$SOP){
          temp_poin = temp_poin +1
        }else if(input$SOP == 6){
          temp_poin = temp_poin +1
        }else if(input$SOP == 3 & b[i,]$SOP ==2){
          temp_poin = temp_poin +1
        }else if(input$SOP == 2 & b[i,]$SOP ==1){
          temp_poin = temp_poin +1
        }else if(input$SOP == 3 & b[i,]$SOP ==1){
          temp_poin = temp_poin +1
        }else if(input$SOP != b[i,]$SOP & b[i,]$SOP !=6){
          temp_poin = temp_poin +1
        }
        if(temp_poin > 2){
          total_poin = total_poin +1
        }
      }
      #print()
      if(total_poin / length(b[,1])>= 0.5){
        paste("Fit to the market")
      }else if(total_poin/length(b[,1])<0.5){
        paste("Not fit to the market")
      }
    }
    
  }) 
  
  output$result <- renderText({
    return()
    rv$case()
  })
  
  output$table <- renderTable({
    print(SF()$data)
  })
} # server


# Create Shiny object

shinyApp(ui = ui, server = server)







