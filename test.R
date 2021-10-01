library(shiny)
library(shinythemes)
library(tidyverse)


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
                    
                    h4("New Package Detail"),
                    uiOutput('result'),
                    h4("Competitor Package"),
                    uiOutput('table'),
                    h4("Comparison"),
                    textOutput('datafungsi'),
                    
                  
                    
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
    
    df <- data.frame("Package Name"=input$package_name_detail, "Status"=input$status, "Package Category"=input$pc, "Launch Date"=as.character(input$date), "Package Activity Status"=input$pas, "Package Price"=input$package_price,
                     "Package Type 1"=input$pt1, "Package Type 2"=input$pt2, "Package Validity"=input$package_validity, "VAS"=input$vas, "Type of VAS"=input$tvas, "Package Business Category"=input$pbc,
                     "Category Business"=input$category_business, "SOP"=input$SOP)
    return(list(df=df))
    
  })
  
  output$result <- renderTable({
    Data()$df
  })
  
  
  datasetInput <- reactive({
    #OC <- read.csv(file = 'OC.csv', header = T, sep=";")
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
    return(list(OC=OC, OC_1=OC_1, 
                OC_2=OC_2, 
                OC_3=OC_3, 
                OC_4=OC_4, 
                OC_5=OC_5, 
                OC_6=OC_6,
                OC_7=OC_7,
                OC_8=OC_8,
                OC_9=OC_9,
                OC_10=OC_10,
                OC_11=OC_11,
                OC_12=OC_12,
                OC_13=OC_13,
                OC_14=OC_14,
                OC_15=OC_15,
                OC_16=OC_16,
                OC_17=OC_17,
                OC_18=OC_18,
                OC_19=OC_19,
                OC_20=OC_20,
                OC_21=OC_21,
                OC_22=OC_22,
                OC_23=OC_23,
                OC_24=OC_24,
                OC_25=OC_25,
                OC_26=OC_26,
                OC_27=OC_27,
                OC_28=OC_28,
                OC_29=OC_29,
                OC_30=OC_30,
                OC_31=OC_31,
                OC_32=OC_32,
                OC_33=OC_33,
                OC_34=OC_34,
                OC_35=OC_35,
                OC_36=OC_36,
                OC_37=OC_37,
                OC_38=OC_38,
                OC_39=OC_39,
                OC_40=OC_40,
                OC_41=OC_41,
                OC_42=OC_42,
                OC_43=OC_43,
                OC_44=OC_44,
                OC_45=OC_45,
                OC_46=OC_46,
                OC_47=OC_47,
                OC_48=OC_48,
                OC_49=OC_49,
                OC_50=OC_50,
                OC_51=OC_51,
                OC_52=OC_52,
                OC_53=OC_53,
                OC_54=OC_54,
                OC_55=OC_55,
                OC_56=OC_56
    ))
    
    
  })
  
  datamatch <- reactive({
    if(input$package_price == 0){
      b = print("no data")
    }
    else if(input$package_price <=20000 & input$category_business == "Validity + quota"){
      b = datasetInput()$OC_1
    }else if(input$package_price <=20000 & input$category_business =="Roaming"){
      b = datasetInput()$OC_2
    }else if(input$package_price <=20000 & input$category_business =="VAS"){
      b = datasetInput()$OC_3
    }else if(input$package_price <=20000 & input$category_business =="Unlimited"){
      b = datasetInput()$OC_4
    }else if(input$package_price <=20000 & input$category_business =="Voice"){
      b = datasetInput()$OC_5
    }else if(input$package_price <=20000 & input$category_business =="Sms"){
      b = datasetInput()$OC_6
    }else if(input$package_price <=20000 & input$category_business =="Bonus/promotion"){
      b = datasetInput()$OC_49
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Validity + quota"){
      b = datasetInput()$OC_7
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Roaming"){
      b = datasetInput()$OC_8
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="VAS"){
      b = datasetInput()$OC_9
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Unlimited"){
      b = datasetInput()$OC_10
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Voice"){
      b = datasetInput()$OC_11
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Sms"){
      b = datasetInput()$OC_12
    }else if(input$package_price >20000 & input$package_price <=50000 & input$category_business =="Bonus/promotion"){
      b = datasetInput()$OC_50
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Validity + quota"){
      b = datasetInput()$OC_13
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Roaming"){
      b = datasetInput()$OC_14
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="VAS"){
      b = datasetInput()$OC_15
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Unlimited"){
      b = datasetInput()$OC_16
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Voice"){
      b = datasetInput()$OC_17
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Sms"){
      b = datasetInput()$OC_18
    }else if(input$package_price >50000 & input$package_price <=75000 & input$category_business =="Bonus/promotion"){
      b = datasetInput()$OC_51
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Validity + quota"){
      b = datasetInput()$OC_19
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Roaming"){
      b = datasetInput()$OC_20
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="VAS"){
      b = datasetInput()$OC_21
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Unlimited"){
      b = datasetInput()$OC_22
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Voice"){
      b = datasetInput()$OC_23
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Sms"){
      b = datasetInput()$OC_24
    }else if(input$package_price >75000 & input$package_price <=100000 & input$category_business =="Bonus/promotion"){
      b = datasetInput()$OC_52
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Validity + quota"){
      b = datasetInput()$OC_25
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Roaming"){
      b = datasetInput()$OC_26
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="VAS"){
      b = datasetInput()$OC_27
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Unlimited"){
      b = datasetInput()$OC_28
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Voice"){
      b = datasetInput()$OC_29
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Sms"){
      b = datasetInput()$OC_30
    }else if(input$package_price >100000 & input$package_price <=150000 & input$category_business =="Bonus/promotion"){
      b = datasetInput()$OC_53
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Validity + quota"){
      b = datasetInput()$OC_31
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Roaming"){
      b = datasetInput()$OC_32
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="VAS"){
      b = datasetInput()$OC_33
    }else if(inpu$package_price >150000 & input$package_price <=200000 & input$category_business =="Unlimited"){
      b = datasetInput()$OC_34
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Voice"){
      b = datasetInput()$OC_35
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Sms"){
      b = datasetInput()$OC_36
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Bonus/promotion"){
      b = datasetInput()$OC_54
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Validity + quota"){
      b = datasetInput()$OC_37
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Roaming"){
      b = datasetInput()$OC_38
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="VAS"){
      b = datasetInput()$OC_39
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Unlimited"){
      b = datasetInput()$OC_40
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Voice"){
      b = datasetInput()$OC_41
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Sms"){
      b = datasetInput()$OC_42
    }else if(input$package_price >200000 & input$package_price <=400000 & input$category_business =="Bonus/promotion"){
      b = datasetInput()$OC_55
    }else if(input$package_price >400000 & input$category_business =="Validity + quota"){
      b = datasetInput()$OC_43
    }else if(input$package_price >400000 & input$category_business =="Roaming"){
      b = datasetInput()$OC_44
    }else if(input$package_price >400000 & input$category_business =="VAS"){
      b = datasetInput()$OC_45
    }else if(input$package_price >400000 & input$category_business =="Unlimited"){
      b = datasetInput()$OC_46
    }else if(input$package_price >400000 & input$category_business =="Voice"){
      b = datasetInput()$OC_47
    }else if(input$package_price >400000 & input$category_business =="Sms"){
      b = datasetInput()$OC_48
    }else if(input$package_price >400000 & input$category_business =="Bonus/promotion"){
      b = datasetInput()$OC_56
    }
    return(list(b=b))
  })
  
  output$table <- renderTable({
    print(datamatch()$b)
  }
  )
  
  datafungsi <- reactive({
    if(#dim(b)[1]==0 #using dimension
      length(datamatch()$b[,1])==0){#using length
      print("There are No Similiar Competitor Package")
    }
  })
  
  output$datafungsi <- renderText({
    print(datafungsi)
  })
  
  
  
  
} # server


# Create Shiny object

shinyApp(ui = ui, server = server)







