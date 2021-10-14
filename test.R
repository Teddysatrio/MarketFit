library(shiny)
library(shinythemes)
library(tidyverse)
library(cat)
library(plotly)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Best Market Fit Offer"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    div(style = 'overflow-x: scroll', sidebarPanel(width = 4,
                                                   tags$h3("Input Detail New Package:", style="font-size:30px; font-style:bold; color:black"),
                                                   textInput("package_name_detail", label = h3("Package Name :", style="font-size:25px"), ""),
                                                   div(style="font-size:17px", radioButtons("status", label = h2("Status",  style="font-size:25px"),
                                                                                            choices = list("Active", "Not Active" ) 
                                                   )),
                                                   selectInput("pc", label = h2("Package Category",  style="font-size:25px"), 
                                                               choices = list("Prepaid" , "Postpaid", "Both"), 
                                                   ),
                                                   #dateInput("date", label = h3("Launch Date")),
                                                   div(style="font-size:17px",radioButtons("pas", label = h2("Package Activity Status",  style="font-size:25px"),
                                                                                           choices = list("Yes" , "No"), 
                                                   )),
                                                   numericInput("package_price", label = h2("Package Price",  style="font-size:25px"), value = 0),
                                                   div(style="font-size:17px",radioButtons("pt1", label = h2("Package Type 1 (with per day data limit till validity)",  style="font-size:25px"),
                                                                                           choices = list("Yes" , "No" ), 
                                                   )),
                                                   div(style="font-size:17px",radioButtons("pt2", label = h2("Package Type 2 (package with unlimited usage data per day)",  style="font-size:25px"),
                                                                                           choices = list("Yes" , "No" ), 
                                                   )),
                                                   numericInput("package_validity", label = h2("Package Validity",  style="font-size:25px"), value = 0),
                                                   div(style="font-size:17px",radioButtons("vas", label = h2("VAS (Value Added Service)",  style="font-size:25px"),
                                                                                           choices = list("Yes" , "No" ), 
                                                   )),
                                                   div(style="font-size:17px",radioButtons("tvas", label = h2("Type of VAS",  style="font-size:25px"),
                                                                                           choices = list("VAS" , "Yes", "No"), 
                                                   )),
                                                   selectInput("pbc", label = h2("Package Business Category",  style="font-size:25px"), 
                                                               choices = list("BB" , "CLM" , "Corporate" , "International" , "My Special Plan", "MySmartVALUE",
                                                                              "Paket Video Harian", "PUAS Plan", "QOS", "Service Plan", "Simas Plan", "Switch", "VAS", "Voice"), 
                                                   ),
                                                   selectInput("category_business", label = h2("Category Business",  style="font-size:25px"), 
                                                               choices = list("Validity + quota", "Roaming" , "VAS" , "Unlimited" , "Voice", "Sms",
                                                                              "Bonus/promotion"), 
                                                   ),
                                                   selectInput("SOP", label = h2("SOP (Scheme of Package)",  style="font-size:25px"), 
                                                               choices = list("1" , "2" , "3" , "4" , "5", "6")
                                                   ),
                                                   submitButton("Check"),
                                                   
    ),mainPanel(
      
      strong(h1("Market Fit Testing", style="font-style:bold; color:black ")),
      
      fluidRow(
        box(h3("Smartfren New Package"),width = 15 ,div(style = 'overflow-x: scroll', DT::dataTableOutput('result')))
      ),
      fluidRow(
        box(h3("Comparison"), br(), status = "primary", width = 15 ,
            (strong(uiOutput('datafungsi', style="font-size:17px"))
            ))),
      
      fluidRow(
        box(h3("Smartfren Package"),br(), status = "primary", width = 15,
            strong(uiOutput('datafung_SF', style="font-size:17px")))
      ),
      
      fluidRow(
        box(h3("Competitor Package"),width = 15 ,div(style = 'overflow-x: scroll', DT::dataTableOutput('table')))
      )
      
    )))
)

server <- function(input, output) {
  Data = reactive({
    
    df <- data.frame("Package Name"=input$package_name_detail, "Status"=input$status, "Package Category"=input$pc, #"Launch Date"=as.character(input$date), 
                     "Package Activity Status"=input$pas, "Package Price"=input$package_price,
                     "Package Type 1"=input$pt1, "Package Type 2"=input$pt2, "Package Validity"=input$package_validity, "VAS"=input$vas, "Type of VAS"=input$tvas, "Package Business Category"=input$pbc,
                     "Category Business"=input$category_business, "SOP"=input$SOP)
    return(list(df=df))
    
  })
  
  output$result <- DT::renderDataTable({
    DT::datatable(Data()$df)
  })
  
  datamatch_SF <- reactive({
    SF <- read.csv(file = 'SF.csv', header = T, sep=";")
    return(list(SF = SF))
  })
  
  datafungsi_SF <- reactive({
    d<-c()
    e<-"e"
    f<-"f"
    for(i in 1: length(datamatch_SF()$SF[,1])){
      if(input$package_price == datamatch_SF()$SF[i,]$package_price & input$package_validity == datamatch_SF()$SF[i,]$package_validity & input$vas == datamatch_SF()$SF[i,]$vas & input$SOP == datamatch_SF()$SF[i,]$SOP){
        d<-c(d,e)
        print(d)
      }else{
        d<-c(d,f)
        print(d)
      }
    }
    if(("e" %in% d)==T){
      print("This Package Already Exist")
    }else{
      print("New Package for Smartfren")
    }
  })
  output$datafung_SF <- renderText({
    print(datafungsi_SF())
  })
  
  datasetInput <- reactive({
    OC <- read.csv(file = 'OC.csv', header = T, sep=";")
    OC_1 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Validity + quota")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_2 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Roaming")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_3 <- OC %>% filter(Package.Price <=20000 & Category.Business =="VAS")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_4 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Unlimited")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_5 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Voice")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_6 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Sms")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_49 <- OC %>% filter(Package.Price <=20000 & Category.Business =="Bonus/promotion")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    
    OC_7 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Validity + quota")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_8 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Roaming")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_9 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="VAS")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_10 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Unlimited")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_11 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Voice")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_12 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Sms")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_50 <- OC %>% filter(Package.Price >20000 & Package.Price <=50000 & Category.Business =="Bonus/promotion")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    
    OC_13 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Validity + quota")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_14 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Roaming")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_15 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="VAS")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_16 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Unlimited")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_17 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Voice")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_18 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Sms")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_51 <- OC %>% filter(Package.Price >50000 & Package.Price <=75000 & Category.Business =="Bonus/promotion")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    
    OC_19 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Validity + quota")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_20 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Roaming")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_21 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="VAS")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_22 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Unlimited")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_23 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Voice")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_24 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Sms")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_52 <- OC %>% filter(Package.Price >75000 & Package.Price <=100000 & Category.Business =="Bonus/promotion")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    
    OC_25 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Validity + quota")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_26 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Roaming")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_27 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="VAS")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_28 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Unlimited")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_29 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Voice")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_30 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Sms")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_53 <- OC %>% filter(Package.Price >100000 & Package.Price <=150000 & Category.Business =="Bonus/promotion")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    
    OC_31 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Validity + quota")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_32 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Roaming")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_33 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="VAS")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_34 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Unlimited")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_35 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Voice")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_36 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Sms")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_54 <- OC %>% filter(Package.Price >150000 & Package.Price <=200000 & Category.Business =="Bonus/promotion")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    
    OC_37 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Validity + quota")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_38 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Roaming")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_39 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="VAS")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_40 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Unlimited")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_41 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Voice")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_42 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Sms")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_55 <- OC %>% filter(Package.Price >200000 & Package.Price <=400000 & Category.Business =="Bonus/promotion")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    
    OC_43 <- OC %>% filter(Package.Price >400000 & Category.Business =="Validity + quota")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_44 <- OC %>% filter(Package.Price >400000 & Category.Business =="Roaming")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_45 <- OC %>% filter(Package.Price >400000 & Category.Business =="VAS")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_46 <- OC %>% filter(Package.Price >400000 & Category.Business =="Unlimited")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_47 <- OC %>% filter(Package.Price >400000 & Category.Business =="Voice")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_48 <- OC %>% filter(Package.Price >400000 & Category.Business =="Sms")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
    OC_56 <- OC %>% filter(Package.Price >400000 & Category.Business =="Bonus/promotion")%>% select(c(Operator, Package.Name.Purchase,Package.customer.type, Package.Price, Package.Type.1, Package.Type.2,Package.Validity, Value.added.Service.VAS.., Category.Business, SOP))
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
    }else if(input$package_price >150000 & input$package_price <=200000 & input$category_business =="Unlimited"){
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
  
  output$table <- DT::renderDataTable({
    DT::datatable(datamatch()$b)
  })
  
  datafungsi <- reactive({
    if(#dim(b)[1]==0 #using dimension
      length(datamatch()$b[,1])==0){#using length
      print("There are No Similiar Competitor Package")
    }else{
      total_poin = 0;
      for(i in 1: length(datamatch()$b[,1])){
        temp_poin = 0
        #print(b[i,]$Package.Price)
        #print(length(b$Package.Price))
        if(input$package_price <= datamatch()$b[i,]$Package.Price){
          temp_poin = temp_poin+1
        }
        else{
          temp_poin = temp_poin+0
        }
        if(input$package_validity >= datamatch()$b[i,]$Package.Validity){
          temp_poin = temp_poin+1
        }
        if(input$vas=="Yes"){
          temp_poin = temp_poin +1
        }else if(input$vas=="No" & datamatch()$b[i,]$Value.added.Service.VAS..=="No"){
          temp_poin = temp_poin +1
        }
        if(input$SOP == datamatch()$b[i,]$SOP){
          temp_poin = temp_poin +1
        }else if(input$SOP == 6){
          temp_poin = temp_poin +1
        }else if(input$SOP == 3 & datamatch()$b[i,]$SOP ==2){
          temp_poin = temp_poin +1
        }else if(input$SOP == 2 & datamatch()$b[i,]$SOP ==1){
          temp_poin = temp_poin +1
        }else if(input$SOP == 3 & datamatch()$b[i,]$SOP ==1){
          temp_poin = temp_poin +1
        }else if(input$SOP != datamatch()$b[i,]$SOP & datamatch()$b[i,]$SOP !=6){
          temp_poin = temp_poin +1
        }
        if(temp_poin > 2){
          total_poin = total_poin +1
        }
        
      }
      
      tp <- paste0(input$package_name_detail," total point is ", total_poin, " out of ",length(datamatch()$b[,1]))
      
      
      prcnt <- (total_poin / length(datamatch()$b[,1]))*100
      if(total_poin / length(datamatch()$b[,1])>= 0.5){
        HTML(paste0(tp, br(), br(),"Fit to the market ",sprintf(prcnt, fmt = '%#.2f'),"%",br(), br(), "Estimated number of customer : Not Available"))
        #print(total_poin / length(datamatch()$b[,1])," %")
      }else if(total_poin/length(datamatch()$b[,1])<0.5){
        HTML(paste0(tp, br(), br(),"Not Fit to the market ",sprintf(prcnt, fmt = '%#.2f'),"%",br(), br(), "Estimated number of customer : Not Available"))
        #print(total_poin / length(datamatch()$b[,1])," %")
      }
      
    }
  })
  
  output$datafungsi <- renderText({
    print(datafungsi())
  })
}

shinyApp(ui, server)