  library(shinyjs)
  library(MASS)
  library(nnet)
  library(ranger)
  library(randomForest)
  library(mice)
  library(shinyWidgets)
  library(dplyr)
  library(shiny)
  library(purrr)
  library(stringr)
  library(brglm2)
  
  library(rsconnect) #Shiny app
  library(shinycssloaders)
  library(anytime) #for function anydate()
  
  library(shinydashboard)
  library(shinydashboardPlus)
  options(shiny.maxRequestSize=30*3000^2)
  
  library(missForest)
  
  ui <- fluidPage(
    
    #For the background color of thee Shiny App
    setBackgroundColor(
      color = c("#F7FBFF", "#2171B5"),
      #color = "#00BFFF",
      gradient = "linear",
      direction = "bottom"),
    
    
    #sidebarPanel(width = 5,
    sidebarLayout(
     wellPanel(
       
      style = "overflow-y: auto; position:fixed; width:600px; top:0; bottom:0",
       
      titlePanel(h1("All in One Data Cleaning App", style='background-color:#A0A0A0;padding-left: 15px')),
      
      prettyCheckboxGroup(inputId = "checkbox1",
                          label = "Select the below packages to be installed(FOR FIRST TIME RUN ONLY)",
                          choices = c("shinyWidgets", "mice", "missForest"),
                          icon = icon("check"),
                          shape = "round",
                          animation = "tada",
                          inline = TRUE), 
      
      #Giving user the input to select the dataset from their computer path
      fileInput("file1", "Choose CSV File for Data import",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      #Creating a Radiobutton for the type of Data Types Handling
      prettyRadioButtons(inputId="datatype", label="Identifying Column Data Types", 
                   choices=c("Baased on pre-defined data type structure file", "Based on number of unique values")),
      
      sliderInput("lev.thr", "Threshold for Number of Unique Levels",
                  min = 0, max = 100, value = 20
      ),
      
      #Giving user the input to select the dataset from their computer path
      fileInput("file2", "Choose Data Type Schema File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      numericInput("tgt", "Enter Target Variable Column Number", value = "0"),
      
      #Creating a Radiobutton for the type of Outlier Handling
      prettyRadioButtons(inputId="outliers", label="Handling Outliers", 
                   choices=c("Ignore","Show Outliers","Show Box Plots for Outlier Columns","Replace Outliers with NA's")),
      
      #Creating a Radiobutton for the type of Outlier Handling
      prettyRadioButtons(inputId="removecols", label="Threshold % limit for NA's in each column", 
                   choices=c("Ignore","Remove Columns")),
      
      sliderInput("nas", "Threshold (%) value for permitted NA's in each column",
                  min = 0, max = 100, value = 0
      ),
      
      #Creating a Radiobutton for the type of Correlation Handling
      prettyRadioButtons(inputId="cor", label="Correlation between Independent Variables", 
                   choices=c("Ignore","Check for variables having HIGH Correlation","Remove Highly Correlated Variable"), selected = "Ignore"),
      
      
      sliderInput("rem.cor", "Select Threshold(%) for Correlation coefficient(r)",
                  min = 0, max = 100, value = 0
      ),
      
      #Creating a Radiobutton for the P-values Signifcance Test
      prettyRadioButtons(inputId="pval", label="P-Value Significance Tests", 
                   choices=c("Ignore","Continuous Variables","Categorical Variables")),
      
      
      sliderInput("pval.thr", "Select Threshold for P-value",
                  min = 0, max = 0.1, value = 0, step = 0.05
      ),
      
      #Creating a Radiobutton for the type of BEST Missing Values Imputation
      prettyRadioButtons(inputId="choice", label="Best Missing Values Imputation Method", 
                   choices=c("Ignore","MissForest","Mice"), selected = "Ignore"),
      
      
      useShinyjs(),

      sliderInput("missforest_maxit", "Select Maximum Number of Iterations",
                  min = 0, max = 10, value = 0, step = 1
      ),
      
      sliderInput("missforest_ntree", "Select Number of Trees",
                  min = 0, max = 50, value = 0, step = 10
      ),
      
      sliderInput("mice_maxit", "Select Maximum Number of Iterations",
                  min = 0, max = 10, value = 0, step = 1
      ),
      
      sliderInput("mice_m", "Select Number of Imputed datasets",
                  min = 0, max = 5, value = 0, step = 1
      ),
      
      #Creating a Radiobutton for the type of Easy Missing Values Imputation
      prettyRadioButtons(inputId="easy.imp", label="Easiest Missing Values Imputation Method", 
                   choices=c("Ignore","Mean (Numerical) and Mode (Categorical)","Median (Numerical) and Mode (Categorical)")),
      
      #Creating a Radiobutton for the type of Imbalanced Target Variables Class Handling
      prettyRadioButtons(inputId="imbalance", label="Handling Imbalanced Target Variables Class", 
                   choices=c("Ignore","Check Target Variable Classes", "Handle Imbalanced data through SMOTE")), 
      
      sliderInput("perc.over", "Select Perc Over for SMOTE Input",
                  min = 0, max = 1000, value = 0, step = 100
      ),
      
      sliderInput("perc.under", "Select Perc Under for SMOTE Input",
                  min = 0, max = 1000, value = 0, step = 100
      ),
      
      #Creating a Radiobutton for the type of Standardising variables
      prettyRadioButtons(inputId="standardise", label="Standardize Numerical Variables", 
                   choices=c("Ignore","Min-Max Transformation")), 
      
      prettyRadioButtons(inputId="dummy", label="Convert Categorical Variables into dummy Variables", 
                   choices=c("Ignore","Convert")), 
      
      prettyRadioButtons(inputId="unique", label="Remove Columns having Unique Value i.e. SNO's, Primary Keys", 
                   choices=c("Ignore","Remove")), 
      
      prettyRadioButtons(inputId="onelevel", label="Remove Columns having just 1 Level", 
                   choices=c("Ignore","Remove")), 
      
      prettyRadioButtons(inputId="direction1", label="direction1", 
                         choices=c("Ignore","unq.one.easy")),
      
      prettyRadioButtons(inputId="direction2a", label="direction2", 
                         choices=c("Ignore","bestmiss")), 
      
      prettyRadioButtons(inputId="direction2b", label="direction2", 
                         choices=c("Ignore","bestmiss")), 
      
      prettyRadioButtons(inputId="direction3a", label="direction3", 
                         choices=c("Ignore","imbal")), 
      
      prettyRadioButtons(inputId="direction3b", label="direction3", 
                         choices=c("Ignore","imbal")),
      
      prettyRadioButtons(inputId="direction4a", label="direction4", 
                         choices=c("Ignore", "standard")), 
      
      prettyRadioButtons(inputId="direction4b", label="direction4", 
                         choices=c("Ignore", "standard")), 
      
      prettyRadioButtons(inputId="direction5a", label="direction5", 
                         choices=c("Ignore", "dummy")), 
      
      prettyRadioButtons(inputId="direction5b", label="direction5", 
                         choices=c("Ignore", "dummy")),
      
      prettyRadioButtons(inputId="download", label="download", 
                         choices=c("Ignore", "download")),
      
      actionButton("script", "Download the data")
      #downloadBttn("script", "Download the data")
      #downloadBttn("download1", "Download the data")
      
     ),    #Well Panel
    
    
    mainPanel(column(10, offset = 6,
      
      #verbatimTextOutput("text"), #%>% withSpinner(color="#0dc5c1"),
      #tableOutput("table"), #%>% withSpinner(color="#0dc5c1"),
      #plotOutput("dtplot", width = "749px", height="150px"), #%>% withSpinner(color="#0dc5c1") 
      #plotOutput("boxplot"),
      verbatimTextOutput("start"),
      verbatimTextOutput("download1"),
      verbatimTextOutput("download2"),
      verbatimTextOutput("bestmiss"),
      verbatimTextOutput("bestmiss1"),
      verbatimTextOutput("imbalance.start"),
      verbatimTextOutput("imbalance.end"),
      verbatimTextOutput("standard.start"),
      verbatimTextOutput("standard.end"),
      verbatimTextOutput("dummy.start"),
      verbatimTextOutput("dummy.end")
      #verbatimTextOutput("smote"),
      #verbatimTextOutput("smote.load"),
      #verbatimTextOutput("smote.load1"),
      #verbatimTextOutput("smote.load2")
     )
    ) #main
   )  
  )       #ui
  
  server <- function(input,output,session){ # 1{
   
    shinyjs::runjs('toggleCodePosition();')
#===========================================================================================================================================
#                           PART - 1 (BEGIN OF CODE WHEN DATATYPE SCHEMA FILE IS UPLOADED)  
#===========================================================================================================================================    
    observeEvent(input$datatype,  {   #2 - ({
      observeEvent(input$lev.thr, {   #3 - ({
      datatype <- input$datatype
      lev.thr <- input$lev.thr
      
      hide("direction1")
      hide("direction2a")
      hide("direction2b")
      hide("direction3a")
      hide("direction3b")
      hide("direction4a")
      hide("direction4b")
      hide("direction4c")
      hide("direction5a")
      hide("direction5b")
      hide("download")
      
      
      if (datatype == "Baased on pre-defined data type structure file"){  #4  - {  
        
        show("file2", TRUE)
        hide("lev.thr", TRUE)
        output$nout <- renderPrint({
          print(paste("Columns having number of unique values less than", lev.thr, "will be treated as FACTORS"))
        })
        
        #Begin of Code when Input dataset is uploaded 
        observeEvent(input$file1, { #5 - ({
          file1 <- read.csv(input$file1$datapath,stringsAsFactors = FALSE)
          assign('data',file1,envir=.GlobalEnv)
          
          library(gridExtra) #For grid.table() function
          
          dt.upload <- data.frame(matrix(NA, nrow = 5, ncol = 6))
          names(dt.upload)[1] <- "Column Name"
          names(dt.upload)[2] <- "Column Data Type"
          
          dt.upload[1,1] <- "SNO\t\t\t\t\t\t\t\t\t\t\t"
          dt.upload[1,2] <- "numeric\t\t\t\t\t\t\t\t\t\t\t"
          dt.upload[1,3] <- "\t\t\t\t\t\t\t\t\t\t\t"
          dt.upload[1,4] <- "\t\t\t\t\t\t\t\t\t\t\t"
          dt.upload[1,5] <- "\t\t\t\t\t\t\t\t\t\t\t"
          dt.upload[1,6] <- "\t\t\t\t\t\t\t\t\t\t\t"
          dt.upload[2,1] <- "Age"
          dt.upload[2,2] <- "numeric"
          dt.upload[2,3] <- " "
          dt.upload[2,4] <- " "
          dt.upload[2,5] <- " "
          dt.upload[2,6] <- " "
          dt.upload[3,1] <- "sex"
          dt.upload[3,2] <- "factor"
          dt.upload[3,3] <- " "
          dt.upload[3,4] <- " "
          dt.upload[3,5] <- " "
          dt.upload[3,6] <- " "
          dt.upload[4,1] <- "Size"
          dt.upload[4,2] <- "ordinal"
          dt.upload[4,3] <- "Small\t\t\t\t\t\t\t\t\t\t\t"
          dt.upload[4,4] <- "Medium"
          dt.upload[4,5] <- "Large"
          dt.upload[4,6] <- "Extra Large"
          dt.upload[5,1] <- "Ranking"
          dt.upload[5,2] <- "ordinal"
          dt.upload[5,3] <- "Bad "
          dt.upload[5,4] <- "Good"
          dt.upload[5,5] <- "Excellent"
          dt.upload[5,6] <- " "
          dt.upload[6,1] <- "Purchase Date"
          dt.upload[6,2] <- "Date"
          dt.upload[6,3] <- " "
          dt.upload[6,4] <- " "
          dt.upload[6,5] <- " "
          dt.upload[6,6] <- " "
          
          dt.upload1 <- dt.upload
          
          output$text <- renderPrint({
            cat("NOTE :\n")
            cat("1. Upload .csv Data Type Schema File in the below format - \n")
            cat("2. For ordinal variables, mention the levels in increasing order of their importance - Starting from Column 3\n")
          })
          output$dtplot <- renderPlot({
            grid.table(dt.upload)
          })
            
            
          observeEvent(input$tgt, { #6 - ({ #Begin of Code when Target variable Column Number is entered
            
            tgt.no   <- input$tgt
            tgt.name <- colnames(data)[tgt.no]
            
            assign('target.no',tgt.no,envir=.GlobalEnv)
            assign('tgt.name',tgt.name,envir=.GlobalEnv)  
            
            observeEvent(input$file2, {  #7 - ({
              
              file2 <- read.csv(input$file2$datapath,stringsAsFactors = FALSE)
              assign('dt',file2,envir=.GlobalEnv)
              
              #Starting
              num  <- which(dt[,2] == "numerical")
              fact <- which(dt[,2] == "factor")
              ord <- which(dt[,2] == "ordinal")
              
              #Separated the different data types into different dataframes
              num.data <- data[,num]     #Numerical variables dataframe
              cat.data <- data[,fact]    #Categorical vaariables dataframe
              ord.data <- data[,ord]     #Ordinal variables dataframe
              
              #Converting the num.data variables into numeric
              num.data[,] <- lapply(num.data[,], as.numeric)
              #Converting the cat.data variables into categorical
              cat.data[,] <- lapply(cat.data[,], as.factor)
              
              #Giving a RANK to Categorical variables to make it Ordinal
              ord.rows <- which(dt[,2] == "ordinal")
              ord.dt <- dt[ord.rows,]
              
              ord.data <- as.data.frame(ord.data)
              
              #Converting the attributes of Survey Questionnaires to ordinal variables
              if (length(ord.rows) != 0){
                
                for (i in 1:ncol(ord.data)){
                  
                  l <- ord.dt[i,3:length(ord.dt[i,])]
                  l <- as.list(l)
                  l <- unlist(l)
                  l <- unname(l)
                  
                  ord.data[,i] <- factor(ord.data[,i], ordered = TRUE, levels = c(l))
                  
                }#end for
              }#end if
              
              #Combine all the above 3 datasets into one dataset
              if (length(ord.rows) != 0){
                data.comb <- cbind(num.data,ord.data,cat.data)
              }
              
              if (length(ord.rows) == 0){
                data.comb <- cbind(num.data,cat.data)
              }
              
              data.comb <- as.data.frame(data.comb)
              
              assign('data.comb',data.comb,envir=.GlobalEnv)
              
              output$text <- renderPrint({
                return(str(data.comb))
              })
        
            })  #7
          })    #6
        })      #5
      }         #4

#===========================================================================================================================================
#                           PART - 1 (END OF CODE WHEN DATATYPE SCHEMA FILE IS UPLOADED)  
#===========================================================================================================================================
      
#============================================================================================================================================
#                           PART - 2 (BEGIN OF CODE WHEN DATATYPE IS BASED ON UNIQUE LEVELS THRESHOLD)
#============================================================================================================================================    
      
      if (datatype == "Based on number of unique values"){     #8 - {
        
        hide("file2", TRUE)
        show("lev.thr", TRUE)
        
        observeEvent(input$file1, {  #9  - ({
          file1 <- read.csv(input$file1$datapath,stringsAsFactors = FALSE)
          assign('data',file1,envir=.GlobalEnv)
          
          perc.over.new <- 0
          perc.under.new <- 0
           
          assign('perc.over.new',  perc.over.new, envir=.GlobalEnv)
          assign('perc.under.new', perc.under.new,envir=.GlobalEnv)
          
          observeEvent(input$tgt, {  #10  - ({ 
            
            tgt.no   <- input$tgt
            tgt.name <- colnames(data)[tgt.no]
            
            assign('tgt.no',tgt.no,envir=.GlobalEnv)
            assign('tgt.name',tgt.name,envir=.GlobalEnv)
            
           # if (is.null(input$file2)){     #11 - {         
              #Start of column Data Type code
              
              print("file 2 not selected")
              data.comb <- data
              data.comb <- as.data.frame(data.comb)
              
              date <- data.frame(data.comb)
              datecheck <- sapply(date, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))
              
              n <- 0
              dc <- data.frame(matrix(NA, nrow = 2, ncol = 2))
              dc1 <- 0
              date.colnames <- c()
              for (i in 1:length(datecheck)){
                if(datecheck[i] == "TRUE"){
                  data.comb[,i] <- anydate(data.comb[,i])  #Converts date into yyyy-mm-dd format
                  data.comb[,i] <- as.Date(data.comb[,i])  #Converts the date column into date data type
                  n <- n + 1
                  date.colnames[n] <- colnames(data.comb[i])
                }
              }
              assign('date.colnames',date.colnames,envir=.GlobalEnv)
              
              withProgress(message = 'Making plot', value = 0, {  
              #Converts the columns into their corresponding datatypes
              for (i in 1:ncol(data.comb)){
                uq.ln <- length(unique(data.comb[,i]))
                if (uq.ln > 20){  
                  if (class(data.comb[,i]) == "integer" || class(data.comb[,i]) == "numeric" || class(data.comb[,i]) == "double"){  
                    data.comb[,i] <- as.numeric(data.comb[,i]) 
                  }
                  if (class(data.comb[,i]) == "character" || class(data.comb[,i]) == "factor"){
                    data.comb[,i] <- as.factor(data.comb[,i]) 
                  }
                }
                #If number of levels are LESS THAN OR EQUAL TO 20, treat all of them as FACTORS
                if (uq.ln > 1 && uq.ln <= 20){
                  if (class(data.comb[,i]) == "double"){
                    data.comb[,i] <- as.numeric(data.comb[,i])   
                  } else {
                    data.comb[,i] <- as.factor(data.comb[,i]) 
                  }
                }
                # Increment the progress bar, and update the detail text.
                incProgress(1/ncol(data.comb), detail = paste("Doing part", i))
              }
            }) #end withProgress 
              assign('data.comb',data.comb,envir=.GlobalEnv)
              
              output$text <- renderPrint({
                return(str(data.comb))
              })
              #End of column Data Type code
              
         })   #10 - })
        })    #9  - })
       }      #8  - }  
      })      #3  - })
     })       #2  - })
      
    
#============================================================================================================================================
#                           PART - 2 (END OF CODE WHEN DATATYPE IS BASED ON UNIQUE LEVELS THRESHOLD)
#============================================================================================================================================    

  #START OF OUTLIERS
  observeEvent(input$outliers,{ #11 - ({
    req(input$outliers)
    show.out <- input$outliers
    assign('show.out',show.out,envir=.GlobalEnv)
    
    
    if (show.out == "Show Outliers"){ #12 - {
      show("text")
      hide("boxplot")
      hide("dtplot")
      
      n <- 1
      n1 <- 1
      n2 <- 0
      out.string <- c()
      out.colnames <- c()
      out.string[n1] <- paste("Below are the details of Outliers - ")
      n1 <- n1 + 1
      out.string[n1] <- paste(" ")

      for (col in 1:ncol(data.comb)){
        
        n <- n + 1

        if (class(data.comb[,col]) == "numeric"){
        
          
          #boxplot.stats() uses the Turkey method which is used to identify the outliers ranged above and below 1.5 * IQR 
          outliers <- boxplot.stats(data.comb[,col])$out
          
          if (length(outliers != 0)){
            n1 <- n1 + 1
            out.string[n1] <- paste("Column No. : ", n, "Column Name :",colnames(data.comb[col]))
            #n1 <- n1 + 1
            #out.string[n1] <- paste("")
            n1 <- n1 + 1
            out.string[n1] <- paste("No. of Outliers in column", col, "is -", length(outliers))
            n2 <- n2 + 1
            out.colnames[n2] <- colnames(data.comb[col])
          } #end 2nd if
        }     #end 1st if
      }           #end for loop
      
      out.string <- as.data.frame(out.string)
      names(out.string) <- NULL
      
      output$text <- renderPrint({
        print(out.string, row.names = FALSE)
      }) #end renderPrint
    }  #12 - } -> end if show.out == "Show Outliers"
    
    if (show.out == "Ignore"){ #13 - {
      #hide("text")
      #hide("boxplot")
    }    #13 - } - end if "Ignore
    
#===========================================================================================================================================
#                                 START OF SHOW BOX PLOTS FOR OUTLIERS (With Schema datatype File)
#===========================================================================================================================================
    
    if (show.out == "Show Box Plots for Outlier Columns"){ #14 - {
      show("text")
      show("boxplot")
      hide("dtplot")
      
      n <- 1
      n1 <- 1
      n2 <- 0
      out.string <- c()
      out.colnames <- c()
      out.string[n1] <- paste("Below are the details of Outliers - ")
      n1 <- n1 + 1
      out.string[n1] <- paste(" ")
      
     # withProgress(message = 'Making plot', value = 0, { #start of progress indicator
        
      for (col in 1:ncol(data.comb)){
        
        n <- n + 1
        #FINDING THE OUTLIERS AND REPLACING THEM WITH NA's
        if (class(data.comb[,col]) == "numeric"){
          #boxplot.stats() uses the Turkey method which is used to identify the outliers ranged above and below 1.5 * IQR 
          outliers <- boxplot.stats(data.comb[,col])$out
          if (length(outliers != 0)){
            n1 <- n1 + 1
            out.string[n1] <- paste("Column No. : ", n, "Column Name :",colnames(data.comb[col]))
            #n1 <- n1 + 1
            #out.string[n1] <- paste("")
            n1 <- n1 + 1
            out.string[n1] <- paste("No. of Outliers in column", col, "is -", length(outliers))
            n2 <- n2 + 1
            out.colnames[n2] <- colnames(data.comb[col])
          }#end 2nd if
        }#end 1st if
        #Increment the progress bar, and update the detail text.
        #incProgress(1/ncol(data.comb), detail = paste("Doing part", col))  
      }#end for loop
    #})#end withProgress indicator 
      o.cn <- 0
      out.colno <- c()
      for (i in 1:length(out.colnames)){
        o.cn <- o.cn + 1
        out.colno[o.cn] <- which(colnames(data) == out.colnames[i])
      }
      
      output$boxplot <- renderPlot({
        ln.out <- length(out.colno)
        if (ln.out > 1){
          a <- ln.out/2
          b <- ln.out
          par(mfrow = c(a, b))
        }
        for (i in 1:length(out.colno)){
          #dev.off()
          #graphics.off()
          boxplot(data[,out.colno[i]], xlab = out.colnames[i])
        }
      }) #end renderPlot
    }  #14 - } -> end if show.out == "Show Box Plots"
    
    #===========================================================================================================================================          
    #                                 END OF SHOW BOX PLOTS FOR OUTLIERS (With Schema datatype File)
    #===========================================================================================================================================          
    
    #===========================================================================================================================================          
    #                                 START OF REPLACE OUTLIERS WITH NA's (With Schema datatype File)
    #===========================================================================================================================================          
    
    if (show.out == "Replace Outliers with NA's"){ #15 - {
      hide("dtplot")
      hide("text")
      hide("boxplot")
      
      for (col in 1:ncol(data.comb)){
        #FINDING THE OUTLIERS AND REPLACING THEM WITH NA's
        if (class(data.comb[,col]) == "numeric"){
          #boxplot.stats() uses the Turkey method which is used to identify the outliers ranged above and below 1.5 * IQR 
          outliers <- boxplot.stats(data.comb[,col])$out
          if (length(outliers) != 0){
            data.comb[,col] <- ifelse(data.comb[,col]%in%outliers, NA, data.comb[,col])
          }    #end of 1st if
        }        #end of 2nd if
      }             #end for loop 
    }                  #15 - } -> end of show.out == "Replace Outliers with NA's
    
    observeEvent(input$removecols, {
      observeEvent(input$nas, {
        na  <- input$removecols
        nas <- input$nas
        if (na == "Ignore"){
          hide(id = "nas", TRUE)    
        }  
        
        if (na == "Remove Columns"){
          show(id = "nas", TRUE)
          
        if (nas != 0){
          
          nas.thr.perc <- nas/100
          nas.col <- c()
          n <- 0
          
          for (col in 1:ncol(data.comb)){
             #Fetching the column numbers having more than 70% Missing data
             sum <- sum(is.na(data.comb[col]))
            if (sum > (nas.thr.perc * nrow(data.comb))){
               n <- n + 1
               nas.col[n] <- colnames(data.comb[col])   
            }  #end if
           }     #end for
           assign('nas.col',nas.col,envir=.GlobalEnv)
          }        #end if(nas!=0)
         }           #end remove columns if
       })              #end Observeevent - input$nas  
      })                 #end Observeevent - input$removecols   
    
    
#===========================================================================================================================================    
#                                               START OF CORRELATION CODE
#===========================================================================================================================================    
    
    observeEvent(input$rem.cor, {  
      req(input$rem.cor)
      rem.cor <- input$rem.cor
      rem.cor <- as.numeric(rem.cor)
      observeEvent(input$cor, {
        req(input$cor)
        c <- input$cor
        if (c == "Ignore"){
          hide("rem.cor", TRUE)
        }
        if (c == "Check for variables having HIGH Correlation"){
          show("rem.cor", TRUE)
      
          if (rem.cor != 0){
            #Correlations Code
            cor <- cor(data.comb[sapply(data.comb, is.numeric)])
            cor2 <- cor
            
            cor.no <- 0
            n1 <- 0
            correlations <- data.frame(matrix(NA, nrow = nrow(cor2), ncol = 2))
            a <- 1
            
            corr.thr <- rem.cor/100
            for (i in 1:(nrow(cor2)-1)){
              a  <- a + 1
              a1 <- a - 1
              for (b in 1:a1){
                if (abs(cor2[a,b]) > corr.thr){
                  n1 <- n1 + 1
                  correlations[n1,1] <- colnames(data.comb[a]) 
                  correlations[n1,2] <- colnames(data.comb[b])
                  correlations[n1,3] <- cor2[a,b]
                }
              }
            }
            
            final.corr <- correlations[complete.cases(correlations),]
            
      
            if (length(final.corr[[1]]) == 0){
              output$text <- renderPrint({
                print("No variables are correlated for this threshold value")
              })
            }
            
            if (length(final.corr[[1]]) != 0){
              
              names(final.corr)[1] <- "Column 1"
              names(final.corr)[2] <- "Column 2"
              names(final.corr)[3] <- "Correlation Coefficient value"
              
              output$nout <- renderPrint({
                print(paste("Below are the variables having Highest Correlation for threshold value", corr.thr))
              })
              output$table <- renderTable({
                return(head(final.corr))
              }) 
              
              tgt.no <- as.numeric(tgt.no)
              target.nm <- colnames(data)[tgt.no]
              target.no <- which(colnames(data.comb) == target.nm)
              
              lm.p <- data.frame(matrix(NA, nrow = nrow(final.corr), ncol = 2))
              
              for (i in 1:nrow(final.corr)) {
                
                corr.no   <- which(colnames(data.comb) == final.corr[i,1]) 
                lm1       <- glm(data.comb[,target.no] ~ data.comb[,corr.no], data = data.comb, family = binomial)
                sum.lm1   <- summary(lm1)
                lm.p1     <- sum.lm1$coefficients[8]
                
                corr.no   <- which(colnames(data.comb) == final.corr[i,2]) 
                lm2       <- glm(data.comb[,target.no] ~ data.comb[,corr.no], data = data.comb, family = binomial)
                sum.lm2   <- summary(lm2)
                lm.p2     <- sum.lm2$coefficients[8] 
                
                if (lm.p1 < lm.p2){
                  lm.p[i,1] <- final.corr[i,1]
                  lm.p[i,2] <- lm.p1
                }
                if (lm.p2 < lm.p1){
                  lm.p[i,1] <- final.corr[i,2] 
                  lm.p[i,2] <- lm.p2
                }
                
              }
              assign('lm.p',lm.p,envir=.GlobalEnv)
            }
          }  
        }
        if (c == "Remove one variable among the HIGHLY correlated pairs obtained above based on P-value Significance test"){
          hide("rem.cor",TRUE)
          if (exists("lm.p") == TRUE){
            cor.rem <- lm.p
            assign('cor.rem',cor.rem,envir=.GlobalEnv)
          }  
        }
      })  
    })     

#===========================================================================================================================================    
#                                               END OF CORRELATION CODE
#===========================================================================================================================================    

    
    
#===========================================================================================================================================    
#                                           START OF P-VALUE SIGNIFICANCE CODE
#===========================================================================================================================================    
    

    observeEvent(input$pval, { 
      observeEvent(input$pval.thr, {
        req(input$pval)
        req(input$pval.thr)
        pval <- input$pval
        pval.thr <- input$pval.thr
        pval.thr <- as.numeric(pval.thr)
        #print(input$pval)
        if (pval == "Ignore"){
          hide("pval.thr", TRUE)
        }
        if (pval == "Continuous Variables"){
          show("pval.thr", TRUE)
          
          if (pval.thr != 0){
          #Start of P-value test for Continuous Variables
          n <- 0
          glm.op <- 0
          glm.pval <- 0
          glm.num <- data.frame(matrix(NA, nrow = 2, ncol = 2))
          names(glm.num)[1] <- "Column Names"
          names(glm.num)[2] <- "P-Values"
          
          for (i in 1:ncol(data.comb)){
            if (class(data.comb[,i]) == "numeric"){
              glm.op       <- glm(data.comb[,target.no] ~ data.comb[,i], data = data.comb, family = binomial)
              glm.sum      <- summary(glm.op)
              glm.pval <- glm.sum$coefficients[8]
              
              if (glm.pval > pval.thr){
                n <- n + 1
                glm.num[n,1] <- colnames(data.comb[i])
                glm.num[n,2] <- glm.pval
              }
            }
          }
          
          final.glm <- glm.num[complete.cases(glm.num),]
          final.glm <- as.data.frame(final.glm)
          
          output$table <- renderTable({
            return(head(final.glm))
          }) 
          
          num.p.rem <- final.glm[,1]
          assign('num.p.rem',num.p.rem,envir=.GlobalEnv)
          
          }#end of if(pval.thr != 0)
          #End of P-value test for Continuous Variables
        }
        
        if (pval == "Categorical Variables"){
          show("pval.thr", TRUE)
          
          if (pval.thr != 0){
          #Chi-Square Significance Test
          chi.coln <- c()
          chi.val <- c()
          n <- 0
          
          chisq <- data.frame(matrix(NA, nrow = 2, ncol = 2))
          names(chisq)[1] <- "Column Names"
          names(chisq)[2] <- "P-Values"
          ordf <- c("ordered", "factor")
          for (i in 1:ncol(data.comb)){
            if (class(data.comb[,i]) == ordf || class(data.comb[,i]) == "factor"){
              chi <- chisq.test(data.comb[,target.no],data.comb[,i], correct = FALSE)
              if (chi[3] > pval.thr){
                n <- n + 1
                chisq[n,1] <- colnames(data.comb[i])
                chisq[n,2]  <- chi[3]
              }
            }
          }
          
          final.chisq <- chisq[complete.cases(chisq),]
          output$table <- renderTable({
            return(head(final.chisq))
          }) 
          
          cat.p.rem <- final.chisq[,1]
          assign('cat.p.rem',cat.p.rem,envir=.GlobalEnv)
          #End of ChiSquare Test
          }  
        }

      })
    })    
    
    
#===========================================================================================================================================    
#                                            END OF P-VALUE SIGNIFICANCE CODE
#===========================================================================================================================================    


#============================================================================================================================================
#                                   START OF CODE FOR BEST MISSING VALUES IMPUTATION METHOD
#============================================================================================================================================    
    
    
    observeEvent(input$mice_maxit, {
      observeEvent(input$mice_m, {
        observeEvent(input$missforest_maxit, {
          observeEvent(input$missforest_ntree, {
            observeEvent(input$choice, {
              x <- input$choice
              
              if(x == "Ignore") {
                hide("mice_maxit", TRUE)
                hide("mice_m", TRUE) 
                hide("missforest_maxit", TRUE)
                hide("missforest_ntree", TRUE)
                missing <- 0
                bestimp <- 0
                assign('missing',missing,envir=.GlobalEnv)
                assign('bestimp',bestimp,envir=.GlobalEnv)
              }
              
              if(x == "MissForest" && x != "Mice") {
                hide("mice_maxit")
                hide("mice_m")
                show(id = "missforest_maxit", TRUE)
                show(id = "missforest_ntree", TRUE) 
                
                missforest_maxit <- input$missforest_maxit
                missforest_ntree <- input$missforest_ntree
                bestimp          <- "missforest"
                mice_maxit       <- 0
                mice_m           <- 0
                missing          <- 1
                
                assign('missing',missing,envir=.GlobalEnv)
                assign('bestimp',bestimp,envir=.GlobalEnv)
                assign('missforest_maxit',missforest_maxit,envir=.GlobalEnv)
                assign('missforest_ntree',missforest_ntree,envir=.GlobalEnv)
                assign('mice_maxit',mice_maxit,envir=.GlobalEnv)
                assign('mice_m',mice_m,envir=.GlobalEnv)
              }  
              
              if(x == "Mice" && x != "MissForest") {
                hide("missforest_maxit")
                hide("missforest_ntree")
                show(id = "mice_maxit",TRUE)
                show(id = "mice_m",TRUE) 
                
                mice_maxit       <- input$mice_maxit
                mice_m           <- input$mice_m
                bestimp          <- "mice"
                missforest_maxit <- 0
                missforest_ntree <- 0
                missing          <- 1
                
                assign('missing',missing,envir=.GlobalEnv)
                assign('mice_maxit',mice_maxit,envir=.GlobalEnv)
                assign('mice_m',mice_m,envir=.GlobalEnv)
                assign('missforest_maxit',missforest_maxit,envir=.GlobalEnv)
                assign('missforest_ntree',missforest_ntree,envir=.GlobalEnv)
                
              }
              
              
            }) #observeEvent(input$choice, {  
          })    #observeEvent(input$missforest_ntree, {
        })       #observeEvent(input$missforest_maxit, {
      })          #observeEvent(input$mice_m, {
    })             #observeEvent(input$mice_maxit, {
    
    
#============================================================================================================================================
#                                 END OF CODE FOR BEST MISSING VALUES IMPUTATION METHOD
#============================================================================================================================================    
    

#============================================================================================================================================
#                                 START OF CODE FOR EASIEST MISSING VALUES IMPUTATION METHOD
#============================================================================================================================================    

  observeEvent(input$easy.imp,{
    easy.imp <- input$easy.imp
    
    if (easy.imp == "Ignore"){
      missing          <- 0
      easyimp          <- 0
      assign('missing',missing,envir=.GlobalEnv)
      assign('easyimp',easyimp,envir=.GlobalEnv)
    }
    
    if (easy.imp == "Mean (Numerical) and Mode (Categorical)"){
      
      easyimp          <- "mean"
      mice_maxit       <- 0
      mice_m           <- 0
      missforest_maxit <- 0
      missforest_ntree <- 0
      missing          <- 2
      
      assign('missing',missing,envir=.GlobalEnv)
      assign('easyimp',easyimp,envir=.GlobalEnv)
      assign('mice_maxit',mice_maxit,envir=.GlobalEnv)
      assign('mice_m',mice_m,envir=.GlobalEnv)
      assign('missforest_maxit',missforest_maxit,envir=.GlobalEnv)
      assign('missforest_ntree',missforest_ntree,envir=.GlobalEnv)
    }
    
    if (easy.imp == "Median (Numerical) and Mode (Categorical)"){
      
      easyimp          <- "median"
      mice_maxit       <- 0
      mice_m           <- 0
      missforest_maxit <- 0
      missforest_ntree <- 0
      missing          <- 2
      
      assign('missing',missing,envir=.GlobalEnv)
      assign('easyimp',easyimp,envir=.GlobalEnv)
      assign('mice_maxit',mice_maxit,envir=.GlobalEnv)
      assign('mice_m',mice_m,envir=.GlobalEnv)
      assign('missforest_maxit',missforest_maxit,envir=.GlobalEnv)  
    }  
    
  })    
    
    
#============================================================================================================================================
#                                 END OF CODE FOR EASIEST MISSING VALUES IMPUTATION METHOD
#============================================================================================================================================    

    
#============================================================================================================================================
#                                 START OF CODE FOR HANDLING IMBALANCED CLASSES
#============================================================================================================================================    
#SMOTE PACKAGE- DMwR    
    observeEvent(input$imbalance,  {
      observeEvent(input$perc.over,  {
        observeEvent(input$perc.under, { 
          
          req(input$imbalance)
          imbalance <- input$imbalance
            
          
            if (imbalance == "Ignore"){
              imbalance  <- 0  
              perc.over  <- 0
              perc.under <- 0
              hide("perc.over")
              hide("perc.under")
              #hide("smote")
              
              assign('imbalance',  imbalance,  envir=.GlobalEnv)
              assign('perc.over',  perc.over,  envir=.GlobalEnv)
              assign('perc.under', perc.under, envir=.GlobalEnv)
            }
      
            if (imbalance == "Check Target Variable Classes"){
              
              imbalance  <- 0  
              perc.over  <- 0
              perc.under <- 0
              
              hide("perc.over")
              hide("perc.under")
              #show("smote")
              
              assign('imbalance',  imbalance,  envir=.GlobalEnv)
              assign('perc.over',  perc.over,  envir=.GlobalEnv)
              assign('perc.under', perc.under, envir=.GlobalEnv)
              
              tgt.lev <- table(data.comb[,target.no])
              output$smote <- renderPrint({
                cat("Target Variable Classes are -")
                print(tgt.lev)
              })
              
            }
      
            if (imbalance == "Handle Imbalanced data through SMOTE"){
              
              
              show("perc.over")
              show("perc.under")
              
              perc.over  <- input$perc.over
              perc.under <- input$perc.under
              
              perc.over  <- as.numeric(perc.over)
              perc.under <- as.numeric(perc.under)
              
              Sys.sleep(1)
              
              if (perc.over == 0 || perc.under == 0){
                #hide("smote.load1")
                #output$smote.load <- renderPrint({
                #  cat("Please provide the inputs for SMOTE -")
                #})
              }
              
              
              if (perc.over != 0 && perc.under != 0){ 
            
              
              if (perc.over != perc.over.new || perc.under != perc.under.new) {  
          
                  
                print(paste("perc over      - ", perc.over))
                print(paste("perc under     - ", perc.under))
                print(paste("perc over  new - ", perc.over.new))
                print(paste("perc under new - ", perc.under.new))
                
                imbalance <- 1
                
                assign('imbalance',  imbalance,  envir=.GlobalEnv)
                assign('perc.over',  perc.over,  envir=.GlobalEnv)
                assign('perc.under', perc.under, envir=.GlobalEnv)
                print(paste("imbalance", imbalance))
               
                
                } #end if (perc.over != perc.over.new || perc.under != perc.under.new) {  
               }    #end if (perc.over != 0 && perc.under != 0){  

        
              if (imbalance == "1"){
                
                withProgress(message = 'Making plot', value = 0, { #start of progress indicator
                  
                  #Increment the progress bar, and update the detail text.
                  incProgress(1/1, detail = paste("Doing part", 1))
                  
                }) #end of progress indicator
                
                  #output$smote <- renderText({
                  #  print("Loading Output")
                  #})
               
                
                print(imbalance)
                date.colno <- c()
                n <- 0
                
                for (i in 1:length(date.colnames)){
                  n <- n + 1
                  date.colno[n] <- which(colnames(data.comb) == date.colnames[i])
                  col <- date.colno[n]
                  data.comb[,col] <- as.factor(data.comb[,col])
                }
                
                target.no <- which(colnames(data.comb) == tgt.name)
                ll        <- levels(data.comb[,target.no])
                ll1       <- ll[1]
                
                data.comb <- mutate(data.comb,newtarget = ifelse(data.comb[,target.no]==ll1,0,1))
                newtgt.no <- which(colnames(data.comb) == "newtarget")
                data.comb[,target.no] <- as.factor(data.comb[,target.no])
                data.comb[,newtgt.no] <- as.factor(data.comb[,newtgt.no])
                
                data.comb <- SMOTE(newtarget ~ ., data.comb, perc.over = perc.over, perc.under = perc.under)
                
                for (i in 1:length(date.colno)){
                  col <- date.colno[i]
                  data.comb[,col] <- as.Date(data.comb[,col])
                  data.comb[,col] <- as.Date(data.comb[,col])
                }
                
                
                newtgt.no <- which(colnames(data.comb) == "newtarget")
                data.comb <- data.comb[,-newtgt.no]
                
                
                tgt.lev <- table(data.comb[,target.no])
                print(tgt.lev)
                
                
                #output$smote.load <- renderPrint({
                #  cat("New Target Variable Classes are -")
                #  print(tgt.lev)
                  
                #})
                
                perc.over.new  <- perc.over
                perc.under.new <- perc.under
                
                assign('perc.over.new',  perc.over.new,  envir=.GlobalEnv)
                assign('perc.under.new', perc.under.new,  envir=.GlobalEnv)
                
            }  #end if (imbalance == "1"){
          }      #end if (imbalance == "Handle Imbalanced data through SMOTE"){      
        })          #observeEvent(input$perc.over,  {
      })              #observeEvent(input$perc.under, {               
    })                  #end of observeEvent(input$imbalance,  {
    
#============================================================================================================================================
#                                 END OF CODE FOR HANDLING IMBALANCED CLASSES
#============================================================================================================================================    


#============================================================================================================================================
#                                 START OF CODE FOR STANDARDISE NUMERICAL VARIABLES
#============================================================================================================================================    
    
    observeEvent(input$standardise, {
      standardise <- input$standardise
      
      if (input$standardise == "Ignore"){
        standardise <- 0  
        assign('standardise.n',  standardise,  envir=.GlobalEnv)
      }
      
      if (input$standardise == "Min-Max Transformation"){
        standardise <- 1
        assign('standardise.n',  standardise,  envir=.GlobalEnv)
      }
      
    })
    
    
#============================================================================================================================================
#                                 END OF CODE FOR STANDARDISE NUMERICAL VARIABLES
#============================================================================================================================================    

    
#============================================================================================================================================
#                          START OF CODE FOR CONVERTING CATEGORICAL VARIABLES TO DUMMY VARIABLES
#============================================================================================================================================    
    
    observeEvent(input$dummy, {
      dummy <- input$dummy
      
      if (input$dummy == "Ignore"){
        dummy <- 0
        assign('dummy.c',  dummy,  envir=.GlobalEnv)  
      }
      
      if (input$dummy == "Convert"){
        dummy <- 1
        assign('dummy.c',  dummy,  envir=.GlobalEnv)
      }
      
    })
    
    
#============================================================================================================================================
#                             END OF CODE FOR CONVERTING CATEGORICAL VARIABLES TO DUMMY VARIABLES
#===========================================================================================================================================    
        

#============================================================================================================================================
#                            START OF CODE FOR REMOVING COLUMNS HAVING ALL UNIQUE VALUES
#============================================================================================================================================    
    
    observeEvent(input$unique, {
      unique <- input$unique
      
      if (input$unique == "Ignore"){
        unique <- 0
        assign('unique.v',  unique,  envir=.GlobalEnv)
      }
      
      if (input$unique == "Remove"){
        unique <- 1
        assign('unique.v',  unique,  envir=.GlobalEnv)
      }
      
    })
    
    
#============================================================================================================================================
#                             END OF CODE FOR REMOVING COLUMNS HAVING ALL UNIQUE VALUES
#===========================================================================================================================================    
    
    
#============================================================================================================================================
#                              START OF CODE FOR COLUMNS HAVING ONLY 1 LEVEL
#============================================================================================================================================    
    
    observeEvent(input$onelevel, {
      onelevel <- input$onelevel
      
      if (input$onelevel == "Ignore"){
        onelevel <- 0
        assign('onelevel',  onelevel,  envir=.GlobalEnv)
      }
      
      if (input$onelevel == "Remove"){
        onelevel <- 1
        assign('onelevel',  onelevel,  envir=.GlobalEnv)
      }
      
    })
    
    
#============================================================================================================================================
#                               END OF CODE FOR COLUMNS HAVING ONLY 1 LEVEL
#===========================================================================================================================================    

        
              
}) #11 - }) -> END OF OUTLIERS
  
#============================================================================================================================================
#                                         START OF CODE FOR DOWNLOAD BUTTON
#============================================================================================================================================    
 
observeEvent(input$script,{
  
  output$start <- renderPrint({
    cat("Process has started.. Might take some time.. But dont worry We will keep you updated. :) ")
  })
  
  
  hide("start")
  hide("download1")
  hide("download2")
  hide("bestmiss")
  hide("bestmiss1")
  hide("imbalance.start")
  hide("imbalance.end")
  hide("standard.start")
  hide("standard.end")
  hide("dummy.start")
  hide("dummy.end")
  
  updateSelectInput(session, "direction1",  selected = "unq.one.easy")
  updateSelectInput(session, "direction2a", selected = "Ignore")
  updateSelectInput(session, "direction2b", selected = "Ignore")
  updateSelectInput(session, "direction3a", selected = "Ignore")
  updateSelectInput(session, "direction3b", selected = "Ignore")
  updateSelectInput(session, "direction4a", selected = "Ignore")
  updateSelectInput(session, "direction4b", selected = "Ignore")
  updateSelectInput(session, "direction5a", selected = "Ignore")
  updateSelectInput(session, "direction5b", selected = "Ignore")
  updateSelectInput(session, "download"   , selected = "Ignore")
  
  #if (missing == "1"){  
  #  updateSelectInput(session, "direction2", selected = "bestmiss1")  
  #}  
  
})#end observeEvent(input$script,{  
    
#============================================================================================================================================
#                         START OF CODE FOR HANDLING UNIQUE VALUES, ONE LEVEL AND EASY MISSING VALUES METHODS
#============================================================================================================================================    

observeEvent(input$direction1,{
  
if (exists("data.comb") == "TRUE"){    
  
if (unique.v != "1" && onelevel != "1" && missing != "2"){ 
  updateSelectInput(session, "direction2a", selected = "bestmiss")    
} 
  
if (unique.v == "1" || onelevel == "1" || missing == "2"){            
    
  if (exists("data.comb") == "TRUE"){
    n1      <- 0
    n2      <- 0
    miss.n  <- 0
    miss.c  <- 0
    miss.vl <- 0
    unq     <- c()
    onelvl  <- c()
    
    #Mode function for imputation of Catgeorical Missing Values
    Mode <- function (x, na.rm) {
      xtab <- table(x)
      xmode <- names(which(xtab == max(xtab)))
      if (length(xmode) > 1) xmode <- ">1 mode"
      return(xmode)
    }
    
    for (i in 1:ncol(data.comb)){
      
      if (unique.v == "1"){
        len.unq <- length(unique(data.comb[,i]))  
        if (len.unq == nrow(data.comb)){
          n1      <- n1 + 1
          unq[n1] <- i
        }
      }
      
      if (onelevel == "1"){
        len.lev <- length(levels(data.comb[,i]))
        if (len.lev == "1"){
          n2          <- n2 + 1
          onelvl[n2]  <- i
        }
      }
      
      
      if (class(data.comb[,i]) == "numeric"){
        any.na <- any(is.na(data.comb[,i]))
        if(any.na == "TRUE"){
          miss.n <- miss.n + 1
          if (easyimp == "mean"){
            #Imputing the NA's with the Mean Values
            data.comb[is.na(data.comb[,i]),i] = mean(data.comb[,i], na.rm=TRUE)
          }
          if (easyimp == "median"){
            #Imputing the NA's with the Median Values
            data.comb[is.na(data.comb[,i]),i] = median(data.comb[,i], na.rm=TRUE)
          }
        }
      }
      
      if (class(data.comb[,i]) == "factor"){
        any.na <- any(is.na(data.comb[,i]))
        if(any.na == "TRUE"){
          miss.c <- miss.c + 1
          #Imputing the NA's with the Mode Values
          data.comb[is.na(data.comb[,i]),i] = Mode(data.comb[,i], na.rm=TRUE)
        }
      }
    }   #end for (i in 1:ncol(data.comb)){
    
    miss.vl <- c(miss.n,miss.c)
    
    #Removing the columns having only 1 level
    if (length(unq) == 0 && length(onelvl) != 0){
      rem.col <- onelvl
      output$download1 <- renderPrint({
        cat("The columns having only 1 level removed Successsfully")
        cat("There are no columns having all Unique Values")
      })
      show("download1")
    } 
    #Removing the columns having only unique values
    if (length(unq) != 0 && length(onelvl) == 0){
      rem.col <- unq
      output$download1 <- renderPrint({
        cat("The columns having all unique values removed Successsfully")
        cat("There are no columns having only 1 level")
      })
      show("download1")
    }
    #Removing the columns having only 1 level && Removing the columns having only unique values
    if (length(unq) != 0 && length(onelvl) != 0){
      rem.col <- c(unq, onelvl)
      rem.col <- unique(rem.col)
      output$download1 <- renderPrint({
        cat("The columns having all unique values removed Successsfully")
        cat("The columns having only 1 level removed Successsfully")
      })
      show("download1")
    } 
    if (length(unq) == 0 && length(onelvl) == 0){
      rem.col <- 0
      output$download1 <- renderPrint({
        cat("There are no columns having all Unique Values")
        cat("There are no columns having only 1 level")
      })
      show("download1")
    } 
    
    if (miss.vl == "0" && missing == "2"){
      output$download2 <- renderPrint({
        cat("No Missing Value Imputation done as there are no Missing Values in the dataset")
      })
      show("download2")
    }
    
    if (miss.vl != "0" && missing == "2"){
      output$download2 <- renderPrint({
        cat(paste("Missing Values Imputation successfully done with", easyimp))
      })
      show("download2")
    }
    
    if (rem.col != "0"){
      data.comb <- data.comb[,-rem.col]
      assign('data.comb',  data.comb,  envir=.GlobalEnv)
    }
    
    updateSelectInput(session, "direction2a", selected = "bestmiss")
    
  } #end if(exists(data.comb) == "TRUE)  
  
}#end if (unique.v == "1" || onelevel == "1" || missing == "2"){ 
}#end if (exists("data.comb") == "TRUE"){  
})#end observeevent(input$extra - unq.one.easy)
    
#============================================================================================================================================
#                         END OF CODE FOR HANDLING UNIQUE VALUES, ONE LEVEL AND EASY MISSING VALUES METHODS
#============================================================================================================================================    

#============================================================================================================================================
#                         START OF CODE FOR HANDLING BEST MISSING VALUES IMPUTATION METHOD
#============================================================================================================================================    
    
    observeEvent(input$direction2a,{
      print("2nd entered")
      if (exists("data.comb") == "TRUE"){  
        if (missing == "1"){
          output$bestmiss <- renderPrint({
            cat(paste("Best Missing Value Imputation process with",bestimp,"has begun"))
          })
          show("bestmiss")
          updateSelectInput(session, "direction2b", selected = "bestmiss")
        }
        if (missing == "0"){
          updateSelectInput(session, "direction3a", selected = "imbal")
        }  
      }  
    })
    
    observeEvent(input$direction2b,{
      
      if (exists("data.comb") == "TRUE"){
        miss.vl <- 0 
        for (i in 1:ncol(data.comb)){
          any.na <- any(is.na(data.comb[,i]))
          if(any.na == "TRUE"){
            miss.vl <- miss.vl + 1
          }#end if(any.na == "TRUE"){
        }#end for (i in 1:ncol(data.comb)){
        
        if (miss.vl == 0){
          output$bestmiss <- renderPrint({
            cat("No Missing Values Imputation done as there are NO Missing values in the dataset")
          })     
          show("bestmiss")
          updateSelectInput(session, "direction3a", selected = "imbal")
        }#end if (miss.vl == 0){
        
        if (miss.vl != 0){
          
          #Finding all the categorical variables having more than 53 levels
          n <- 0
          ind <- c()
          ordf <- c("ordered", "factor")
          for (i in 1:ncol(data.comb)){
            if (class(data.comb[,i]) == "factor" || class(data.comb[,i]) == ordf){
              if (length(levels(data.comb[,i])) > 53){
                n <- n + 1
                ind[n] <- i
              }  
            }
          }
          
          #Creating a dataframe excluding the categorical variables having more than 53 levels
          if (length(ind) != 0){
            data_excl_53lev <- data.comb[,-ind]
            data_excl_53lev <- as.data.frame(data_excl_53lev)  
          }
          
          print(missforest_maxit)
          print(missforest_ntree)
          #str(data_excl_53lev)
          missforest_maxit <- as.numeric(missforest_maxit)
          missforest_ntree <- as.numeric(missforest_ntree)
          #Random Forest Method for missing values imputation in R
          imputed.rf <- missForest(data.frame(data_excl_53lev), maxiter = missforest_maxit, ntree = missforest_ntree, replace = TRUE)
          
          data.comb <- as.data.frame(imputed.rf$ximp)    
          assign('data.comb',  data.comb,  envir=.GlobalEnv)
          
          output$bestmiss1 <- renderPrint({
            cat(paste("Ahh that took a long time I guess. Well Best Missing Values Imputation Method with",bestimp, "has finished successfully"))
          }) 
          show("bestmiss1")
          updateSelectInput(session, "direction3a", selected = "imbal")
        }#end  if (miss.vl != 0){
      }   #endif if (exists(data.comb) == "TRUE"){
    })      #end observeEvent(input$direction2b,{ 
    
#============================================================================================================================================
#                         END OF CODE FOR HANDLING BEST MISSING VALUES IMPUTATION METHOD
#============================================================================================================================================    
    
#============================================================================================================================================
#                         START OF CODE FOR HANDLING IMBALANCED TARGET VARIABLE CLASS
#============================================================================================================================================    
    
    observeEvent(input$direction3a, {
      if (exists("data.comb") == "TRUE"){
        
        if (imbalance == "1"){
          output$imbalance.start <- renderPrint({
            cat("Imbalance Data Handling has started")
          })
          show("imbalance.start")
          updateSelectInput(session, "direction3b", selected = "imbal")
        }
        
        if (imbalance == "0"){
          updateSelectInput(session, "direction4a", selected = "standard")
        }  
      } 
    })#end observeEvent(input$direction3a, {
    
    observeEvent(input$direction3b, {
      if (exists("data.comb") == "TRUE"){
        
        date.colno <- c()
        n <- 0
        
        for (i in 1:length(date.colnames)){
          n <- n + 1
          date.colno[n] <- which(colnames(data.comb) == date.colnames[i])
          col <- date.colno[n]
          data.comb[,col] <- as.factor(data.comb[,col])
        }
        
        target.no <- which(colnames(data.comb) == tgt.name)
        ll        <- levels(data.comb[,target.no])
        ll1       <- ll[1]
        
        data.comb <- mutate(data.comb,newtarget = ifelse(data.comb[,target.no]==ll1,0,1))
        newtgt.no <- which(colnames(data.comb) == "newtarget")
        data.comb[,target.no] <- as.factor(data.comb[,target.no])
        data.comb[,newtgt.no] <- as.factor(data.comb[,newtgt.no])
        
        data.comb <- SMOTE(newtarget ~ ., data.comb, perc.over = perc.over, perc.under = perc.under)
        
        for (i in 1:length(date.colno)){
          col <- date.colno[i]
          data.comb[,col] <- as.Date(data.comb[,col])
          data.comb[,col] <- as.Date(data.comb[,col])
        }
        
        
        newtgt.no <- which(colnames(data.comb) == "newtarget")
        data.comb <- data.comb[,-newtgt.no]
        
        assign('data.comb',  data.comb,  envir=.GlobalEnv)
        
        #tgt.lev <- table(data.comb[,target.no])
        #print(tgt.lev)
        
        output$imbalance.end <- renderPrint({
          cat("Imbalance Data Handling has successfully finished")
        })  
        show("imbalance.end")
        updateSelectInput(session, "direction4a", selected = "standard")
        
      }#end if (exists("data.comb") == "TRUE"){  
    })   #end observeEvent(input$direction3b, {    

#============================================================================================================================================
#                         END OF CODE FOR HANDLING IMBALANCED TARGET VARIABLE CLASS
#============================================================================================================================================    
    
    
#============================================================================================================================================
#                         START OF CODE FOR HANDLING NUMERICAL DATA STANDARDISATION
#============================================================================================================================================    
    
    observeEvent(input$direction4a, {
      if (exists("data.comb") == "TRUE"){
        if (standardise.n == "1"){
          output$standard.start <- renderPrint({
            cat("Numerical variables standardisation has begun")
          })
          show("standard.start")
          updateSelectInput(session, "direction4b", selected = "standard")  
        } 
        
        if (standardise.n == "0"){
          updateSelectInput(session, "direction5a", selected = "dummy")  
        }
        
      }#end if (exists("data.comb") == "TRUE"){
    })   #end observeEvent(input$direction4a, {  
    
    observeEvent(input$direction4b, {
      if (exists("data.comb") == "TRUE"){
        num.colno <- 0
        n        <- 0
        for (i in 1:ncol(data.comb)){
          if (class(data.comb[,i]) == "numeric"){
            n            <- n + 1
            num.colno[n] <- i
          }
        }#end for (i in 1:ncol(data.comb)){  
        if (length(num.colno) != 0){
        num.data <- data.comb[,num.colno]
        num.data <- as.data.frame(num.data)
        data.comb1 <- data.comb[,-num.colno]
        
        #Min-Max transformation
        mins <- apply(num.data, 2, min)
        maxs <- apply(num.data, 2, max)
        num.scaled <- as.data.frame(scale(num.data, center = mins, scale = maxs-mins))
        
        #Converting the num.scaled variables into numeric datatype
        num.scaled[,] <- lapply(num.scaled[,], as.numeric)
        
        data.comb <- cbind(num.scaled,data.comb1)
        data.comb <- as.data.frame(data.comb)
        assign('data.comb',  data.comb,  envir=.GlobalEnv)
        output$standard.end <- renderPrint({
          print("Data standardisation of numerical variables haas finished successfully")
        })
        show("standard.end")
        updateSelectInput(session, "direction5a", selected = "dummy")
        }#end if (exists("data.comb") == "TRUE"){
        
        if (length(num.data) == 0){
          output$standard.end <- renderPrint({
            cat("There are no numerical variables to standardize in the dataset")
          })  
          show("standard.end")
        }#end if (length(num.data) == 0){
      }    #end if (exists("data.comb") == "TRUE"){
    })       #end observeEvent(input$direction4b, {
      
        
#============================================================================================================================================
#                           END OF CODE FOR HANDLING NUMERICAL DATA STANDARDISATION
#============================================================================================================================================    
    
#============================================================================================================================================
#                             START OF CODE FOR DUMMIFYING CATEGORICAL VARIABLES
#============================================================================================================================================    
    
observeEvent(input$direction5a,{
  if (exists("data.comb") == "TRUE"){  
  
    if (dummy.c == "1"){
      output$dummy.start <- renderPrint({
        cat("Dummifying of Categorical variables has started")
      })
      show("dummy.start")
      updateSelectInput(session, "direction5b", selected = "dummy")
    }
  
    if (dummy.c == "0"){
      output$dummy.end <- renderPrint({
        cat("CONGRATULATIONS..ALL THE OPERATIONS HAVE FINISHED SUCCESFULLY..:D")
      })
      show("dummy.end")
      updateSelectInput(session, "download", selected = "download")
    }
    
  }#end if (exists("data.comb") == "TRUE"){
})#end observeEvent(input$direction5a,{    
    
    
observeEvent(input$direction5b,{
  if (exists("data.comb") == "TRUE"){     
    cat.colno <- 0
    n        <- 0
    for (i in 1:ncol(data.comb)){
      if (class(data.comb[,i]) == "factor"){
        n            <- n + 1
        cat.colno[n] <- i
      }
    }#end for (i in 1:ncol(data.comb)){    
    if (length(cat.colno) != 0){
      cat.data   <- data.comb[,cat.colno]
      data.comb1 <- data.comb[,-cat.colno]
      
      library(caret)
      dummy_var <- dummyVars(" ~.", data = cat.data, levelsOnly = FALSE)
      one_hot_data <- data.frame(predict(dummy_var, newdata = cat.data))
      data.comb <- cbind(data.comb1,one_hot_data)
      data.comb <- as.data.frame(data.comb)
      assign('data.comb',  data.comb,  envir=.GlobalEnv)
      
      output$dummy.end <- renderPrint({
        cat("All the categorical variables have been dummified successfully")
        cat("CONGRATULATIONS..ALL THE OPERATIONS HAVE FINISHED SUCCESFULLY..:D")
      }) 
      show("dummy.end")
      updateSelectInput(session, "download", selected = "download")
    }
    
    if (length(cat.colno) == 0){
      output$dummy.end <- renderPrint({
        cat("There are no categorical variables to dummify in the dataset")
      })  
      show("dummy.end")
    }
  }#end if (exists("data.comb") == "TRUE"){
})#end observeEvent(input$direction5b,{     
        
    
    
#============================================================================================================================================
#                              END OF CODE FOR DUMMIFYING CATEGORICAL VARIABLES
#============================================================================================================================================    
    
observeEvent(input$download,{  
  if (exists("data.comb") == "TRUE"){  
  write.csv(data.comb, file=file.choose(), row.names=TRUE, sep=',', col.names=TRUE)
  }
})#end observeEvent(input$download,{

#============================================================================================================================================
#                                         END OF CODE FOR DOWNLOAD BUTTON
#============================================================================================================================================    
    
} #1} #END OF SERVER  
  
#============================================================================================================================================
#                                         CALLING THE SHINY APP
#============================================================================================================================================    
  
  shinyApp(ui = ui, server=server)
  #runApp(list(ui = ui, server=server), launch.browser = TRUE)
#============================================================================================================================================  
  
  
  
  
  data.comb <- data
  data.comb <- as.data.frame(data.comb)
  
  date <- data.frame(data.comb)
  datecheck <- sapply(date, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))
  
  n <- 0
  dc <- data.frame(matrix(NA, nrow = 2, ncol = 2))
  dc1 <- 0
  
  for (i in 1:length(datecheck)){
    if(datecheck[i] == "TRUE"){
      data.comb[,i] <- anydate(data.comb[,i])  #Converts date into yyyy-mm-dd format
      data.comb[,i] <- as.Date(data.comb[,i])  #Converts the date column into date data type
    }
  }
  
  #Converts the columns into their corresponding datatypes
  for (i in 1:ncol(data.comb)){
    uq.ln <- length(unique(data.comb[,i]))
    if (uq.ln > 20){  
      if (class(data.comb[,i]) == "integer" || class(data.comb[,i]) == "numeric" || class(data.comb[,i]) == "double"){  
        data.comb[,i] <- as.numeric(data.comb[,i]) 
      }
      if (class(data.comb[,i]) == "character" || class(data.comb[,i]) == "factor"){
        data.comb[,i] <- as.factor(data.comb[,i]) 
      }
    }
    #If number of levels are LESS THAN OR EQUAL TO 20, treat all of them as FACTORS
    if (uq.ln > 1 && uq.ln <= 20){
      data.comb[,i] <- as.factor(data.comb[,i]) 
    }
  }
  
  nas <- 0.001
  nas.thr.perc <- nas/100
  nas.col <- c()
  n <- 0
  
  for (col in 1:ncol(data.comb)){
    #Fetching the column numbers having more than 70% Missing data
    sum <- sum(is.na(data.comb[col]))
    if (sum > (nas.thr.perc * nrow(data.comb))){
      n <- n + 1
      nas.col[n] <- col   
    }  #end if
  }     #end for
  
  library(DMwR)
  sd<- data.comb
  target <- sd[,52]
  ll <- levels(sd[,target.no])
  ll1 <- ll[1]
  sd <- data.comb
  target.no <- which(colnames(sd) == tgt.name)
  
  
  sd <- mutate(sd,newtarget = ifelse(sd[,target.no]==ll1,0,1))
  sd$newtarget <- as.factor(sd$newtarget)
  sd[,52] <- as.factor(sd[,52])
  sd[,48] <- as.factor(sd[,48])
  sd[,49] <- as.factor(sd[,49])
  
  d <- c(48,49)
  sd1[,48] <- as.factor(sd1[,48])
  class(sd1[,48])
  sd1 <- SMOTE(newtarget ~ ., sd,perc.over = 100, perc.under = 200)
  table(sd1$NPS_Status)
  sd[,48] <- as.Date(sd[,48])
  sd[,49] <- as.Date(sd[,49])
  str(sd) 
  which(class(sd[,41]) == "Date")
  which(colnames(sd) == "newtarget")
  nrow(sd)
  nrow(sd1)  
  table(sd[,52])  
  table(sd1[,52])  
  
  Sys.sleep(1)
  
  data.comb
  length(unique(data.comb[,1]))  
  nrow(data.comb)
  
  #Imputing the NA's with the Mean Values
  data.comb[,1][is.na(data.comb[,1])] = mean(data.comb[,1], na.rm=TRUE)
  
  
  
  
  
  
  
  n1      <- 0
  n2      <- 0
  miss.vl <- 0
  unq     <- c()
  onelvl  <- c()
  
  for (i in 1:ncol(data.comb)){
    
    if (unique.v == "1"){
      len.unq <- length(unique(data.comb[,i]))  
      if (len.unq == nrow(data.comb)){
        n1      <- n1 + 1
        unq[n1] <- i
      }
    }
    
    if (onelevel == "1"){
      len.lev <- length(levels(data.comb[,i]))
      if (len.lev == "1"){
        n2          <- n2 + 1
        onelvl[n2]  <- i
      }
    }
    
    if (easyimp == "mean"){
      any.na <- any(is.na(data.comb[,i]))
      if(any.na == "TRUE"){
        miss.vl <- miss.vl + 1
        #Imputing the NA's with the Mean Values
        #data.comb[,i][is.na(data.comb[,i])] = mean(data.comb[,i], na.rm=TRUE)
      }
    }
  }  
  
  any(is.na(data.comb[,3]))
  
  dd <- c()
  n <- 0
  for (i in 1:ncol(data.comb)){
    
    len.lev <- length(levels(data.comb[,i]))
    if (len.lev !=0){
      n <- n + 1
      dd[n] <- i
    }
    
  }
  
  len.lev <- length(levels(data.comb[,11]))
  
  is.na(data.comb$total_prodA_std_booking_amount)
  
  which(colnames(data.comb) == "total_prodA_std_booking_amount")  
  
  
  miss.vl <- 0
  n <- 0
  mss.c <- c()
  for (i in 1:ncol(data.comb)){
    if (class(data.comb[,i]) == "factor")
      any.na <- any(is.na(data.comb[,i]))
    if(any.na == "TRUE"){
      miss.vl <- miss.vl + 1
      n <- n + 1
      mss.c[n] <- i
      #Imputing the NA's with the Mean Values
      #data.comb[,i][is.na(data.comb[,i])] = mean(data.comb[,i], na.rm=TRUE)
    }  
  }  
  
  any.na <- any(is.na(data.comb[,161]))
  
  
  is.factor(data.comb[,160])
  
  data.comb[is.na(data.comb[,160]),160] = mode(data.comb[,160], na.rm=TRUE)
  
  impute()    
  
  Mode <- function (x, na.rm) {
    xtab <- table(x)
    xmode <- names(which(xtab == max(xtab)))
    if (length(xmode) > 1) xmode <- ">1 mode"
    return(xmode)
  }
  
  miss.vl <- 0
  miss.col <- c()
  n <- 0
  for(i in 1:ncol(data.comb)){
    if (class(data.comb[,i]) == "factor"){
      any.na <- any(is.na(data.comb[,i]))
      if(any.na == "TRUE"){
        n <- n + 1
        miss.col[n] <- i
        miss.vl <- miss.vl + 1
        #Imputing the NA's with the Mode Values
        #data.comb[is.na(data.comb[,i]),i] = Mode(data.comb[,i], na.rm=TRUE)
      }
    }
  }
  
  
  data.comb[is.na(data.comb[,474]),474] = Mode(data.comb[,474], na.rm=TRUE)
  
  
  colnames(data.comb[474])  
  
