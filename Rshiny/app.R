library("netmeta")
library("NlcOptim")
library("kableExtra")
library("dplyr")
source(file = "SolveSampleSize.R")

# contrast-level dat input only currently 
ui <-
    fluidPage(titlePanel("Sample Size Calculation to reach a predefined power for a future two-arm study (binary outcome), based
                         on the existing network"),
              tabsetPanel(
                  tabPanel("sample data",
                           fluidRow(
                               
                               column(4,style="background-color:lightgoldenrodyellow",
                                      h4("Step1: Sample network data"),
                                      "Please refer to the ",
                                      tags$a(href="https://github.com/fangshuye/NMA-two-arms-sample-size/blob/main/sampledat.csv", 
                                             "sample dataset"),
                                      " on Github and the ",
                                      tags$a(href="https://github.com/fangshuye/NMA-two-arms-sample-size/blob/main/README.md", 
                                             "ReadMe file"),
                                      " explains the meaning of each column",
                                      # Horizontal line ----
                                      tags$hr(),
                                      h4("Step2: Select two treatments in your future trial"),
                                      #helpText("This part would appear after the data with correct format is uploaded"),
                                      uiOutput("treatment1_s"),
                                      uiOutput("treatment2_s")
                                      
                               ),
                               
                               column(4,style="background-color:seashell",
                                      h4("Step3: Confirm the parameters"),
                                      uiOutput("treatment_s"), # select the treatment that you know the risk 
                                      uiOutput("Risk_above_s"), # enter the risk of the selected risk above
                                      htmlOutput("Risk_NMA_s"), # show the estimated risk of the other treatment by our existing NMA
                                      uiOutput("NMA_Enter_s"), # enter the risk of the other treatment if you don't want to use the estimated one
                                      uiOutput("Risk_Enter_s"), # the value you entered
                                      numericInput("power_level_s","Predefind Power",step=0.000001,value = 0.8,max = 0.99999,min = 0.1),
                                      h4("Step4: Enter the paramater to calculate the cost"),
                                      radioButtons("cost_s",
                                                   "Do you want to calculate the cost for each allocation plan?",
                                                   c("Yes",
                                                     "No"),
                                                   selected = "No"),
                                      uiOutput("cost_number_s")
                               ),
                               
                               column(4,style="background-color:aliceblue",
                                      h4("Information from the existing network"),
                                      htmlOutput("txtOutput_s"),
                                      tags$hr(),
                                      h4("The optimal sample size for each treatment in a future trial"),
                                      tableOutput("tabOutput_s"),
                                      h4("Summary"),
                                      htmlOutput("Summary_s"))
                           )),
                  tabPanel("your data",
                           fluidRow(
                               
                               column(4,style="background-color:lightgoldenrodyellow",
                                      h4("Step1: Upload the existing evidence (Contrast-level data)"),
                                      "(Please refer to the ",
                                      tags$a(href="https://github.com/fangshuye/NMA-two-arms-sample-size/blob/main/sampledat.csv", 
                                             "sample dataset"),
                                      " on Github and the ",
                                      tags$a(href="https://github.com/fangshuye/NMA-two-arms-sample-size/blob/main/README.md", 
                                             "ReadMe file"),
                                      " explains the meaning of each column)",
                                      # contrast-level dat
                                      fileInput("file1", "Choose CSV File",
                                                multiple = F,
                                                accept = ".csv"),
                                      helpText("Please upload the dataset using the same format",
                                               "and column names as the sample dataset."),
                                      
                                      # Horizontal line ----
                                      tags$hr(),
                                      h4("Step2: Select two treatments in your future trial"),
                                      helpText("This part would appear after the data with correct format is uploaded"),
                                      uiOutput("treatment1"),
                                      uiOutput("treatment2")
                                      
                               ),
                               
                               column(4,style="background-color:seashell",
                                      h4("Step3: Confirm the parameters"),
                                      uiOutput("treatment"), # select the treatment that you know the risk 
                                      uiOutput("Risk_above"), # enter the risk of the selected risk above
                                      htmlOutput("Risk_NMA"), # show the estimated risk of the other treatment by our existing NMA
                                      uiOutput("NMA_Enter"), # enter the risk of the other treatment if you don't want to use the estimated one
                                      uiOutput("Risk_Enter"), # the value you entered
                                      numericInput("power_level","Predefind Power",step=0.000001,value = 0.8,max = 0.99999,min = 0.1),
                                      h4("Step4: Enter the paramater to calculate the cost"),
                                      radioButtons("cost",
                                                   "Do you want to calculate the cost for each allocation plan?",
                                                   c("Yes",
                                                     "No"),
                                                   selected = "No"),
                                      uiOutput("cost_number")
                               ),
                               
                               column(4,style="background-color:aliceblue",
                                      h4("Information from the existing network"),
                                      htmlOutput("txtOutput"),
                                      tags$hr(),
                                      h4("The optimal sample size for each treatment in a future trial"),
                                      tableOutput("tabOutput"),
                                      h4("Summary"),
                                      htmlOutput("Summary"))
                           ))
                 
              
              ))

server <- function(input, output,session) {
    ########################## tab: sample dat ##########################
    #read the sample data
    filedata_s <- reactive({
        read.csv(file = 'sampledat.csv')
    })
    arm_s <- reactive({
        dat_s <- filedata_s()
        if (!is.null(dat_s)){
            nma_old <- netmeta(TE,seTE,treat1,treat2,studlab,data=dat_s,
                               sm="OR",comb.fixed = T,comb.random = F)
            arms <- unique(c(dat_s$treat1,dat_s$treat2))
            list(nma_old = nma_old, arms = arms)
        }
    })
    
    
    output$treatment1_s <- renderUI({
        
        dat_s <- filedata_s()
        
        if(!is.null(dat_s)){
            radioButtons(inputId = "choice1_s",
                         label = "Please select the first treatment in your future trial",
                         choices = arm_s()$arms,
                         selected = arm_s()$arms[2])
            
        }
    })
    
    arm2_s <- reactive({
        dat_s <- filedata_s()
        if (!is.null(dat_s)){
            setdiff(arm_s()$arms,input$choice1_s)
        }
    })
    
    output$treatment2_s <- renderUI({
        
        dat_s <- filedata_s()
        
        if(!is.null(dat_s)){
            radioButtons(inputId="choice2_s", 
                         label="Please select the second treatment in your future trial",
                         choices= arm2_s(),
                         selected = arm2_s()[2])
            
        }
    })
    
    sigma_nma_old_s <- reactive({
        dat_s <- filedata_s()
        if (!is.null(dat_s)){
            nma_old <- arm_s()$nma_old
            trt1 <- input$choice1_s
            trt2 <- input$choice2_s
            nma_old$seTE.fixed[trt1,trt2]
        }
        
    })
    
    name_trt1_s <- reactive({
        dat_s <- filedata_s()
        if (!is.null(dat_s) & !is.null(input$choice1_s)){
            input$choice1_s
        }
    })
    
    name_trt2_s <- reactive({
        dat_s <- filedata_s()
        if (!is.null(dat_s) & !is.null(input$choice2_s)){
            input$choice2_s
        }
    })
    
    output$treatment_s <- renderUI({
        
        dat_s <- filedata_s()
        if(!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s)){
            radioButtons(inputId = "baseline_s",
                         label = "Please select one treatment that you know the risk",
                         choices = c(name_trt1_s(),name_trt2_s()),
                         selected = name_trt1_s())
        }
    })
    
    name_trt_s <- reactive({
        dat_s <- filedata_s()
        if(!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s)){
            list(base = input$baseline_s, 
                 second = setdiff(c(name_trt1_s(),name_trt2_s()),input$baseline_s))
        }
    })
    
    output$Risk_above_s <- renderUI({
        dat_s <- filedata_s()
        if(!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s)){
            numericInput("risk1_s",paste0("Risk of ",name_trt_s()$base),
                         step=0.000001,value = 0.20,max = 0.99999,min = 0.00001)
        }
    })
    
    p2_s <- reactive({
        dat_s <- filedata_s()
        if (!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s) & !is.null(input$risk1_s)){
            nma_old <- arm_s()$nma_old
            trt1 <- name_trt1_s()
            trt2 <- name_trt2_s()
            
            # get baseline risk
            base_trt <- name_trt_s()$base
            lor_2 <- nma_old$TE.fixed[base_trt,trt2]
            lor2prob(input$risk1_s,lor_2)
        }
        
    })
    
    
    
    output$Risk_NMA_s  = renderUI({
        dat_s <- filedata_s()
        if(!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s)){
            str1 <- paste0("The risk of ",name_trt_s()$second," estimated by the existing network is ",
                           round(p2_s(),4))
            HTML(str1)
        }
    })
    
    output$NMA_Enter_s <- renderUI({
        
        dat_s <- filedata_s()
        if(!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s)){
            radioButtons("howrisk_s",
                         paste0("Which would you use for the risk of ",name_trt()$second),
                         c("Enter/Define the risk" = "enter",
                           "Use the value above directly (estimated by existing NMA)" = "NMA"),
                         selected = "enter")
        }
    })
    
    
    output$Risk_Enter_s <- renderUI({
        dat_s <- filedata_s()
        if(!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s) & !is.null(input$howrisk_s)){
            if(input$howrisk_s=='enter'){
                numericInput("risk2_s",paste0("Risk of ",name_trt_s()$second),
                             step=0.000001,value = 0.25,max = 0.99999,min = 0.00001)
            }
        }
    })
    
    output$cost_number_s <- renderUI({
        if(input$cost_s=="Yes"){
            tagList(
                numericInput("cost1_s",paste0("Cost($) per treatment(",name_trt_s()$base,")"),
                             step=0.000001,value = 2,min = 0.00001),
                numericInput("cost2_s",paste0("Cost($) per treatment(",name_trt_s()$second,")"),
                             step=0.000001,value = 2,min = 0.00001),
                numericInput("cost3_s","Cost($) per animal",
                             step=0.000001,value = 2,min = 0.00001)
                
            ) 
        }
    })
    
    output$txtOutput_s  = renderUI({
        dat_s <- filedata_s()
        if(!is.null(dat_s)){
            str <- paste0("Standard error of the estimated effect size 
                          between selected two treatments by the existing network is ",
                          round(sigma_nma_old_s(),4))
            HTML(str)
        }
    })
    
    
    SampleSize_s <- reactive({
        dat_s <- filedata_s()
        if (!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s) & !is.null(input$howrisk_s) & !is.null(input$risk1_s)){
            sigma <- sigma_nma_old_s()
            power_level <- input$power_level_s
            
            risk1 <- input$risk1_s
            risk2 <- ifelse(input$howrisk_s=='enter',input$risk2_s,p2_s())
            
            samplesize_even = rep(SolveSampleSize_Withprev_equal(risk1,risk2,sigma,power_level)/2,2)
            samplesize = SolveSampleSize_Withprev(risk1,risk2,sigma,power_level)
            samplesize_single = SolveSampleSize_Single(risk1,risk2,power_level)
            samplesize_single_even = rep(SolveSampleSize_Single_equal(risk1,risk2,power_level)/2,2)
            list(NMA_even = samplesize_even,
                 NMA = samplesize,
                 Single_even = samplesize_single_even,
                 Single = samplesize_single)
        }
        
    })
    
    output$tabOutput_s <- function() {
        dat_s <- filedata_s()
        if(!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s) & !is.null(input$howrisk_s)){
            samplesize_even <- SampleSize_s()$NMA_even
            samplesize <- SampleSize_s()$NMA
            samplesize_single_even <- SampleSize_s()$Single_even
            samplesize_single <- SampleSize_s()$Single
            
            output_dat <- data.frame(n1 = c(samplesize_even[1],samplesize[1],
                                            samplesize_single_even[1],samplesize_single[1]),
                                     n2 = c(samplesize_even[2],samplesize[2],
                                            samplesize_single_even[2],samplesize_single[2]))
            
            output_dat$total <- output_dat$n1+output_dat$n2
            
            collapse_rows_dt <- cbind(C1 = c(rep("with existing NMA", 2), rep("isolation", 2)),
                                      C2 = c("even","uneven","even","uneven"),
                                      output_dat)
            colnames(collapse_rows_dt) <- c("","",name_trt1_s(),name_trt2_s(),"Total")
            if(input$cost_s=="Yes"){
                cost <- c(input$cost1_s,input$cost2_s,input$cost3_s)
                costs <- output_dat$n1*cost[1] + output_dat$n2*cost[2] + output_dat$total*cost[3]
                collapse_rows_dt[,6] <- costs
                colnames(collapse_rows_dt)[1:2] <- c("","")
                colnames(collapse_rows_dt)[6] <- "Costs ($)"
                collapse_rows_dt %>%
                    kbl() %>%
                    kable_styling() %>%
                    collapse_rows(columns = 1:2, valign = "middle")%>%
                    add_header_above(c("","","Sample Size" = 3,"")) 
            }else{
                collapse_rows_dt %>%
                    kbl() %>%
                    kable_styling() %>%
                    collapse_rows(columns = 1:2, valign = "middle") %>%
                    add_header_above(c("","","Sample Size" = 3)) 
                
            }
            
            
        }
    }
    
    output$Summary_s  = renderUI({
        dat_s <- filedata_s()
        if(!is.null(dat_s) & !is.null(input$choice1_s) & !is.null(input$choice2_s) & !is.null(input$howrisk_s) & !is.null(input$risk1_s)){
            samplesize_even <- SampleSize_s()$NMA_even
            samplesize <- SampleSize_s()$NMA
            samplesize_single_even <- SampleSize_s()$Single_even
            samplesize_single <- SampleSize_s()$Single
            
            size1 <- paste0("With existing NMA, ","the optimal sample sizes for ", 
                            name_trt_s()$base," and ",name_trt_s()$second, " are ",
                            samplesize[1], " and ", samplesize[2], " respectively.",
                            " If we want to even allocate the sample size, ",
                            "the optimal sample size for each treatment is ", 
                            samplesize_even[1],".")
            
            size2 <- paste0("Without existing NMA (analyze in isolation), ","the optimal sample sizes for ", 
                            name_trt_s()$base," and ",name_trt_s()$second, " are ",
                            samplesize_single[1], " and ", samplesize_single[2], " respectively.",
                            " If we want to even allocate the sample size, ",
                            "the optimal sample size for each treatment is ", 
                            samplesize_single_even[1],".")
            
            HTML(paste(size1, size2, sep = '<br/>'))
        }
    })
    
    ########################## tab: your dat ##########################
    #This function is repsonsible for loading in the selected file
    filedata <- reactive({
        infile <- input$file1
        if (!is.null(infile)) {
            # User has not uploaded a file yet
            read.csv(infile$datapath)
        }
    })
    
    arm <- reactive({
        dat <- filedata()
        if (!is.null(dat)){
            nma_old <- netmeta(TE,seTE,treat1,treat2,studlab,data=dat,
                               sm="OR",comb.fixed = T,comb.random = F)
            arms <- unique(c(dat$treat1,dat$treat2))
            list(nma_old = nma_old, arms = arms)
        }
    })
    
    sigma_nma_old <- reactive({
        dat <- filedata()
        if (!is.null(dat)){
            nma_old <- arm()$nma_old
            trt1 <- input$choice1
            trt2 <- input$choice2
            nma_old$seTE.fixed[trt1,trt2]
        }
        
    })

    
    output$treatment1 <- renderUI({
        
        dat <- filedata()
        
        if(!is.null(dat)){
            radioButtons(inputId = "choice1",
                         label = "Please select the first treatment in your future trial",
                         choices = arm()$arms,
                         selected = arm()$arms[2])
            
        }
    })
    
    arm2 <- reactive({
        dat <- filedata()
        if (!is.null(dat)){
            setdiff(arm()$arms,input$choice1)
        }
    })
    
    output$treatment2 <- renderUI({
        
        dat <- filedata()
        
        if(!is.null(dat)){
            radioButtons(inputId="choice2", label="Please select the second treatment in your future trial",
                         choices= arm2(),
                         selected = arm2()[2])
            
        }
    })
    
    name_trt1 <- reactive({
        dat <- filedata()
        if (!is.null(dat) & !is.null(input$choice1)){
            input$choice1
        }
    })
    
    name_trt2 <- reactive({
        dat <- filedata()
        if (!is.null(dat) & !is.null(input$choice2)){
            input$choice2
        }
    })
    
    output$treatment <- renderUI({
        
        dat <- filedata()
        
        if(!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2)){
            radioButtons(inputId = "baseline",
                         label = "Please select one treatment that you know the risk",
                         choices = c(name_trt1(),name_trt2()),
                         selected = name_trt1())
        }
    })
    
    name_trt <- reactive({
        dat <- filedata()
        if (!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2)){
            list(base = input$baseline, 
                 second = setdiff(c(name_trt1(),name_trt2()),input$baseline))
        }
    })
    
    output$Risk_above <- renderUI({
        
        dat <- filedata()
        
        if(!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2)){
            numericInput("risk1",paste0("Risk of ",name_trt()$base),
                         step=0.000001,value = 0.20,max = 0.99999,min = 0.00001)
        }
    })
    
    p2 <- reactive({
        dat <- filedata()
        if (!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2) & !is.null(input$risk1)){
            nma_old <- arm()$nma_old
            trt1 <- name_trt1()
            trt2 <- name_trt2()
            
            # get baseline risk
            base_trt <- name_trt()$base
            lor_2 <- nma_old$TE.fixed[base_trt,trt2]
            lor2prob(input$risk1,lor_2)
        }
        
    })
    

    
    output$Risk_NMA  = renderUI({
        dat <- filedata()
        if(!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2)){
            str1 <- paste0("The risk of ",name_trt()$second," estimated by the existing network is ",
                           round(p2(),4))
            HTML(str1)
        }
    })
    
    output$NMA_Enter <- renderUI({
        
        dat <- filedata()
        
        if(!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2)){
            radioButtons("howrisk",
                         paste0("Which would you use for the risk of ",name_trt()$second),
                         c("Enter/Define the risk" = "enter",
                           "Use the value above directly (estimated by existing NMA)" = "NMA"),
                         selected = "enter")
        }
    })
    
    
    output$Risk_Enter <- renderUI({
        dat <- filedata()
        
        if(!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2) & !is.null(input$howrisk)){
            if(input$howrisk=='enter'){
                numericInput("risk2",paste0("Risk of ",name_trt()$second),
                             step=0.000001,value = 0.25,max = 0.99999,min = 0.00001)
            }
        }
    })

    output$cost_number <- renderUI({
        if(input$cost=="Yes"){
            tagList(
                numericInput("cost1",paste0("Cost($) per treatment(",name_trt()$base,")"),
                             step=0.000001,value = 2,min = 0.00001),
                numericInput("cost2",paste0("Cost($) per treatment(",name_trt()$second,")"),
                             step=0.000001,value = 2,min = 0.00001),
                numericInput("cost3","Cost($) per animal",
                             step=0.000001,value = 2,min = 0.00001)
                
            ) 
        }
    })
    
    output$txtOutput  = renderUI({
        dat <- filedata()
        if(!is.null(dat)){
            str <- paste0("Standard error of the estimated effect size 
                          between selected two treatments by the existing network is ",
                          round(sigma_nma_old(),4))
          HTML(str)
        }
    })
    
    
    SampleSize <- reactive({
        dat <- filedata()
        if (!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2) & !is.null(input$howrisk) & !is.null(input$risk1)){
            sigma <- sigma_nma_old()
            power_level <- input$power_level
            
            risk1 <- input$risk1
            risk2 <- ifelse(input$howrisk=='enter',input$risk2,p2())
            
            samplesize_even = rep(SolveSampleSize_Withprev_equal(risk1,risk2,sigma,power_level)/2,2)
            samplesize = SolveSampleSize_Withprev(risk1,risk2,sigma,power_level)
            samplesize_single = SolveSampleSize_Single(risk1,risk2,power_level)
            samplesize_single_even = rep(SolveSampleSize_Single_equal(risk1,risk2,power_level)/2,2)
            list(NMA_even = samplesize_even,
                 NMA = samplesize,
                 Single_even = samplesize_single_even,
                 Single = samplesize_single)
        }
        
    })
    
    output$tabOutput <- function() {
        dat <- filedata()
        if(!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2) & !is.null(input$howrisk)){
            samplesize_even <- SampleSize()$NMA_even
            samplesize <- SampleSize()$NMA
            samplesize_single_even <- SampleSize()$Single_even
            samplesize_single <- SampleSize()$Single
            
            output_dat <- data.frame(n1 = c(samplesize_even[1],samplesize[1],
                                            samplesize_single_even[1],samplesize_single[1]),
                                     n2 = c(samplesize_even[2],samplesize[2],
                                           samplesize_single_even[2],samplesize_single[2]))
            
            output_dat$total <- output_dat$n1+output_dat$n2

            collapse_rows_dt <- cbind(C1 = c(rep("with existing NMA", 2), rep("isolation", 2)),
                                      C2 = c("even","uneven","even","uneven"),
                                      output_dat)
            colnames(collapse_rows_dt) <- c("","",name_trt1(),name_trt2(),"Total")
            if(input$cost=="Yes"){
                cost <- c(input$cost1,input$cost2,input$cost3)
                costs <- output_dat$n1*cost[1] + output_dat$n2*cost[2] + output_dat$total*cost[3]
                collapse_rows_dt[,6] <- costs
                colnames(collapse_rows_dt)[1:2] <- c("","")
                colnames(collapse_rows_dt)[6] <- "Costs ($)"
                collapse_rows_dt %>%
                    kbl() %>%
                    kable_styling() %>%
                    collapse_rows(columns = 1:2, valign = "middle")%>%
                    add_header_above(c("","","Sample Size" = 3,"")) 
            }else{
                collapse_rows_dt %>%
                    kbl() %>%
                    kable_styling() %>%
                    collapse_rows(columns = 1:2, valign = "middle") %>%
                    add_header_above(c("","","Sample Size" = 3)) 
                    
            }
            
            
        }
    }
    
    output$Summary  = renderUI({
        dat <- filedata()
        if(!is.null(dat) & !is.null(input$choice1) & !is.null(input$choice2) & !is.null(input$howrisk) & !is.null(input$risk1)){
            samplesize_even <- SampleSize()$NMA_even
            samplesize <- SampleSize()$NMA
            samplesize_single_even <- SampleSize()$Single_even
            samplesize_single <- SampleSize()$Single
            
            size1 <- paste0("With existing NMA, ","the optimal sample sizes for ", 
                            name_trt()$base," and ",name_trt()$second, " are ",
                            samplesize[1], " and ", samplesize[2], " respectively.",
                            " If we want to even allocate the sample size, ",
                            "the optimal sample size for each treatment is ", 
                            samplesize_even[1],".")
            
            size2 <- paste0("Without existing NMA (analyze in isolation), ","the optimal sample sizes for ", 
                            name_trt()$base," and ",name_trt()$second, " are ",
                            samplesize_single[1], " and ", samplesize_single[2], " respectively.",
                            " If we want to even allocate the sample size, ",
                            "the optimal sample size for each treatment is ", 
                            samplesize_single_even[1],".")
            
            HTML(paste(size1, size2, sep = '<br/>'))
        }
    })
        
}

shinyApp(ui = ui, server = server)

