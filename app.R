library("netmeta")
library("NlcOptim")
library("kableExtra")
library("dplyr")
source(file = "SolveSampleSize.R")

# contrast-level dat input only currently 
ui <-
    fluidPage(titlePanel("Sample Size Calculation to reach a predefined power for a future two-arm study, 
                         based on the previous network"),
              fluidRow(
                  
                  column(4,style="background-color:lightgoldenrodyellow",
                         h4("Step1: Upload the existing evidence"),
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
                         radioButtons("howrisk",
                                      "Which would you use to represent for the risks of the two treatments?",
                                      c("I have other evidence or references to use and I want to enter the risks by myself" = "enter",
                                        "I would like to use the effect size (log odds ratio) estimated by previous NMA and enter the baseline risk by myself" = "NMA"),
                                      selected = "enter"),
                         uiOutput("Risk"),
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
                         h4("Information from the previous network"),
                         htmlOutput("txtOutput"),
                         tags$hr(),
                         h4("The optimal sample size for each treatment in a future trial"),
                         tableOutput("tabOutput"))
              ))

server <- function(input, output,session) {
    
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
    
    baseline <- reactive({
        dat <- filedata()
        if (!is.null(dat) && input$howrisk=='NMA'){
            nma_old <- arm()$nma_old
            trt1 <- input$choice1
            trt2 <- input$choice2
            
            # get baseline risk
            base_trt <- input$baseline_trt
            lor_1 <- nma_old$TE.fixed[base_trt,trt1]
            lor_2 <- nma_old$TE.fixed[base_trt,trt2]
            p1 <- lor2prob(input$baseline_risk,lor_1)
            p2 <- lor2prob(input$baseline_risk,lor_2)
            list(p1 = p1, p2 = p2) 
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
                         selected = character(0))
            
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
                         selected = character(0))
            
        }
    })
    
    name_trt1 <- reactive({
        dat <- filedata()
        if (!is.null(dat)){
            input$choice1
        }
    })
    
    name_trt2 <- reactive({
        dat <- filedata()
        if (!is.null(dat)){
            input$choice2
        }
    })
    
    
    output$Risk <- renderUI({
        if(input$howrisk=='enter'){
            tagList(
                numericInput("risk1",paste0("Risk of ",name_trt1()),
                             step=0.000001,value = 0.20,max = 0.99999,min = 0.00001),
                numericInput("risk2",paste0("Risk of ",name_trt2()),
                             step=0.000001,value = 0.25,max = 0.99999,min = 0.00001)
            )
        }else{
            tagList(
                radioButtons(inputId="baseline_trt", 
                             label="Please select the baseline treatment in your previous NMA",
                             choices = arm()$arms),
                helpText("Note: you can choose any treatment in your upload file as baseline treatment 
               as long as you can get an estimation or observed result of the risk of the baseline 
               treatment you seleced"),
                numericInput("baseline_risk","Risk of the baseline treatment you selected above",
                             step=0.000001,value = 0.15,max = 0.99999,min = 0.00001)
            )
        }
    })

    output$cost_number <- renderUI({
        if(input$cost=="Yes"){
            tagList(
                numericInput("cost1",paste0("Cost($) per treatment(",name_trt1(),")"),
                             step=0.000001,value = 2,min = 0.00001),
                numericInput("cost2",paste0("Cost($) per treatment(",name_trt2(),")"),
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
                          between selected two treatments by the previous network is ",
                          round(sigma_nma_old(),4))
          if(input$howrisk=='NMA'){
              str1 <- paste0("The risk of ",name_trt1()," estimated by the previous network is ",
                             round(baseline()$p1,4))
              str2 <- paste0("The risk of ",name_trt2()," estimated by the previous network is ",
                             round(baseline()$p2,4))
          }else{
              str1 <- NULL
              str2 <- NULL
          }
            
          HTML(paste(str, str1, str2, sep = '<br/>'))
        }
    })
    
    output$tabOutput <- function() {
        dat <- filedata()
        if(!is.null(dat)){
            sigma <- sigma_nma_old()
            power_level <- input$power_level
            
            risk1 <- ifelse(input$howrisk=='enter',input$risk1,baseline()$p1)
            risk2 <- ifelse(input$howrisk=='enter',input$risk2,baseline()$p2)
            
            samplesize_even = rep(SolveSampleSize_Withprev_equal(risk1,risk2,sigma,power_level)/2,2)
            samplesize = SolveSampleSize_Withprev(risk1,risk2,sigma,power_level)
            samplesize_single = SolveSampleSize_Single(risk1,risk2,power_level)
            samplesize_single_even = rep(SolveSampleSize_Single_equal(risk1,risk2,power_level)/2,2)
            
            output_dat <- data.frame(n1 = c(samplesize_even[1],samplesize[1],
                                            samplesize_single_even[1],samplesize_single[1]),
                                     n2 = c(samplesize_even[2],samplesize[2],
                                           samplesize_single_even[2],samplesize_single[2]))
            
            output_dat$total <- output_dat$n1+output_dat$n2

            collapse_rows_dt <- cbind(C1 = c(rep("with previous NMA", 2), rep("isolation", 2)),
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
 
        
}

shinyApp(ui = ui, server = server)

