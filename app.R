library("netmeta")
library("NlcOptim")
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
                         
                         textOutput("Test"),
                         
                         # Horizontal line ----
                         tags$hr(),
                         h4("Step2: Select two treatments in your future trial"),
                         h6("This part would appear after the data with correct format is uploaded"),
                         conditionalPanel(condition = "!is.null(input.file1)",
                                          uiOutput("treatment1")),
                         conditionalPanel(condition = "!is.null(input.file1)",
                                          uiOutput("treatment2"))
                    
                  ),
                  
                  column(4,style="background-color:seashell",
                         h4("Step3: Confirm the parameters"),
                         radioButtons("howrisk",
                                      "Which would you use to represent for the risks of the two treatments?",
                                      c("I have other evidence or references to use and I want to enter the risks by myself" = "enter",
                                        "I would like to use the effect size (log odds ratio) estimated by previous NMA and enter the baseline risk by myself" = "NMA"),
                                      selected = "enter"),
                         uiOutput("Risk1"),
                         uiOutput("Risk2"),
                         uiOutput("Baseline"),
                         uiOutput("help_base"),
                         uiOutput("BaselineRisk"),
                         numericInput("power_level","Predefind Power",step=0.000001,value = 0.8,max = 0.99999,min = 0.1)
                         ),
                  
                  column(4,style="background-color:aliceblue",
                         h4("Information from the previous network"),
                         uiOutput("txtOutput2"),
                         uiOutput("RiskOutput"),
                         tags$hr(),
                         h4("The optimal sample size for each treatment in a future trial"),
                         tableOutput("tabOutput"))
              ))

server <- function(input, output,session) {
    
    arm <- reactive({
        if(!is.null(input$file1)){
            inFile <- input$file1
            dat <- read.csv(inFile$datapath)
            nma_old <- netmeta(TE,seTE,treat1,treat2,studlab,data=dat,
                               sm="OR",comb.fixed = T,comb.random = F)
            arms <- unique(c(dat$treat1,dat$treat2))
            list(nma_old = nma_old, arms = arms)
        }   
    })
    
    baseline <- reactive({
        if(!is.null(input$file1) && input$howrisk=='NMA'){
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
        if(!is.null(input$file1)){
            nma_old <- arm()$nma_old
            trt1 <- input$choice1
            trt2 <- input$choice2
            nma_old$seTE.fixed[trt1,trt2]
        }   
    })
    
    output$treatment1 <- renderUI({
        if(!is.null(input$file1)){
            arms <- arm()$arms
            radioButtons(inputId = "choice1",
                         label = "Please select the first treatment in your future trial",
                         choices = arms,
                         selected = character(0))
        }
    })

    output$treatment2 <- renderUI({
        if(!is.null(input$file1)){
            arms <- arm()$arms
            radioButtons(inputId="choice2", label="Please select the second treatment in your future trial",
                         choices=setdiff(arms,input$choice1),
                         selected = character(0))
        }
    })
    
    
    output$Risk1 <- renderUI({
        if(input$howrisk=='enter'){
            numericInput("risk1","Risk of 1st treatment you selected in Step2",
                         step=0.000001,value = 0.20,max = 0.99999,min = 0.00001)
        }
    })
    
    output$Risk2 <- renderUI({
        if(input$howrisk=='enter'){
            numericInput("risk2","Risk of 2nd treatment you selected in Step2",
                         step=0.000001,value = 0.25,max = 0.99999,min = 0.00001)
        }
    })
    
    output$Baseline <- renderUI({
        if(!is.null(input$file1) && input$howrisk=='NMA'){
            arms <- arm()$arms
            radioButtons(inputId="baseline_trt", 
                         label="Please select the baseline treatment in your previous NMA",
                         choices=arms)
        }
    })
    
    output$help_base <- renderUI({
        if(!is.null(input$file1) && input$howrisk=='NMA'){
            helpText("Note: you can choose any treatment in your upload file as baseline treatment 
               as long as you can get an estimation or observed result of the risk of the baseline 
               treatment you seleced")
        } 
    })
    
    output$BaselineRisk <- renderUI({
        if(!is.null(input$file1) && input$howrisk=='NMA'){
            numericInput("baseline_risk","Risk of the baseline treatment you selected above",
                         step=0.000001,value = 0.15,max = 0.99999,min = 0.00001)
        }
    })
    
    output$txtOutput2 = renderUI({
        if(!is.null(input$file1)){
            sigma <- sigma_nma_old()
            withMathJax(
                h6(paste0("Standard error of the estimated effect size 
                          between selected two treatments by the previous network is "),
                   round(sigma,4))
            )
        }
        })
    
    output$RiskOutput  = renderUI({
        if(!is.null(input$file1) && input$howrisk=='NMA'){
            p1 <- baseline()$p1
            p2 <- baseline()$p2
            
            withMathJax(
                h6(paste0("The risk of treatment 1 estimated by the previous network is"),
                   round(p1,4)),
                h6(paste0("The risk of treatment 2 estimated by the previous network is "),
                   round(p2,4))
            )
            
        }
    })
    
    output$tabOutput = renderTable({
        if(!is.null(input$file1)){
            sigma <- sigma_nma_old()
            power_level <- input$power_level
            p1 <- ifelse(input$howrisk=='enter',input$risk1,baseline()$p1)
            p2 <- ifelse(input$howrisk=='enter',input$risk2,baseline()$p2)
            
            samplesize_even = rep(SolveSampleSize_Withprev_equal(p1,p2,sigma,power_level)/2,2)
            samplesize = SolveSampleSize_Withprev(p1,p2,sigma,power_level)
            output_dat <- data.frame(Yes=c(samplesize_even,sum(samplesize_even)),
                                     No=c(samplesize,sum(samplesize)))
            rownames(output_dat) <- c("treatment 1","treatment 2","Total")
            colnames(output_dat) <- c("the optimal sample size with the condition of even",
                                      "the optimal sample size without the condition of even")
            output_dat
        }
    },
    rownames = TRUE,
    digits = 0)
        
}

shinyApp(ui = ui, server = server)

