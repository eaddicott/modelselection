# Interactive RShiny App to Accompany ``Toward an improved understanding of causation in global change science''
# September 7, 2020

# Load Packages -----


library(shiny)
library(ggplot2)
library(DT)
library(reshape2)
library(cowplot)
library(scales)

# UI Outline Head ----- 
ui <- navbarPage(title = "Causal Inference in Ecology", id = "inTabset", fluid = TRUE,
                 # Tab Panel 1 ####
                 {tabPanel("Introduction", id = "intro",
                 titlePanel("Informing the Use of Information Criteria"), 
                          fluidRow(column(12,align='center',img(src = "fish.jpg", height = 400, width = 600,align='center'))),
                                   fluidRow(column(12,align='center',
                                    p(" \"Fish\"by Mathias Appel is licensed under CC BY-NC 2.0", 
                                      style="font-size: 8pt"))),
                 fluidRow(column(10, offset = 1,
                                    
                                    p("Society increasingly demands accurate predictions of complex ecosystem processes 
                                      under novel conditions to address environmental challenges. Obtaining the process-level 
                                      knowledge required to do so does not necessarily align with the burgeoning use in 
                                      ecology of correlative model selection criteria such as Akaike's Information Criterion (AIC). 
                                      These criteria select models based on their ability to reproduce outcomes, not their 
                                      accurate representation of causal effects. Causal understanding does not require 
                                      matching outcomes. It involves identifying model forms and parameter values that 
                                      accurately describe processes. We argue that researchers can reach incorrect 
                                      conclusions about cause-and-effect relationships by relying on information criteria. 
                                      This app is designed to walk through a concrete example involving a population of fish
                                      where inference extending beyond prediction into causality is badly misled by AIC.", style="font-size: 12pt")
                                    )),
                 fluidRow(column(1, offset = 1,
                          actionButton('totruemodel','Next: True Model')))
                          )
                     },
                 # Tab Panel 2 ####
                 {tabPanel("True Model", value = "truemodel", withMathJax(),
                                     tags$script("MathJax.Hub.Config({
                                            tex2jax: {inlineMath: [['$','$']], processEscapes: true
                                            }
                                            });"
                                     ),
                                     titlePanel("Motivating Example"),
                                     
                                     mainPanel(withMathJax("Suppose there exists a simple system where the $Biomass$ of a fish species 
                                     at a given site is determined by the sum of the $Habitat$ suitability index and the exploitation rate, $ER$, 
                                     of the fish. The researcher can observe habitat suitability but cannot observe the exploitation rate."),
                                               br(),
                                               p("The more suitable the habitat, the greater the biomass of fish at the site."),
                                               br(),
                                               p("The greater the exploitation rate, the lower the biomass at the site."),
                                               br(),
                                               p("The true model is given by"),
                                               withMathJax("$$Biomass_i = \\alpha + \\beta Habitat_i + \\gamma ER_i + \\eta _i$$"),
                                     #           withMathJax("with parameter $\\beta=2$, the coefficient
                                     # on the measure of habitat suitability,"),
                                     #           br(),
                                     #           withMathJax("parameter $\\gamma=0.8$ as the coefficient on the unobserved 
                                     # value of the exploitation rate,"),
                                     # br(),
                                     # withMathJax("and a normally distributed error term, $\\eta$, with variance 0.01."),
                                     br(),
                                     h3("Generating the Dataset"),
                                     br(),
                                     helpText(withMathJax("To generate the dataset for this example, input parameter values of your choice for $\\beta,\\gamma,$ and $\\eta$ below. 
                                                 You can also set the number of observations in the dataset and record the seed for replication of your dataset.")),
                                     fluidRow(
                                         column(3,
                                     numericInput('userbeta',withMathJax("$\\beta$"), value = 2)),
                                         column(3,
                                     numericInput('usergamma',withMathJax("$\\gamma$"), value = 0.8)),
                                         column(3,
                                     numericInput('usereta',withMathJax("$\\eta$"), value = 0.01))),
                                     fluidRow(
                                         column(4,
                                     numericInput('userseed','Set Seed: ', value = 1, min = 1, step = 1)),
                                         column(4,
                                     numericInput('usern','Set Sample Size: ', value = 1000, min = 25, max = 2000))),
                                     fluidRow(column(4,selectInput("xaxis", label = "Choose x-axis", choices = list("Habitat","ER_Unobserved","Biomass"), selected = "Habitat")),
                                              column(4,selectInput("yaxis", label = "Choose y-axis", choices = list("Habitat","ER_Unobserved","Biomass"), selected = "Biomass"))),
                                     fluidRow(column(3,actionButton('submitparams','Generate/Plot Dataset'))),
                                     fluidRow(
                                         column(3, offset = 7, actionButton('tobadmodel','Next: Mis-specified Model'))),
                                     plotOutput("firstLook")
                                     ))
                 },
                 navbarMenu("Model 1",
                            
                    {tabPanel("Model 1: Mis-specified", value = "badmodel",withMathJax(),
                             tags$script("MathJax.Hub.Config({
                                            tex2jax: {inlineMath: [['$','$']], processEscapes: true
                                            }
                                            });"
                             ),
                             titlePanel("Mis-specified Model"),
                
                             mainPanel(withMathJax("Since $ER$ is unobservable, suppose the researcher can observe an estimate of the exploitation rate, $\\hat{ER}$,
                                                   that is dependent on the true $ER$ and $Biomass$ at the site in the following way that is unknown to the researcher:"),
                                                   br(),
                                                   withMathJax("$$\\hat{ER}_{i} = ER_{i} +\\sqrt{Biomass_{i}}$$"),
                                                   br(),
                                                   p("The researcher estimates the following linear model given the data they observe:"),
                                                    br(),
                                                    withMathJax("$$Biomass_i= \\alpha + \\beta Habitat_{i} + \\gamma \\hat{ER_{i}} + \\epsilon_{i}$$"),
                                       helpText("Let's run the Mis-specified Model above by clicking the Run Regression Button"),
                                       fluidRow(column(5,offset = 2, actionButton('showreg','Run Regression'))),
                                       fluidRow(column(3,withMathJax("$\\hat{\\beta} $")), column(3,withMathJax("$\\hat{\\gamma}$"))),
                                       fluidRow(column(3,textOutput("badModelOutb")),column(3,textOutput("badModelOutg"))),
                                       p("Notice that the parameter estimates are significant, and NOT the same as the one's we specified in the data generating process (i.e. the TRUE parameters). "),
                                       fluidRow(column(3, offset = 7, actionButton('tobad2','Next: Mis-specified Model Fit')))))
                             },
                    {tabPanel("Model 1: Fit and Residuals", value = "badmodelfit",withMathJax(),
                    tags$script("MathJax.Hub.Config({
                                            tex2jax: {inlineMath: [['$','$']], processEscapes: true
                                            }
                                            });"
                    ),
                    titlePanel("Mis-specified Model Fit"),
                    mainPanel(p("For model fit, we can look at a plot of actual vs. predicted biomass at each site"),
                                       plotOutput("fig2"),
                                       withMathJax("The $R^{2}$ for the model is"),textOutput("rsq"),
                                       p("However, the residuals plot provides further information that may call into question the underlying assumptions implied by estimating the linear model"),
                                       plotOutput("fig3"),
                                       p("Any discrepancy in the estimated parameters is not due to lack of data, or precision. 
                                         The next plot compares the bootstrapped estimates over increasing sample size with the true 
                                         values we declared at the outset (dashed lines)."),
                                       plotOutput("fig4"),
                                       fluidRow(
                                           column(3, offset = 7, actionButton('toiv','Next: Instrumental Variable')))
                                       
                             )
                             )}),
                
                 {tabPanel("Model 2: Instrumental Variable", value = "ivmodel",withMathJax(),
                         tags$script("MathJax.Hub.Config({
                                            tex2jax: {inlineMath: [['$','$']], processEscapes: true
                                            }
                                            });"
                         ),
                         titlePanel("Instrumental Variables Model"),
                         
                         mainPanel(
                             p("How can we solve the issue presented by the mis-specified model?"),
                             withMathJax("As as a first approach we just need to find some way to come 
                                         up with a measure of $ER$ that is independent of $Biomass$."),
                             br(),
                             withMathJax("Let's estimate a first-stage model where $$ER = \\psi*Nets + \\nu$$,"),
                             br(),
                             withMathJax("$Nets$ are data we have on the purchases of fishing nets by vessels at a site in the leading year
                                         that we think is correlated with $ER$, but uncorrelated with $Biomass$."),
                             br(),
                             fluidRow(column(5,offset = 2, actionButton('runIV','Predict ER using Instrument'))),
                             br(),
                             withMathJax("We predict $ER$ using $Nets$, which creates a variable we can use in the second-stage that is not confounded."),
                          
                         plotOutput("iv_predict"),
                         br(),
                         p("The instrumental variable approach then produces estimates of the habitat parameter that are now unbiased and approach the true value as the sample size increases."),
                         br(),
                         fluidRow(column(3,withMathJax("$\\hat{\\beta} $ = ")),textOutput("IVModelOutb")),
                         plotOutput("fig5"),
                         p("Further, the residuals plot does not suggest heteroskedastic error as before."),
                         br(),
                         plotOutput("fig6"),
                         fluidRow(
                             column(3, offset = 7, actionButton('toconc','Next: Conclusion')))
                         ))},
                 
                 tabPanel("Conclusion", value="conclusion", titlePanel("Conclusion"), 
                 mainPanel(
                 p("When we evaluate the model performance using AIC we find that the first model -- with all of its endogeneity -- is a better fit to the data and, using AIC alone, one would prefer it over the second."),
                 br(),
                 fluidRow(column(3, actionButton('AIC','Evaluate Model AIC'))),
                 fluidRow(column(5,"The mis-specified model's AIC is" ), column(3,textOutput("naive.AIC"))),
                 fluidRow(column(5,"The IV model's AIC is" ), column(3,textOutput("s.AIC"))),
                 br(),
                 fluidRow(column(5,"The difference in AIC values is" ), column(3,textOutput("delta.AIC"))),
                 br(),
                 p("This motivating example shows that AIC lends weight to the wrong model with parameter estimates that miss the mark 
                 with high confidence across all explanatory variables. Since most research endeavors are aimed at both outcomes and the processes that generate them, 
                 information theoretic evidence should be used with care. Confidence in effect sizes derived solely from such evidence 
                 should not be given weight and instead strategies suited to identify parameter values, such as proffering robustness 
                 of values across specifications, should be employed.")
                 )))


# Define server logic ####
server <- function(input, output, session) {
    #Next Button 1 ####
    observeEvent(input$totruemodel, 
                 {updateTabsetPanel(session, "inTabset", selected = "truemodel")
                 })
    #Next Button 2a and b ####
    observeEvent(input$tobadmodel,
                 {updateTabsetPanel(session, "inTabset", selected = "badmodel")
                 })
    observeEvent(input$tobad2,
                 {updateTabsetPanel(session, "inTabset", selected = "badmodelfit")
                 })
    #Next Button 3  #####
    observeEvent(input$toiv,
                 {updateTabsetPanel(session, "inTabset", selected = "ivmodel")
                 })
    #Next Button 4 #####
    observeEvent(input$toconc,
                 {updateTabsetPanel(session, "inTabset", selected = "conclusion")
                 })
    # Submit Button Gen Dataset ####
    observeEvent(input$submitparams, getTrueData(input$userseed,input$usern, input$userbeta,input$usergamma,input$usereta,input$xaxis,input$yaxis))
    
    observeEvent(input$showreg, {
        getBadModel(input$userseed,input$usern, input$userbeta,input$usergamma,input$usereta,input$xaxis,input$yaxis)
        se <- round(sqrt(diag(vcov(badmd))),digits = 3)
        fit <- summary(badmd)
        output$badModelOutb<- renderText(paste(round(coefficients(badmd)[[2]],digits = 3),"\u00B1",se[2]))
        output$badModelOutg<- renderText(paste(round(coefficients(badmd)[[3]],digits = 3),"\u00B1",se[3]))
        output$rsq <- renderText(as.character(fit$r.squared))
        })
    observeEvent(input$runIV, {
        getIVModel(input$userseed, input$usern, input$userbeta,input$usergamma,input$usereta)
        se <- round(sqrt(diag(vcov(ivmd))),digits = 3)
        output$IVModelOutb<- renderText(paste(round(coefficients(ivmd)[[2]],digits = 3),"\u00B1",se[2]))
    })
    observeEvent(input$AIC, {
            naive.AIC <- extractAIC(badmd)[2]
            s.AIC     <- extractAIC(ivmd)[2]
            d.AIC <- abs(naive.AIC-s.AIC)
        output$naive.AIC<- renderText(naive.AIC)
        output$s.AIC <- renderText(s.AIC)
        output$delta.AIC <- renderText(d.AIC)
    })
    set.seed(1)
    #Initial Dataset Creation ####
    n <- 1000  # number of data points to construct
    error.var.1 <- 0.01   #variance of the idiosyncratic error term
    trueData <- reactiveValues()
    coeff <- reactiveValues()
    upars <- reactiveValues()
    #getTrueData(1,1000,2,0.8,0.01)
    getTrueData <- function(seed, ssize,ub,ug,ue,haxis,vaxis){
        
        set.seed(floor(seed))
        upars$n <- ssize
        
        trueData$habitat <- (sample.int(101, size = upars$n, replace = TRUE) - 1) / 100 #random values for habitat
        trueData$er.exog.pt <- (sample.int(101, size = upars$n, replace = TRUE) - 1) / 100 #and for ER_exog
        trueData$error.normal <- rnorm(upars$n,0,ue)
        
        trueData$biomass <- ub*trueData$habitat + ug*trueData$er.exog.pt + trueData$error.normal #construct biomass values
        trueData$er <- trueData$er.exog.pt + sqrt(abs(trueData$biomass)) #construct observed ER
        
        #Create a dataframe
        dfTrue <- data.frame("Biomass" = trueData$biomass, "Habitat" = trueData$habitat, "ER_Unobserved" = trueData$er.exog.pt)
        df <- data.frame("Biomass" = trueData$biomass, "Habitat" = trueData$habitat, "ER_Observed" = trueData$er)
        
        #Figure 1 Output First Look #####
        output$firstLook <- renderPlot({
            ggplot(data = dfTrue, aes(x = get(haxis), y = get(vaxis))) + geom_point() + xlab(haxis) + ylab(vaxis) + theme_classic()
        })
    }
    getBadModel <- function(seed, ssize,ub,ug,ue,haxis,vaxis){
        set.seed(floor(seed))
        upars$n <- ssize
        
        trueData$habitat <- (sample.int(101, size = upars$n, replace = TRUE) - 1) / 100 #random values for habitat
        trueData$er.exog.pt <- (sample.int(101, size = upars$n, replace = TRUE) - 1) / 100 #and for ER_exog
        trueData$error.normal <- rnorm(upars$n,0,ue)
        
        trueData$biomass <- ub*trueData$habitat + ug*trueData$er.exog.pt + trueData$error.normal #construct biomass values
        trueData$er <- trueData$er.exog.pt + sqrt(abs(trueData$biomass)) #construct observed ER
        
        #Create a dataframe
        dfTrue <- data.frame("Biomass" = trueData$biomass, "Habitat" = trueData$habitat, "ER_Unobserved" = trueData$er.exog.pt)
        df <- data.frame("Biomass" = trueData$biomass, "Habitat" = trueData$habitat, "ER_Observed" = trueData$er)
        trueData$naive.md <- lm(trueData$biomass ~  trueData$habitat + trueData$er, data = df)
        assign("badmd",trueData$naive.md, envir = .GlobalEnv)
        trueData$naive.bio.pred <- predict(trueData$naive.md)
        
        df1 <- data.frame(cbind(trueData$habitat, trueData$biomass,trueData$naive.bio.pred))
        names(df1) <- c("Habitat","Actual","Predicted")
        df1.melt <- melt(df1, id.vars = "Habitat", variable.name= "Type", value.name = "Biomass")
        
        coeff$b <- rep(NA,upars$n - 9)
        coeff$g <- rep(NA,upars$n - 9)
        
        #This is where we can make the program run lighter
        for (j in seq(10,upars$n)) {
            trueData$md <- lm(Biomass ~ Habitat + ER_Observed, data = df[1:j,])
            coeff$b[j - 9] <- coefficients(trueData$md)[[2]]
            coeff$g[j - 9] <- coefficients(trueData$md)[[3]]
        }
        obs <- seq(10,upars$n)
        df2 <- data.frame(cbind(obs,coeff$b,coeff$g))
        output$truedatatable <- DT::renderDataTable(DT::datatable(df))
        output$badmodelsummary<- renderPrint(summary(trueData$naive.md))
        output$fig2 <- renderPlot({
            ggplot(df1.melt, aes(x = Habitat, y= Biomass, shape = Type)) + 
                geom_point(aes(colour = Type)) + 
                scale_shape_manual(values = c('Actual' = 6, 'Predicted' = 4)) + 
                scale_colour_manual(values = c('Actual' = 'grey','Predicted' = 'black')) + 
                theme_classic() + xlab('Habitat') + ylab('Biomass')        
        })
        output$fig3 <- renderPlot(ggplot(df1, aes(x = Predicted, y = Actual - Predicted)) + geom_point() + theme_classic() + xlab('Fitted') + ylab('Residuals'))
        output$fig4 <- renderPlot({
            beta1plot <- ggplot(df2, aes(x = obs, y = coeff$b)) + geom_line() + 
                geom_hline(yintercept=ub,linetype = 'dashed') + 
                theme_classic() + xlab('Observations') + ylab(expression(beta*": Estimate of Habitat Parameter")) 
            beta2plot <- ggplot(df2, aes(x = obs, y = coeff$g)) + geom_line()+ 
                geom_hline(yintercept=ug, linetype = 'dashed') +
                theme_classic() + xlab('Observations') + ylab(expression(gamma*": Estimate of Exploitation Rate Parameter"))  
            # + scale_x_continuous(expand = c(0, 0), limits = c(0, 1025), labels=comma) + scale_y_continuous(expand = c(0, 0), limits = c(0.5, 1))
            cowplot::plot_grid(beta1plot,beta2plot, labels = c('a','b'))
        })
    }
    getIVModel  <- function(seed, ssize,ub,ug,ue){
        set.seed(floor(seed))
        upars$n <- ssize
        
        trueData$habitat <- (sample.int(101, size = upars$n, replace = TRUE) - 1) / 100 #random values for habitat
        trueData$er.exog.pt <- (sample.int(101, size = upars$n, replace = TRUE) - 1) / 100 #and for ER_exog
        trueData$error.normal <- rnorm(upars$n,0,ue)
        
        trueData$biomass <- ub*trueData$habitat + ug*trueData$er.exog.pt + trueData$error.normal #construct biomass values
        trueData$er <- trueData$er.exog.pt + sqrt(abs(trueData$biomass)) #construct observed ER
        
        #Create a dataframe
        dfTrue <- data.frame("Biomass" = trueData$biomass, "Habitat" = trueData$habitat, "ER_Unobserved" = trueData$er.exog.pt)
        
        error.var.2 <- .25
        trueData$fishnets <- .04*trueData$er.exog.pt + rnorm(upars$n,0,error.var.2)
        
        df <- data.frame("Biomass" = trueData$biomass, "Habitat" = trueData$habitat, "ER_Observed" = trueData$er, "Nets" = trueData$fishnets)
        f.stg <- lm(ER_Observed ~ Nets, data = df)
        fstgout <- summary(f.stg)
        f.stg.coeff <- coefficients(f.stg)
        IVb <- f.stg.coeff[1]
        df$ER_hat <- predict(f.stg)
        output$iv_predict <- renderPlot({
            ggplot(df) + geom_line(aes(x=Nets,y=ER_hat)) + geom_point(aes(x=Nets, y=ER_Observed)) + theme_classic()})
        
        s.stg <- lm(Biomass ~ Habitat + ER_hat, data = df)
        sstgout <- summary(s.stg)
        s.stg.beta <- coefficients(s.stg)[2]

        coeff$b <- rep(NA,upars$n - 9)
        assign("ivmd",s.stg, envir = .GlobalEnv)
        for (j in seq(10,upars$n)) {
            trueData$mdiv <- lm(Biomass ~ Habitat + ER_hat, data = df[1:j,])
            coeff$b[j - 9] <- coefficients(trueData$mdiv)[[2]]
            coeff$g[j - 9] <- coefficients(trueData$mdiv)[[3]]
        }
        obs <- seq(10,upars$n)
        df3 <- data.frame(cbind(obs,coeff$b))
        output$fig5 <- renderPlot({
            ggplot(df3, aes(x=obs,y=coeff$b)) + geom_line() + 
                geom_hline(yintercept = ub, linetype = 'dashed') + 
                theme_classic() + xlab('Observations') + ylab(expression(beta*": Estimate of Habitat Parameter")) 
        })


        trueData$biomass.pred <- predict(s.stg)
        trueData$residual <- trueData$biomass.pred - trueData$biomass
        df4 <- data.frame(cbind(trueData$biomass.pred, trueData$residual))
        
        output$fig6 <- renderPlot({  ggplot(df4, aes(x=trueData$biomass.pred, y=trueData$residual)) + geom_point() + theme_classic() + 
             xlab('Predicted Biomass') + ylab('Residual')
        
            })
    }
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)


