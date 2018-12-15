library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(modelr)

#
# STEP 1: PREPARATION
#
# Reading data
#
unzip("data.zip")
ucenici17 <- read_csv("ucenici17.csv")
ucenici16 <- read_csv("ucenici16.csv")
ucenici15 <- read_csv("ucenici15.csv")
#minus označava drop -- efektivno, dropujemo kolone od likovno6 do bodovi_sa_prijemnog, 
#jer su u pitanju proseci koji se mogu izračunati. Nije nužno (ali štedi memoriju)
smerovi17 <- read_csv("smerovi17.csv") %>% select(-(likovno6:bodovi_sa_prijemnog)) 
smerovi16 <- read_csv("smerovi16.csv") %>% select(-(likovno6:bodovi_sa_prijemnog))
smerovi15 <- read_csv("smerovi15.csv") %>% select(-(likovno6:bodovi_sa_prijemnog))
osnovne17 <- read_csv("osnovne17.csv") %>% select(-(likovno6:bodovi_sa_prijemnog))
osnovne16 <- read_csv("osnovne16.csv") %>% select(-(likovno6:bodovi_sa_prijemnog))
osnovne15 <- read_csv("osnovne15.csv") %>% select(-(likovno6:bodovi_sa_prijemnog))
# renaming columns, in order not to clash names when joining them
names(ucenici17) <- paste0("uc.", names(ucenici17))
names(smerovi17) <- paste0("sm.", names(smerovi17))
names(osnovne17) <- paste0("os.", names(osnovne17))
names(ucenici16) <- paste0("uc.", names(ucenici16))
names(smerovi16) <- paste0("sm.", names(smerovi16))
names(osnovne16) <- paste0("os.", names(osnovne16))
names(ucenici15) <- paste0("uc.", names(ucenici15))
names(smerovi15) <- paste0("sm.", names(smerovi15))
names(osnovne15) <- paste0("os.", names(osnovne15))
uc_varlist <- names(ucenici17)
#
# Tab 2
# -------
# Using joins to combine (join) multiple tables
#
sve17 <- left_join(ucenici17, smerovi17, by=c("uc.upisana_id" = "sm.id")) %>% 
    left_join(osnovne17, by=c("uc.osnovna_id" = "os.id"))
sve16 <- left_join(ucenici16, smerovi16, by=c("uc.upisana_id" = "sm.id")) %>% 
    left_join(osnovne16, by=c("uc.osnovna_id" = "os.id"))
sve15 <- left_join(ucenici15, smerovi15, by=c("uc.upisana_id" = "sm.id")) %>% 
    left_join(osnovne15, by=c("uc.osnovna_id" = "os.id"))
rm(ucenici16, ucenici15, osnovne15, osnovne16, osnovne17, smerovi15, smerovi16, smerovi17)
#
# Standardizing values & putting everything into one huge tibble
#

# Behold. a function! (Braces are optional for one-line functions, it's just a matter of style)
standardize <- function(var, m=0, stdev=1) {
    m+stdev*(var-mean(var))/(sd(var)) # no need to use return -- last expression is a return value
}
# mutate creates new columns transforming existing (e.g. applying a function)
# also, %<>% is a pipe -- it takes a variable, passes it to a function, and updates it
# so a %<>% f(b,c,d) is equivalent to a <- f(a,b,c,d)
# yup, first parameter of mutate is a data frame (tibble)
sve17 %<>% mutate(uc.matematika_std = standardize(uc.matematika, 500, 100), 
                  uc.srpski_std = standardize(uc.srpski, 500, 100), 
                  uc.kombinovani_std = standardize(uc.kombinovani, 500, 100))
sve16 %<>% mutate(uc.matematika_std = standardize(uc.matematika, 500, 100), 
                  uc.srpski_std = standardize(uc.srpski, 500, 100), 
                  uc.kombinovani_std = standardize(uc.kombinovani, 500, 100))
sve15 %<>% mutate(uc.matematika_std = standardize(uc.matematika, 500, 100), 
                  uc.srpski_std = standardize(uc.srpski, 500, 100), 
                  uc.kombinovani_std = standardize(uc.kombinovani, 500, 100))
# we're creating a new column here directly
# $ is analogous to . in more conventional languages (i.e. it's a member access operator)
sve17$godina <- 17
sve16$godina <- 16
sve15$godina <- 15
# bind_rows just appends rows of one (or more) tibbles to another
sve <- bind_rows(sve17, sve16, sve15) 
# we're mutating variables in vars(...), by applying factor function to them
sve %<>% mutate_at(vars(godina, sm.podrucje, sm.mesto, sm.okrug, sm.ime, os.mesto, os.okrug), factor)
rm(sve17, sve16, sve15)
sve_varlist <- names(sve)
sve_factors <- names(sve %>% select_if(is.factor))
# helper -- evaluates string that's passed to it in a given context
evalText <- function(text, context) {
    eval(parse(text=text), context)
}


#
# =======================================================
# STEP 2: DOING STUFF
# =======================================================
#

server <- function(input, output, session) {
    # this is where the base filter is evaluated
    ucenici17f <- reactive({
        # %>% is also a pipe, but it doesn't assign a value to anything, just returns it
        # so a %>% f(b) equals f(a,b)
        ucenici17 %>% filter(if(input$base_filter=="") TRUE else evalText(input$base_filter, ucenici17))
        # in case you haven't noticed, everything is an expression, so we can use if-else
        # like we'd use a ternary operator in C family of languages
    })
    
    # 
    # ---------------------------------------
    # Tab 1: Basic plots
    # ---------------------------------------
    #
    
    output$basic_plot <- renderPlot({
        data <- ucenici17f()
        pos <- if(input$p1_jitter) "jitter" else "identity"
        x <- evalText((input$var_x), data)
        y <- evalText((input$var_y), data)
        if(!input$p1_col)
            ggplot(data, aes(x, y)) + geom_point(alpha=input$p1_alpha, position=pos) + 
                xlab(input$var_x) + ylab(input$var_y)
        else
            ggplot(data, aes(x, y, color=evalText(input$p1_col_var, data))) +
                geom_point(alpha=input$p1_alpha, position=pos) + 
                scale_colour_gradient(name=input$p1_col_var, low="red", high="blue") +
                xlab(input$var_x) + ylab(input$var_y)
    })
    
    # this, but in 2d: https://mathisonian.github.io/kde/
    output$kde2d <- renderPlot({
        data <- ucenici17f()
        x <- evalText((input$var_x), data)
        y <- evalText((input$var_y), data)
        ggplot(data, aes(x, y)) + xlab(input$var_x) + ylab(input$var_y) + 
            stat_density_2d(geom="point", aes(size = stat(density)), n = input$kde2d_n, contour = FALSE)
    })
    
    # 
    # ---------------------------------------
    # Tab 2: Faceting (and joins)
    # ---------------------------------------
    #
    
    output$join1 <- renderPlot({
        filt_var <- input$join1_filter_var
        filt_in <- input$join1_filter_val
        col <- input$join1_col
        svef <- sve %>%
                  filter(if(input$base_filter=="") TRUE else evalText(input$base_filter, .)) %>%
                  filter(if(is.null(filt_in) || filt_var == "") FALSE else (evalText(filt_var, .) %in% filt_in))
        x <- evalText((input$var_x), svef)
        y <- evalText((input$var_y), svef)
        pos <- if(input$join1_jitter) "jitter" else "identity"
        ggplot(svef, aes(x, y, color=evalText(col, svef))) + geom_point(alpha=input$join1_alpha, position=pos) + 
            facet_grid(rows = reformulate(".", filt_var)) + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))) + scale_color_discrete(name=col) +
            xlab(input$var_x) + ylab(input$var_y)
    })
    
    # 
    # ---------------------------------------
    # Tab 3: Linear model
    # ---------------------------------------
    #
    
    # This is where model is built
    model <- reactive({
        if(input$tabs == "models") { # We'll build a model only if tab 3 is showing
            model <- lm(reformulate(input$var_x, input$var_y), (ucenici17f()))
        } else {
            NA 
        }
    })
    
    output$model_regline <- renderPlot({
        data <- ucenici17f()
        x <- evalText((input$var_x), data)
        y <- evalText((input$var_y), data)
        pos <- if(input$model_rl_jitter) "jitter" else "identity"
        grid <- data %>% data_grid(!!input$var_x := x) %>% #this particular evil is called quasiquotation
            add_predictions(model())
        ggplot(data, aes(x, y)) + geom_point(alpha=input$model_rl_alpha, position=pos) +
            geom_line(aes(grid[[1]], grid[[2]]), data=grid, color="red", size=1.5) +
            xlab(input$var_x) + ylab(input$var_y)
    })
    
    output$model_freqpoly <- renderPlot({
        data <- ucenici17f() %>%
            select(input$var_x, input$var_y) %>% add_residuals(model())
        ggplot(data, aes(resid)) + geom_freqpoly(binwidth=input$model_freq_binwidth)
    })
    
    output$model_resid <- renderPlot({
        data <- ucenici17f()  %>%
            select(input$var_x, input$var_y) %>% add_residuals(model())
        x <- evalText((input$var_x), data)
        pos <- if(input$model_res_jitter) "jitter" else "identity"
        ggplot(data, aes(x, resid)) + geom_ref_line(h=0) + 
            geom_point(position=pos, alpha=input$model_res_alpha) + xlab(input$var_x)
    })
    
    
    # 
    # ---------------------------------------
    # Misc: observers
    # These get fired whenever something happens that changes variables they use
    # ---------------------------------------
    #
    
    observe({
        if(!input$p1_col)
            updateSelectInput(session, "p1_col_var", choices = character(0))
        else
            updateSelectInput(session, "p1_col_var", choices = uc_varlist)
    })
    
    observe({
        selx <- input$var_x
        sely <- input$var_y
        if(input$tabs == "basic" || input$tabs == "models") {
            updateSelectInput(session, "var_x", choices = uc_varlist, selected = selx)
            updateSelectInput(session, "var_y", choices = uc_varlist, selected = sely)
        } else if(input$tabs == "joins") {
            updateSelectInput(session, "var_x", choices = sve_varlist, selected = selx)
            updateSelectInput(session, "var_y", choices = sve_varlist, selected = sely)
        }
    })
    
    observe({
        join_choices <- levels(evalText(input$join1_filter_var, sve))
        updateSelectInput(session, "join1_filter_val", choices = join_choices, selected = join_choices[1])
    })
}


#
# =======================================================
# STEP 3: LAYING EVERYTHING OUT
# =======================================================
#
# This is where user interface is built
# note this is a function call -- everything inside is a parameter to fluidPage() !
# (we're not really going to discuss this much)
ui <- fluidPage(
    
    # Application title
    titlePanel("Nedelja informatike — Eksplorativna analiza podataka"),
    
    fluidRow(
        column(6, selectInput("var_x", "X", choices = uc_varlist, selected = "uc.matematika_p")),
        column(6, selectInput("var_y", "Y", choices = uc_varlist, selected = "uc.matematika"))
    ),
    
    fluidRow(
        column(12, textAreaInput("base_filter", "Filter", width="100%", rows=1))
    ),
    
    tabsetPanel(id="tabs",
                
        tabPanel("Basic plots", value="basic",
                 
                 h3("Scatter plot"),
                 
                 fluidRow(
                     column(8, plotOutput("basic_plot")),
                     column(4, wellPanel(
                                sliderInput("p1_alpha", "Transparentnost (alpha):",
                                            min = 0, max = 1, value = 1),
                                checkboxInput("p1_jitter", "Jitter"),
                                checkboxInput("p1_col", "Boja"),
                                selectInput("p1_col_var", "Boja", choices = character(0))
                            ))
                 ),
                 
                 h3("Kernel Density Estimate"),
                 
                 fluidRow(
                     column(8, plotOutput("kde2d")),
                     column(4, wellPanel(
                                sliderInput("kde2d_n", "KDE n",
                                            min = 0, max = 20, value = 10)
                            ))
                 )
        ),
        
        tabPanel("Faceting", value="joins",
               fluidRow(
                   column(8, plotOutput("join1", height = 600)),
                   column(4, wellPanel(
                       sliderInput("join1_alpha", "Transparentnost (alpha):",
                                   min = 0, max = 1, value = 0.07),
                       checkboxInput("join1_jitter", "Jitter", value = TRUE),
                       selectInput("join1_filter_var", "Prikaži samo ako je", choices = sve_factors, selected = "sm.podrucje"),
                       selectInput("join1_filter_val", "jedan od", choices = levels(sve$sm.podrucje), selected = "gimnazija", multiple = TRUE),
                       selectInput("join1_col", "Boja", choices = sve_factors, selected = "sm.podrucje")
                   ))
               )  
        ),
        
        tabPanel("Models", value="models",
                 
                 h3("Regression line"),
                 
                 fluidRow(
                     column(8, plotOutput("model_regline")),
                     column(4,  wellPanel(
                         sliderInput("model_rl_alpha", "Transparentnost (alpha):",
                                     min = 0, max = 1, value = 0.04),
                         checkboxInput("model_rl_jitter", "Jitter", value = TRUE)
                     ))
                 ),
                 
                 h3("Residual count"),
                 
                 fluidRow(
                     column(8, plotOutput("model_freqpoly")),
                     column(4, wellPanel(
                         sliderInput("model_freq_binwidth", "Binwidth",
                                     min = 0.01, max = 3, value = 0.33)
                     ))
                 ),
                 
                 h3("Residual distribution"),
                 
                 fluidRow(
                     column(8, plotOutput("model_resid")),
                     column(4, wellPanel(
                         sliderInput("model_res_alpha", "Transparentnost (alpha):",
                                     min = 0, max = 1, value = 0.05),
                         checkboxInput("model_res_jitter", "Jitter", value = TRUE)
                     ))
                )
        )
    )
)

#
# Finally, run the app!
#
shinyApp(ui = ui, server = server)
