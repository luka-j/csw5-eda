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
ucenici17$kombinovani_p <- (ucenici17$geografija_p+ucenici17$istorija_p+ucenici17$biologija_p+ucenici17$fizika_p+ucenici17$hemija_p)/5
ucenici16$kombinovani_p <- (ucenici16$geografija_p+ucenici16$istorija_p+ucenici16$biologija_p+ucenici16$fizika_p+ucenici16$hemija_p)/5
ucenici15$kombinovani_p <- (ucenici15$geografija_p+ucenici15$istorija_p+ucenici15$biologija_p+ucenici15$fizika_p+ucenici15$hemija_p)/5
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
# helper function -- evaluates string that's passed to it in a given context
evalText <- function(text, context) {
    eval(parse(text=text), context)
}


#
# =======================================================
# STEP 2: DOING STUFF
# =======================================================
#

server <- function(input, output, session) {
    ucenici17f <- reactive({
        # %>% is also a pipe, but it doesn't assign a value to anything, just returns it
        # so a %>% f(b) equals f(a,b)
        tryCatch({
            # if everything's fine, return this
            ucenici17 %>% filter(if(input$base_filter=="") TRUE else evalText(input$base_filter, ucenici17))
        }, warning = function(w) {
            # if shit seems to be happening, return empty tibble
            tibble()
        }, error = function(e) {
            # if shit happens, return empty tibble
            tibble()
        })
        # in case you haven't noticed, R does stuff differently
        # not only tryCatch is a function, but also if-else is an expression (like ?: is C-like langs)
    })
    ucenici17f %<>% debounce(750) # delay refresh time
    
    # 
    # ---------------------------------------
    # Tab 1: Basic plots
    # ---------------------------------------
    #
    
    output$basic_plot <- renderPlot({
        data <- ucenici17f()
        if(count(data) == 0) return(ggplot(data)) # if data is empty (e.g. invalid filter), return early
        
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
    output$basic_plot.ui <- renderUI(plotOutput("basic_plot", height=input$plot_height))
    # only reason renderUI is needed is for dynamic height of plots
    
    output$kde <- renderPlot({
        data <- ucenici17f()
        if(count(data) == 0) return(ggplot(data))
        
        x <- evalText((input$var_x), data)
        ggplot(data, aes(x)) + geom_density(kernel=input$kde_kernel, bw=input$kde_bw)
    })
    output$kde.ui <- renderUI(plotOutput("kde", height=input$plot_height))
    
    # this, but in 2d: https://mathisonian.github.io/kde/
    output$kde2d <- renderPlot({
        data <- ucenici17f()
        if(count(data) == 0) return(ggplot(data))
        
        x <- evalText((input$var_x), data)
        y <- evalText((input$var_y), data)
        ggplot(data, aes(x, y)) + xlab(input$var_x) + ylab(input$var_y) + 
            stat_density_2d(geom="point", aes(size = stat(density)), n = input$kde2d_n, contour = FALSE)
    })
    output$kde2d.ui <- renderUI(plotOutput("kde2d", height=input$plot_height))
    
    # 
    # ---------------------------------------
    # Tab 2: Faceting (and joins)
    # ---------------------------------------
    #
    
    svef <- reactive({
        tryCatch(
            sve %>%
                filter(if(is.null(input$facet_filter_val) || input$facet_filter_var == "") FALSE 
                       else ((evalText(input$facet_filter_var, .) %in% input$facet_filter_val) &
                             (if(input$base_filter=="") TRUE else evalText(input$base_filter, .)))),
            warning = function(w) tibble(), 
            error = function(e) tibble()
        )
    })
    svef %<>% debounce(750)
    
    output$facet <- renderPlot({
        data <- svef()
        if(count(data) == 0) return(ggplot(data))
        
        filt_var <- input$facet_filter_var
        filt_in <- input$facet_filter_val
        col <- input$facet_col
        x <- evalText((input$var_x), data)
        y <- evalText((input$var_y), data)
        pos <- if(input$facet_jitter) "jitter" else "identity"
        ggplot(data, aes(x, y, color=evalText(col, data))) + geom_point(alpha=input$facet_alpha, position=pos) + 
            facet_grid(rows = reformulate(".", filt_var)) + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))) + scale_color_discrete(name=col) +
            xlab(input$var_x) + ylab(input$var_y)
    })
    output$facet.ui <- renderUI(plotOutput("facet", height=input$plot_height))
    
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
        if(count(data) == 0) return(ggplot(data))
        
        x <- evalText((input$var_x), data)
        y <- evalText((input$var_y), data)
        pos <- if(input$model_rl_jitter) "jitter" else "identity"
        grid <- data %>% data_grid(!!input$var_x := x) %>% #this particular evil is called quasiquotation
            add_predictions(model())
        ggplot(data, aes(x, y)) + geom_point(alpha=input$model_rl_alpha, position=pos) +
            geom_line(aes(grid[[1]], grid[[2]]), data=grid, color="red", size=1.5) +
            xlab(input$var_x) + ylab(input$var_y)
    })
    output$model_regline.ui <- renderUI(plotOutput("model_regline", height=input$plot_height))
    
    output$model_freqpoly <- renderPlot({
        data <- ucenici17f() %>%
            select(input$var_x, input$var_y) %>% add_residuals(model())
        if(count(data) == 0) return(ggplot(data))
        
        ggplot(data, aes(resid)) + geom_freqpoly(binwidth=input$model_freq_binwidth)
    })
    output$model_freqpoly.ui <- renderUI(plotOutput("model_freqpoly", height=input$plot_height))
    
    output$model_resid <- renderPlot({
        data <- ucenici17f()  %>%
            select(input$var_x, input$var_y) %>% add_residuals(model())
        if(count(data) == 0) return(ggplot(data))
        
        x <- evalText((input$var_x), data)
        pos <- if(input$model_res_jitter) "jitter" else "identity"
        ggplot(data, aes(x, resid)) + geom_ref_line(h=0) + 
            geom_point(position=pos, alpha=input$model_res_alpha) + xlab(input$var_x)
    })
    output$model_resid.ui <- renderUI(plotOutput("model_resid", height=input$plot_height))
    
    
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
        join_choices <- levels(evalText(input$facet_filter_var, sve))
        updateSelectInput(session, "facet_filter_val", choices = join_choices, selected = join_choices[1])
    })
    
    # this triggers every time btn_swap is changed
    observeEvent(input$btn_swap, {
        selx <- isolate(input$var_x) # if we isolate input variable, this block won't react on it
        sely <- isolate(input$var_y)
        updateSelectInput(session, "var_x", choices = sve_varlist, selected = sely)
        updateSelectInput(session, "var_y", choices = sve_varlist, selected = selx)
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
        column(5, selectInput("var_x", "X", choices = uc_varlist, selected = "uc.matematika_p")),
        column(2, actionButton("btn_swap", "Swap", style="margin-top: 26px")),
        column(5, selectInput("var_y", "Y", choices = uc_varlist, selected = "uc.matematika"))
    ),
    
    fluidRow(
        column(12, textAreaInput("base_filter", "Filter", width="100%", rows=1))
    ),
    
    tabsetPanel(id="tabs",
                
        tabPanel("Basic plots", value="basic",
                 
                 h3("Scatter plot"),
                 
                 fluidRow(
                     column(9, uiOutput("basic_plot.ui")),
                     column(3, wellPanel(
                                sliderInput("p1_alpha", "Transparentnost (alpha):",
                                            min = 0, max = 1, value = 1),
                                checkboxInput("p1_jitter", "Jitter"),
                                checkboxInput("p1_col", "Boja"),
                                selectInput("p1_col_var", "Boja", choices = character(0))
                            ))
                 ),
                 
                 h3("Kernel Density Estimate"),
                 
                 fluidRow(
                     column(9, uiOutput("kde.ui")),
                     column(3, wellPanel(
                         selectInput("kde_kernel", "Kernel",
                                     choices = c("gaussian", "epanechnikov", "rectangular",  
                                        "triangular", "biweight",  "cosine", "optcosine")),
                         sliderInput("kde_bw", "Bandwidth",
                                     min = 0.01, max = 2, value = 0.05)
                     ))
                 ),
                 
                 fluidRow(
                     column(9, uiOutput("kde2d.ui")),
                     column(3, wellPanel(
                                sliderInput("kde2d_n", "KDE n",
                                            min = 0, max = 20, value = 10)
                            ))
                 )
        ),
        
        tabPanel("Faceting", value="joins",
               fluidRow(
                   column(9, uiOutput("facet.ui", height = 600)),
                   column(3, wellPanel(
                       sliderInput("facet_alpha", "Transparentnost (alpha):",
                                   min = 0, max = 1, value = 0.07),
                       checkboxInput("facet_jitter", "Jitter", value = TRUE),
                       selectInput("facet_filter_var", "Prikaži samo ako je", choices = sve_factors, selected = "sm.podrucje"),
                       selectInput("facet_filter_val", "jedan od", choices = levels(sve$sm.podrucje), selected = "gimnazija", multiple = TRUE),
                       selectInput("facet_col", "Boja", choices = sve_factors, selected = "sm.podrucje")
                   ))
               )  
        ),
        
        tabPanel("Models", value="models",
                 
                 h3("Regression line"),
                 
                 fluidRow(
                     column(9, uiOutput("model_regline.ui")),
                     column(3,  wellPanel(
                         sliderInput("model_rl_alpha", "Transparentnost (alpha):",
                                     min = 0, max = 1, value = 0.04),
                         checkboxInput("model_rl_jitter", "Jitter", value = TRUE)
                     ))
                 ),
                 
                 h3("Residual count"),
                 
                 fluidRow(
                     column(9, uiOutput("model_freqpoly.ui")),
                     column(3, wellPanel(
                         sliderInput("model_freq_binwidth", "Binwidth",
                                     min = 0.01, max = 3, value = 0.33)
                     ))
                 ),
                 
                 h3("Residual distribution"),
                 
                 fluidRow(
                     column(9, uiOutput("model_resid.ui")),
                     column(3, wellPanel(
                         sliderInput("model_res_alpha", "Transparentnost (alpha):",
                                     min = 0, max = 1, value = 0.05),
                         checkboxInput("model_res_jitter", "Jitter", value = TRUE)
                     ))
                )
        )
    ),
    
    sliderInput("plot_height", "Global plot height", 100, 1600, 400)
)

#
# Finally, run the app!
#
shinyApp(ui = ui, server = server)
