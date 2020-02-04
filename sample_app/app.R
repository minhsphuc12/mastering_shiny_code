source('~/function_library_tech.R')

library(shiny)
library(tidyverse)
library(lubridate)
library(reactlog)
library(shinyFeedback)
library(waiter)
library(rio)
library(janitor)
options(shiny.reactlog = TRUE)

#first app -----
# ui = fluidPage('Hello, Phuc')

# check dataset app -----
ui = fluidPage(
    selectInput(inputId = 'dataset', label = 'Dataset Label', choices = ls('package:datasets')),
    verbatimTextOutput(outputId = 'summary'),
    tableOutput('table')
)
server = function(input, output , session) {
    dataset = reactive({
        get(input$dataset, 'package:datasets')
    })

    output$summary = renderPrint(
        {
            summary(dataset())
        }
    )

    output$table = renderTable(
        {
            dataset()
        }
    )
}

shinyApp(ui, server)

## exercise: app greet by name -----
library(shiny)
ui = shiny::fluidPage(
    title = 'greeting app',
    textInput(inputId = 'name', label = 'what is your name?', width = 200, placeholder = 'NASDAP'),
    numericInput(inputId = 'age', label = 'How old are you?', value = 18, min = 0, max = 100, width = 200),
    verbatimTextOutput('greeting')
)

server = function(input, output, session) {
    output$greeting = renderText({
        stringr::str_c("Hello ", input$name, ' , ', input$age, ' years old. How are ya?')
    })
}

shinyApp(ui, server)

# exercise 2 ----
ui = fluidPage(
    sliderInput('x', 'x is', min = 1, max = 50, value = 30),
    sliderInput('y', 'y is', min = 1, max = 50, value = 5),
    "then, (x * y) is", textOutput("product"),
    "and, (x * y) + 5 is", textOutput("product_plus5"),
    "and (x * y) + 10 is", textOutput("product_plus10")
)
server  = function(input, output, session) {
    product = reactive({input$x * input$y})
    output$product = renderText({product()})
    output$product_plus5 = renderText({product()+5})
    output$product_plus10 = renderText({product()}+10)
}
shinyApp(ui, server)

# exercise 3 ----
library(ggplot2)
datasets = data(package = 'ggplot2')$results[,'Item']
ui = fluidPage(
    selectInput('dataset', 'Dataset', choices = datasets),
    # textOutput('name'),
    verbatimTextOutput('summary'),
    tableOutput('table'),
    plotOutput('plot')
)

server = function(input, output, session) {
    dataset = reactive({get(input$dataset, 'package:ggplot2')})
    output$summary = renderPrint({summary(dataset())})
    # output$plot = renderPlot({plot(dataset())})
    # output$name = input$dataset
    output$plot = renderPlot({plot(dataset()[,1])})
    # problem is that it is too big of data frame to render plot or table
    output$table = renderTable({dataset()[1:10,]})
}

shinyApp(ui, server)

# basic UI ----
animals = c('dog', 'cat', 'mouse', 'human')

ui = fluidPage(
    textInput('name', 'What is your name?', width = 300, placeholder = 'your name', value = NA_character_),
    textOutput(outputId = 'sen'),
    passwordInput('password', 'your password', placeholder = 'your password', width = 300),
    textAreaInput('story', 'your story', rows = 5, placeholder = 'your story should be good',
                  width = 300, height = 200, cols = 15, value = NULL, resize = 'vertical'),
    sliderInput('age', 'What is your age', min = 0, max = 100, value = 25, round = T, ticks = T,
                step = 2, width = 300, pre = 'age-', sep = ',', post = '-yo'),
    sliderInput('wake_time', 'What is waking hours', min = 0, max = 24, value = c(6, 22), round = T, ticks = T,
                step = 1, width = 300, pre = 'age-', sep = ',', post = '-yo'),
    # textOutput('w'),
    numericInput('lucky', 'What is your lucky number', value = 99, width = 300),
    dateInput('dt', 'Your birthday', weekstart = 1, autoclose = T, daysofweekdisabled = 0, width = 300),
    dateRangeInput('dt_range', 'Best time this month', weekstart = 1, autoclose = T, width = 300),
    selectInput('state', 'favorite state?', choices = state.name, multiple = T,
                selected = c('dog', 'human'), selectize = F, width = 300),
    radioButtons('animal', 'fav animal?', choices = animals, selected = 'cat'),
    radioButtons('emo', 'how do you feel right now?',
                 choiceNames = list(icon('angry'), icon('smile'), icon('sad-tear')),
                 choiceValues = list('angry', 'happy', 'sad'),
                 selected = 'cat'),
    checkboxGroupInput('animal2', 'hated animal?', choices = animals, selected = 'human', width = 300),
    checkboxInput('cleanup', 'Clean up?', value = TRUE),
    fileInput('upload', label = 'Upload your data', width = 300, buttonLabel = 'Upload', placeholder = 'file', multiple = T),
    actionButton("click", 'Click me', icon = icon('running'), width = 100),
    actionButton("drink", 'Drink me', icon = icon('cocktail'), width = 100)
)
server = function(input, output, session) {
    # sen = str_c('your name is ', input$name)
    output$sen = renderText({str_c('your name is ', input$name)})
    # output$w = renderText({str_c('your wake time is ', str_c(input$wake_time, collapse = '-'))})
}

shinyApp(ui, server)

# exercise
ui = fluidPage(
    sliderInput('dt_range', 'When should we deliver?', width = 500,
                min = ymd('2019-08-09'), max = ymd('2019-08-16'), timeFormat = '%Y-%m-%d',
                value = ymd('2019-08-10')),
    selectInput('freq_emo', 'choose most frequent emotion',
                choices = list(positive = list('happy', 'exited', 'bliss'),
                               negative = list('sad', 'angry'))
                ),
    # grouped choices does not work with any of radioButton, checkboxGroupInput
    sliderInput('interval_ani', 'this will animate', value = 50,
                min = 0, max = 100, step = 5, animate = T, width = 500),
    numericInput("number", "Select a value", value = 150, min = 0, max = 1000, step = 50)

)
server = function(input, output, session) {

}

shinyApp(ui, server)

# output ----
ui = fluidPage(
    textOutput('text'),
    verbatimTextOutput('code')
)

print_and_return <- function() {
    print("a")
    print("b")
    "c"
}
server = function(input, output, session) {
    output$text = renderText({print_and_return()})
    output$code = renderPrint({print_and_return()
        })
}
shinyApp(ui, server)

# table
ui = fluidPage(
    tableOutput('static'),
    dataTableOutput('dynamic'),
    plotOutput('plot', width = '400px')
)
server = function(input, output, session) {
    output$static = renderTable({head(mtcars)})
    output$dynamic = renderDataTable({mtcars}, options = list(pageLength = 5))
    output$plot = renderPlot({plot(1:5)})
}
shinyApp(ui, server)

# exercise
ui = fluidPage(
    plotOutput('plot', width = '700px', height = '300px'),
    # plotOutput('plot1', width = '300px'),
    # plotOutput('plot2', width = '300px')
    dataTableOutput('tb')
)
server = function(input, output, session) {
    output$plot = renderPlot({plot(1:5)})
    # output$plot1 = renderPlot({plot(1:5)})
    # output$plot2 = renderPlot({plot(1:5)})
    output$tb = renderDataTable({mtcars}, options = list(dom = 't', pageLength = 5,
                                                         ordering = F,
                                                         searching = F))
}
shinyApp(ui, server)
require(shinytheme)
install.packages('shinythemes')
# layout ----
ui = fluidPage(
    headerPanel('Central Limit Theorem'),
    sidebarLayout(
        sidebarPanel(
            sliderInput('m', 'number of samples:', 2, min = 1, max = 1000)
        ),
        mainPanel(plotOutput('hist')),position = 'right'
    ), theme = shinythemes::shinytheme('darkly')
)
server = function(input, output, session) {
    # output$hist = renderPlot({hist(runif(input$m))})
    output[['hist']] = renderPlot({
        means = replicate(10^3, mean(runif(input[['m']])))
        hist(means, breaks = 20)
    })
}

shinyApp(ui, server)
mean(runif(100))

# reactive programming ----

# expression
histogram = function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
    df = data.frame(
        x = c(x1, x2),
        g = c(rep('x1', length(x1)), rep('x2', length(x2)))
    )
    ggplot(df, aes(x, fill = g, group = g)) +
        geom_histogram(binwidth = binwidth, position = 'identity', color="#e9ecef", alpha=0.6) +
        coord_cartesian(xlim = xlim) +
        scale_fill_manual(values=c("#69b3a2", "#404080")) +
        theme_ipsum()
        # scale_y_continuous(limits = c(0, h))
}

t_test = function(x1, x2) {
    test = t.test(x1, x2)
    sprintf(
        'p value: %0.3f\n[%0.2f, %0.2f]',
        test$p.value, test$conf.int[1], test$conf.int[2]
    )
}

x1 = rnorm(100, mean = 0, sd = 0.5)
x2 = rnorm(200, mean = 0.15, sd = 0.9)

histogram(x1, x2)

ui <- fluidPage(
    fluidRow(
        column(4,
               "Distribution 1",
               numericInput("n1", label = "n", value = 1000, min = 1),
               numericInput("mean1", label = "m", value = 0, step = 0.1),
               numericInput("sd1", label = "dd", value = 0.5, min = 0.1, step = 0.1)
        ),
        column(4,
               "Distribution 2",
               numericInput("n2", label = "n", value = 1000, min = 1),
               numericInput("mean2", label = "m", value = 0, step = 0.1),
               numericInput("sd2", label = "dd", value = 0.5, min = 0.1, step = 0.1)
        ),
        column(4,
               "Histogram",
               numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
               sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
        )
    ),
    fluidRow(
        column(9, plotOutput("hist")),
        column(3, verbatimTextOutput("ttest"))
    )
)

server <- function(input, output, session) {
    x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
    x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
    h1 = reactive(max(input$n1, input$n2) / 2)
    output$hist <- renderPlot({
        # x1 <- rnorm(input$n1, input$mean1, input$sd1)
        # x2 <- rnorm(input$n2, input$mean2, input$sd2)

        histogram(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
    })

    output$ttest <- renderText({
        # x1 <- rnorm(input$n1, input$mean1, input$sd1)
        # x2 <- rnorm(input$n2, input$mean2, input$sd2)

        t_test(x1(), x2())
    })
}

shinyApp(ui, server)

# simpler app ----
ui = fluidPage(
    fluidRow(
        column(3,
               numericInput('lambda1', label = 'lambda1', value = 3),
               numericInput('lambda2', label = 'lambda2', value = 3),
               numericInput('n', label = 'n', value = 10^4, min = 0),
               actionButton('simulate', 'Simulate'),
               verbatimTextOutput('greeting')
    ),
    column(9, plotOutput('hist'))
))
server = function(input, output, session) {
    # # timer = reactiveTimer(500)
    # x1 = reactive({
    #     # timer()
    #     input[['simulate']]
    #     rpois(input$n, input$lambda1)}
    #     )
    # x2 = reactive({
    #     # timer()
    #     input[['simulate']]
    #     rpois(input$n, input$lambda2)
    #     })
    x1 <- eventReactive(input$simulate, {
        rpois(input$n, input$lambda1)
    })
    x2 <- eventReactive(input$simulate, {
        rpois(input$n, input$lambda2)
    })
    output[['hist']] = renderPlot({
        histogram(x1(), x2(), binwidth = 0.3, xlim = c(0,40))
    })

    text = reactive(str_c('Hello, set n to ', input$n))
    output$greeting = renderText(text())
    observeEvent(input$n,
                 {
                     message(str_c('Hello, message n = ', input$n))
                     # message('x')
                 })
}
shinyApp(ui, server)

# case study ----
# install.packages('vroom')
library(vroom)

library(tidyverse)
install_github("hadley/neiss")
library(neiss)

top_prod <- injuries %>%
    filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
    count(prod1, sort = TRUE) %>%
    filter(n > 5 * 365)

dir.create('~/neiss')

injuries %>%
    filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
    semi_join(top_prod, by = "prod1") %>%
    mutate(age = floor(age), sex = tolower(sex), race = tolower(race)) %>%
    filter(sex != "unknown") %>%
    select(trmt_date, age, sex, race, body_part, diag, location, prod_code = prod1, weight, narrative) %>%
    vroom::vroom_write("neiss/injuries.tsv.gz")

products %>%
    semi_join(top_prod, by = c("code" = "prod1")) %>%
    rename(prod_code = code) %>%
    vroom::vroom_write("neiss/products.tsv")

population %>%
    filter(year == 2017) %>%
    select(-year) %>%
    rename(population = n) %>%
    vroom::vroom_write("neiss/population.tsv")

injuries = vroom('neiss/injuries.tsv.gz')
population = vroom('neiss/population.tsv')

selected <- injuries %>% filter(prod_code == 1842)

summary <- selected %>%
    count(age, sex, wt = weight)
summary %>%
    ggplot(aes(age, n, colour = sex)) +
    geom_line() +
    labs(y = "Estimated number of injuries")

summary <- selected %>%
    count(age, sex, wt = weight) %>%
    left_join(population, by = c("age", "sex")) %>%
    mutate(rate = n / population * 1e4)

summary %>%
    ggplot(aes(age, rate, colour = sex)) +
    geom_line(na.rm = TRUE) +
    labs(y = "Injuries per 10,000 people")

ui = fluidPage(
    fluidRow(
        fluidRow(
            column(8,
                   selectInput("code", "Product",
                               choices = setNames(products$code, products$title),
                               width = "100%"
                   )
            ),
            column(4, selectInput("y", "Y axis", c("rate", "count")))
        )
    ),

    fluidRow(
        numericInput('r_num', 'Number of row to be showed', value = 5, min = 1, max = 10, step = 1)
    ),

    fluidRow(
        column(4, tableOutput('diag')),
        column(4, tableOutput('body_part')),
        column(4, tableOutput('location'))
    ),


    # fluidRow(
    #     column(4, actionButton("story", "Tell me a story")),
    #     column(4, actionButton("back", "Get me previous story")),
    #     column(4, actionButton("forth", "Get me next story"))
    # ),

    fluidRow(
        numericInput('story_num', 'Story Index', value = 1, min = 1, max = NA, step = 1),
        column(10, textOutput("narrative"))
    ),

    fluidRow(
        column(12, plotOutput('age_sex'))
    )
)

count_top = function(df, var, n = 5) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

server = function(input, output, session) {
    selected = reactive(injuries %>% filter(prod_code == input$code))
    output$diag = renderTable({
        # selected() %>% count(diag, wt = weight, sort = TRUE)
        selected() %>% count_top(diag, n = input$r_num)
    } , width = '100%')
    output$body_part = renderTable({
        # selected() %>% count(body_part, wt = weight, sort = TRUE)
        selected() %>% count_top(body_part, n = input$r_num)
    } , width = '100%')
    output$location = renderTable({
        # selected() %>% count(location, wt = weight, sort = TRUE)
        selected() %>% count_top(location, n = input$r_num)
    } , width = '100%')
    summary = reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c('age', 'sex')) %>%
            mutate(rate = n/population*10^4)
    })

    # output$narrative = renderText({
    #     input$story
    #     selected() %>% pull(narrative) %>% sample(1)
    # })

    output$narrative = renderText({
        # length(c(1,2))
        (selected() %>% pull(narrative))[[input$story_num %% nrow(selected())]]
    })


    output$age_sex <- renderPlot({
        if (input$y == "count") {
            summary() %>%
                ggplot(aes(age, n, colour = sex)) +
                geom_line() +
                labs(y = "Estimated number of injuries") +
                theme_grey(15)
        } else {
            summary() %>%
                ggplot(aes(age, rate, colour = sex)) +
                geom_line(na.rm = TRUE) +
                labs(y = "Injuries per 10,000 people") +
                theme_grey(15)
        }
    })

}
# install.packages('debugme')
library(debugme)
shinyApp(ui, server)
# sort and lump
injuries %>%
    mutate(diag_2 = fct_lump(fct_infreq(diag), n = 5)) %>%
    group_by(diag_2) %>%
    summarise(n = as.integer(sum(weight)))
# lump and short
injuries %>%
    mutate(diag_2 = fct_infreq(fct_lump(diag, n = 5))) %>%
    group_by(diag_2) %>%
    summarise(n = as.integer(sum(weight)))

# select type of chart

runGist("eb3470beb1c0252bd0289cbc89bcf36f")

# workflow ----

library(shiny)

f <- function(x) g(x)
g <- function(x) h(x)
h <- function(x) x * 2

ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    selectInput("n", "N", 1:10),
    # plotOutput("plot")
    textOutput('half')
)
server <- function(input, output, session) {

    half = reactive({
        even = as.numeric(input$n) %% 2 == 0
        shinyFeedback::feedbackWarning(inputId = 'n',
                                       show = !even,
                                       text = 'Please select an even number')
        req(even)
        as.numeric(input$n) / 2
    })
    # observeEvent(input$n,
    #              shinyFeedback::feedbackWarning(inputId = 'n',
    #                                             show = as.numeric(input$n) %% 2 != 0,
    #                                             text = 'Please select an even number'
    #              ))

    # output$plot <- renderPlot({
    #     n <- f(as.numeric(input$n))
    #     plot(head(cars, n))
    # })
    output$half = renderText(half())
}
shinyApp(ui, server)

head(cars, 3) %>% plot()

reactlogShow()

# repex - reproducible example ----
dput(iris)
# install.packages('prettycode')
styler::style_text('server <- function(input, output, session) {
    output$plot <- renderPlot({
        n <- f(as.numeric(input$n))
        plot(head(cars, n))
    })
}
')

# user feedback ------

# remotes::install_github("JohnCoene/waiter")
# remotes::install_github("merlinoa/shinyFeedback")

library(waiter)
library(shinyFeedback)

# validation
ui = fluidPage(
    selectInput('language', 'Language', choices = c('', 'en', 'mao')),
    textInput('name', 'Name'),
    textOutput('greeting')
)
server = function(input, output, session) {
    greetings = c(en = 'hello', mao = 'ki ora')
    output$greeting = renderText({
        req(input$language, input$name)
        str_c(greetings[[input$language]], ' ', input$name, '!')
    })
}
shinyApp(ui, server)

library(shiny)

# req() validation
ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    textInput('dataset', 'Dataset Name'),
    tableOutput('data')
)

server <- function(input, output, session) {
    data = reactive({
        req(input$dataset)
        exists = exists(input$dataset, 'package:datasets')
        shinyFeedback::feedbackDanger('dataset', !exists, 'unknown dataset')
        req(exists, cancelOutput = TRUE)
        get(input$dataset, 'package:datasets')
    })
    output$data = renderTable({
        head(data())
    })
}

shinyApp(ui, server)

# validate output
library(shiny)

ui <- fluidPage(
  numericInput('x', 'X', value = 0),
  selectInput('trans', 'transformation', choices = c('square', 'log', 'square-root')),
  textOutput('out')
)

server <- function(input, output, session) {
    output$out = renderText({
        if(input$x < 0 && input$trans %in% c('log', 'square-root')) {
            validate('x can not be negative for this transformation')
        }
        switch(input$trans,
               square = input$x ^ 2,
               'square-root' = sqrt(input$x),
               log = log(input$x))
    })
}

shinyApp(ui, server)

switch('loga', loga = log(19))

# notification ------
library(shiny)

ui <- fluidPage(
    actionButton('goodnight','Good night')
)

server <- function(input, output, session) {
    # observeEvent(input$goodnight, {
    #     showNotification('So long', type = 'message', duration = NULL, closeButton = FALSE)
    #     Sys.sleep(1)
    #     showNotification('Farewell', type = 'warning')
    #     Sys.sleep(1)
    #     showNotification('Auf Wiedersehen', type = 'error')
    #
    # })
  data = reactive({
    id = showNotification("Reading data ...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    read.csv(input$path)
  })



}

ui <- fluidPage(
  tableOutput("data")
)


server = function(input, output, session) {
  notify = function(msg, id = NULL) {
    showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
  }

  data = reactive({
    id = notify("Reading data")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)

    notify('Reticulating splines ...', id = id)
    Sys.sleep(1)

    notify('Shit 2 ...', id = id)
    Sys.sleep(1)

    notify('Shit 3 ...', id = id)
    Sys.sleep(1)

    mtcars
    # read.csv(input$path)
  })
  output$data = renderTable(head(data()))
}

shinyApp(ui, server)


# progress bar

ui <- fluidPage(
  numericInput("steps", "How many steps?", 10),
  # use_waiter(),
  actionButton("go", "go"),
  textOutput("result")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$go)

    progress <- Progress$new(max = input$steps)
    on.exit(progress$close())

    progress$set(message = "Computing random number")
    for (i in seq_len(input$steps)) {
      Sys.sleep(0.5)
      progress$inc(1)
    }
    runif(1)
  })

  output$result <- renderText(round(data(), 2))
}

shinyApp(ui, server)

# spinter
library(shiny)

ui <- fluidPage(
  waiter::use_waiter(),
  actionButton("go", "go"),
  textOutput("result")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$go)
    waiter <- waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide())

    Sys.sleep(sample(5, 1))
    runif(1)
  })
  output$result <- renderText(round(data(), 2))
}

shinyApp(ui, server)

# confirming

modal_confirm <- modalDialog(
  "Are you sure you want to continue?",
  title = "Deleting files",
  footer = list(
    actionButton("cancel", "Cancel"),
    actionButton("ok", "Delete", class = "btn btn-danger")
  )
)

server <- function(input, output, session) {
  observeEvent(input$delete, {
    showModal(modal_confirm)
  })

  observeEvent(input$ok, {
    showNotification("Files deleted")
    removeModal()
  })
  observeEvent(input$cancel,
               removeModal()
  )
}

# undoing
ui <- fluidPage(
  textAreaInput("message",
                label = NULL,
                placeholder = "What's happening?",
                rows = 3
  ),
  actionButton("tweet", "Tweet")
)
runLater <- function(action, seconds = 3) {
  observeEvent(
    invalidateLater(seconds * 1000), action,
    ignoreInit = TRUE,
    once = TRUE,
    ignoreNULL = FALSE,
    autoDestroy = FALSE
  )
}

server <- function(input, output, session) {
  waiting <- NULL
  last_message <- NULL

  observeEvent(input$tweet, {
    notification <- glue::glue("Tweeted '{input$message}'")
    last_message <<- input$message
    updateTextAreaInput(session, "message", value = "")

    showNotification(
      notification,
      action = actionButton("undo", "Undo?"),
      duration = NULL,
      closeButton = FALSE,
      id = "tweeted",
      type = "warning"
    )

    waiting <<- runLater({
      cat("Actually sending tweet...\n")
      removeNotification("tweeted")
    })
  })

  observeEvent(input$undo, {
    waiting$destroy()
    showNotification("Tweet retracted", id = "tweeted")
    updateTextAreaInput(session, "message", value = last_message)
  })
}

shinyApp(ui, server)

# upload & downloads ----
# upload ----
library(shiny)

ui <- fluidPage(
  fileInput('file', 'upload file', buttonLabel = 'Upload', multiple = T),
  tableOutput('files')
)

server <- function(input, output, session) {
  req(input$file)
  output$files = renderTable(input$file)
}

shinyApp(ui, server)

library(shiny)

# download
ui <- fluidPage(
  fileInput('file', NULL, accept = c('.csv', '.tsv')),
  numericInput('n', 'rows', value = 5, min = 1, step = 1),
  tableOutput('files'),
  tableOutput('head'),
  downloadButton('download1', class = 'btn-primary'),
  downloadLink('download2', class = c('btn-success', 'btn-lg'))
)

server <- function(input, output, session) {
  output$files = renderTable(input$file)
  data = reactive({
    req(input$file)
    ext = tools::file_ext(input$file$name)
    switch(
      ext,
      csv = rio::import(input$file$datapath),
      tsv = rio::import(input$file$datapath),
      validate('invalid file; please upload csv or tsv file')
    )

  })

  output$head = renderTable(head(x = data(), n = input$n))
  output$download1 = downloadHandler(
    filename = function() {
        input$file$name
    },
    content = function(file) {
      export(data(), file)
    }
  )
}

shinyApp(ui, server)

# transfer data ----
library(shiny)

ui <- fluidPage(
  selectInput('dataset', 'pick dataset', choices = ls('package:datasets')),
  # tableOutput('files'),
  tableOutput('preview'),
  downloadButton('download')
)

server <- function(input, output, session) {
  data = reactive({
    out = get(input$dataset, 'package:datasets')
    if(!is.data.frame(out)) {
      validate(str_c("'", input$dataset, "' is not a data frame"))
    }
    out
  })

  output$preview = renderTable(head(data()))
  output$download = downloadHandler(
    filename = function() {
      str_c('dataset_', input$dataset, '.tsv')
    },
    content = function(file) {
      export(data(), file)
    }
  )
}

shinyApp(ui, server)

ui <- fluidPage(
  sliderInput("n", "Number of points", 1, 100, 50),
  downloadButton("report", "Generate report")
)

server <- function(input, output, session) {

  report_path <- tempfile(fileext = ".Rmd")
  file.copy("report.Rmd", report_path, overwrite = TRUE)


  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      params <- list(n = input$n)

      rmarkdown::render(report_path,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)

shinyApp(
  ui = fluidPage(
    sliderInput("slider", "Slider", 1, 100, 50),
    downloadButton("report", "Generate report")
  ),
  server = function(input, output) {
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(n = input$slider)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)

# case study ----
ui_upload <- sidebarLayout(
  sidebarPanel(
    fileInput("file", "Data", buttonLabel = "Upload..."),
    textInput("delim", "Delimiter (leave blank to guess)", ""),
    numericInput("skip", "Rows to skip", 0, min = 0),
    numericInput("rows", "Rows to preview", 10, min = 1)
  ),
  mainPanel(
    h3("Raw data"),
    tableOutput("preview1")
  )
)

ui_clean <- sidebarLayout(
  sidebarPanel(
    checkboxInput("snake", "Rename columns to snake case?"),
    checkboxInput("constant", "Remove constant columns?"),
    checkboxInput("empty", "Remove empty cols?")
  ),
  mainPanel(
    h3("Cleaner data"),
    tableOutput("preview2")
  )
)

ui_download <- fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block"))
)

ui <- fluidPage(
  ui_upload,
  ui_clean,
  ui_download
)

server <- function(input, output, session) {
  # Upload ---------------------------------------------------------------
  raw <- reactive({
    req(input$file)
    delim <- if (input$delim == "") NULL else input$delim
    vroom::vroom(input$file$datapath, delim = delim, skip = input$skip)
  })
  output$preview1 <- renderTable(head(raw(), input$rows))

  # Clean ----------------------------------------------------------------
  tidied <- reactive({
    out <- raw()
    if (input$snake) {
      names(out) <- janitor::make_clean_names(names(out))
    }
    if (input$empty) {
      out <- janitor::remove_empty(out, "cols")
    }
    if (input$constant) {
      out <- janitor::remove_constant(out)
    }

    out
  })
  output$preview2 <- renderTable(head(tidied(), input$rows))

  # Download -------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(tidied(), file)
    }
  )
}

shinyApp(ui, server)

# dynamic ui ----
# update function -----
library(shiny)

ui <- fluidPage(
  numericInput('min', 'minimum', 0),
  numericInput('max', 'maximum', 3),
  sliderInput('n', 'n', min = 0, max = 3, value = 1)
)

server <- function(input, output, session) {
  observeEvent(input$min, {
    updateNumericInput(session, 'n', min = input$min)
  })
  observeEvent(input$max, {
    updateNumericInput(session, 'n', max = input$max)
  })

}

shinyApp(ui, server)

# hierachial select box ----
library(shiny)

sales <- vroom::vroom("~/sales_data_sample.csv", col_types = list())
sales

ui <- fluidPage(
  selectInput('territory', 'Territory', choices = unique(sales$TERRITORY)),
  selectInput('customername', 'Customer', choices = unique(NULL)),
  selectInput('ordernumber', 'Order', choices = unique(NULL)),
  tableOutput('data')
)

server <- function(input, output, session) {
  territory = reactive({
    sales %>% filter(TERRITORY == input$territory)
  })
  # update customer list based on territory
  observeEvent(input$territory, {
    updateSelectInput(session, 'customername', choices = unique((territory())$CUSTOMERNAME))
  })

  customer = reactive({
    territory() %>% filter(CUSTOMERNAME == input$customername)
  })
  sales$ORDERNUMBER
  # update order list based on customer
  observeEvent(input$customername, {
    updateSelectInput(session, 'ordernumber', choices = unique((customer())$ORDERNUMBER))
  })

  order = reactive({
    customer() %>% filter(ORDERNUMBER == input$ordernumber)
  })

  output$data = renderTable({head(order())})
}

shinyApp(ui, server)

# circular reference ----
library(shiny)

ui <- fluidPage(
  numericInput("n", "n", 0)
)
server <- function(input, output, session) {
  observeEvent(input$n,
               updateNumericInput(session, "n", value = input$n + 1)
  )
}
shinyApp(ui, server)

ui <- fluidPage(
  numericInput("temp_c", "Celsius", NA),
  numericInput("temp_f", "Fahrenheit", NA)
)

server <- function(input, output, session) {
  observeEvent(input$temp_f, {
    c <- round((input$temp_f - 32) * 5 / 9)
    updateNumericInput(session, "temp_c", value = c)
  })

  observeEvent(input$temp_c, {
    f <- round((input$temp_c * 9 / 5) + 32)
    updateNumericInput(session, "temp_f", value = f)
  })
}

shinyApp(ui, server)

# exercise -----
library(shiny)

# 1
ui <- fluidPage(
  numericInput("year", "year", value = 2020),
  dateInput("date", "date"),
  verbatimTextOutput('date_out')
)

server <- function(input, output, session) {
  observeEvent(input$year, {
    updateDateInput(session, 'date',
                    # value = str_c(input$year, '-01-01'),
                    min = str_c(input$year, '-01-01'),
                    max = str_c(input$year, '-12-31'),
                    label = str_c('get date for year ', input$year)
    )
  })

  date_string = reactive({as.character(input$date)})

  output$date_out = renderText({date_string()})
}

shinyApp(ui, server)

#2
install.packages('openintro')
library(openintro)
states = unique(county$state)

ui <- fluidPage(
  selectInput("state", "State", choices = states),
  selectInput("county", "County", choices = NULL)
)

server <- function(input, output, session) {

  filter_county = reactive({
    county %>%
      filter(state == input$state) %>%
      .$name %>%
      as.character() %>%
      unique()
  })
  # county %>%
  #   filter(state == 'Alaska') %>%
  #   .$name %>%
  #   as.character() %>%
  #   unique()

  label_county = reactive({
    switch(input$state,
           Louisiana = 'Parrish',
           Alaska = 'Borrough',
           'Count'
    )
  })

  observeEvent(input$state,
               {
                 updateSelectInput(session,
                                   'county',
                                   choices = filter_county(),
                                   label = label_county())
               })
}

shinyApp(ui, server)

#3 #4
library(gapminder)
continents <- c(' ', as.character(unique(gapminder$continent)))

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents),
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session) {

  filter_country = reactive({
    if(input$continent != ' ') {
      gapminder %>%
        filter(continent == input$continent) %>%
        .$country %>%
        as.character() %>%
        unique()
    } else {
      gapminder %>%
        .$country %>%
        as.character() %>%
        unique()
    }

  })

  # label_county = reactive({
  #   switch(input$state,
  #          Louisiana = 'Parrish',
  #          Alaska = 'Borrough',
  #          'Count'
  #   )
  # })

  observeEvent(input$continent,
               {
                 updateSelectInput(session,
                                   'country',
                                   choices = filter_country()
                                   # label = label_county()
                                   )
               })

  filter_country_data = reactive({
    # if(input$continent != '') {
      gapminder %>%
        # filter(continent == input$continent) %>%
        filter(country == input$country)
    # } else {
      # gapminder %>%
        # filter(country == input$country)
    # }

  })

  output$data = renderTable(filter_country_data())
}

shinyApp(ui, server)

# update visibility ------
ui <- fluidPage(
  tags$style("#switcher { display:none; }"),
  sidebarLayout(
    sidebarPanel(
      selectInput("controller", "Show", choices = paste0("panel", 1:3))
    ),
    mainPanel(
      tabsetPanel(
        id = "switcher",
        tabPanel("panel1", "Panel 1 content"),
        tabPanel("panel2", "Panel 2 content"),
        tabPanel("panel3", "Panel 3 content")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(session, "switcher", selected = input$controller)
  })
}
shinyApp(ui, server)

parameter_tabs <- tagList(
  tags$style("#params { display:none; }"),
  tabsetPanel(id = "params",
              tabPanel("normal",
                       numericInput("mean", "mean", value = 1),
                       numericInput("sd", "standard deviation", min = 0, value = 1)
              ),
              tabPanel("uniform",
                       numericInput("min", "min", value = 0),
                       numericInput("max", "max", value = 1)
              ),
              tabPanel("exponential",
                       numericInput("rate", "rate", value = 1, min = 0),
              )
  )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution",
                  choices = c("normal", "uniform", "exponential")
      ),
      numericInput("n", "Number of samples", value = 100),
      parameter_tabs,
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$dist, {
    updateTabsetPanel(session, "params", selected = input$dist)
  })

  sample <- reactive({
    switch(input$dist,
           normal = rnorm(input$n, input$mean, input$sd),
           uniform = runif(input$n, input$min, input$max),
           exponential = rexp(input$n, input$rate)
    )
  })
  output$hist <- renderPlot(hist(sample()))
}

shinyApp(ui, server)

# wizard interface

ui <- fluidPage(
  tags$style("#wizard { display:none; }"),
  tabsetPanel(id = "wizard",
              tabPanel("page1",
                       "Welcome!",
                       actionButton("page12", "next")
              ),
              tabPanel("page2",
                       "Only one page to go",
                       actionButton("page21", "prev"),
                       actionButton("page23", "next")
              ),
              tabPanel("page3",
                       "You're done!",
                       actionButton("page32", "prev")
              )
  )
)

server <- function(input, output, session) {
  switch_tab <- function(page) {
    updateTabsetPanel(session, "wizard", selected = page)
  }

  observeEvent(input$page12, switch_tab("page2"))
  observeEvent(input$page21, switch_tab("page1"))
  observeEvent(input$page23, switch_tab("page3"))
  observeEvent(input$page32, switch_tab("page2"))
}

shinyApp(ui, server)

# dialog box ----
modalDialog()

# create UI with code ----
ui <- fluidPage(
  textInput("label", "label"),
  selectInput("type", "type", c("slider", "numeric")),
  uiOutput("numeric")
)
server <- function(input, output, session) {
  output$numeric <- renderUI({
    if (input$type == "slider") {
      sliderInput("dynamic", input$label, value = isolate(input$dynamic), min = 0, max = 10)
    } else {
      numericInput("dynamic", input$label, value = isolate(input$dynamic), min = 0, max = 10)
    }
  })
}

shinyApp(ui, server)

# multiple control
ui <- fluidPage(
  numericInput("n", "Number of colours", value = 5, min = 1),
  uiOutput("col"),
  textOutput("palette")
)

server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))

  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL))
  })

  output$palette <- renderText({
    map_chr(col_names(), ~ input[[.x]])
  })
}

shinyApp(ui, server)


a = str_c('col', seq_len(10))
map(a, ~ textInput(.x, NULL))
map(a, ~ textInput(.x, NULL))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of colours", value = 5, min = 1),
      uiOutput("col"),
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)
server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))

  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL, value = isolate(input[[.x]])) %||% "")
  })

  output$plot <- renderPlot({
    cols <- map_chr(col_names(), ~ input[[.x]])
    cols[cols == ""] <- NA

    barplot(
      rep(1, length(cols)),
      col = cols,
      space = 0,
      axes = FALSE
    )
  })
}
shinyApp(ui, server)

# dynamic filtering -----
range(10, na.rm = T)[1]

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      make_ui(iris$Sepal.Length, "Sepal.Length"),
      make_ui(iris$Sepal.Width, "Sepal.Width"),
      make_ui(iris$Species, "Species")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  selected <- reactive({
    filter_var(iris$Sepal.Length, input$Sepal.Length) &
      filter_var(iris$Sepal.Width, input$Sepal.Width) &
      filter_var(iris$Species, input$Species)
  })

  output$data <- renderTable(head(iris[selected(), ], 12))
}
shinyApp(ui, server)

# use all column
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      map(names(iris), ~ make_ui(iris[[.x]], .x))
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  selected <- reactive({
    each_var <- map(names(iris), ~ filter_var(iris[[.x]], input[[.x]]))
    reduce(each_var, `&`)
  })

  output$data <- renderTable(head(iris[selected(), ], 12))
}

shinyApp(ui, server)

# any dataset
dfs <- keep(ls("package:datasets"), ~ is.data.frame(get(.x, "package:datasets")))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Dataset", choices = dfs),
      uiOutput("filter")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  data <- reactive({
    get(input$dataset, "package:datasets")
  })
  vars <- reactive(names(data()))

  output$filter <- renderUI(
    map(vars(), ~ make_ui(data()[[.x]], .x))
  )

  selected <- reactive({
    each_var <- map(vars(), ~ filter_var(data()[[.x]], input[[.x]]))
    reduce(each_var, `&`)
  })

  output$data <- renderTable(head(data()[selected(), ], 12))
}

shinyApp(ui, server)

# exercise -----
# 1
ui <- fluidPage(
  selectInput("type", "type", c("slider", "numeric")),
  uiOutput("numeric")
)
server <- function(input, output, session) {
  output$numeric <- renderUI({
    if (input$type == "slider") {
      sliderInput("n", "n", value = 0, min = 0, max = 100)
    } else {
      numericInput("n", "n", value = 0, min = 0, max = 100)
    }
  })
}

library(shiny)

ui <- fluidPage(
  tags$style('#input_select {display:none;}'),
  selectInput("type", "type", c("slider", "numeric")),
  tabsetPanel(
    id = 'input_select',
    tabPanel('slider',
             sliderInput("n", "n", value = isolate(input$n), min = 0, max = 100)
    ),
    tabPanel('numeric',
             numericInput("n", "n", value = isolate(input$n), min = 0, max = 100))
  )
)

server <- function(input, output, session) {
  observeEvent(input$type, {
    updateTabsetPanel(session, 'input_select', selected = input$type)
  })
}

shinyApp(ui, server)

# 2 Add support for date and date-time columns make_ui() and filter_var().

# 3 (Advanced) If you know the S3 OOP system, consider how you could replace the if blocks in make_ui() and filter_var() with generic functions.

# 4 (Hard) Make a wizard that allows the user to upload their own dataset.
# The first page should handle the upload.
# The second should handle reading it, providing one drop down for each variable that lets the user select the column type.
# The third page should provide some way to get a summary of the dataset.

library(shiny)

tabPanel('Upload',
         fileInput('file', label = 'Upload File', multiple = FALSE,
                   accept = c('.csv', '.xlsx'), buttonLabel = 'angellist'),
         actionButton("page12", "next")
)

ui <- fluidPage(
  tags$style("#wizard { display:none; }"),
  # first tab: upload file
  tabsetPanel(id = 'wizard',
    tabPanel(title = 'Upload', icon = icon('fort-awesome'),
             actionButton("page12", "next"),
             fileInput('file', label = 'Upload File', multiple = FALSE,
                        accept = c('.csv', '.xlsx'), buttonLabel = icon('angellist')),

             verbatimTextOutput('file_class')
            ),

  # second tab: handle data filter
    tabPanel(title = 'Filter', icon = icon('black-tie'),
             actionButton("page21", "prev"),
             actionButton("page23", "next"),
             sidebarLayout(
               sidebarPanel(
                 numericInput('h', 'Select number of row:', value = 5, min = 1, max = 10),
                 selectInput('data_type', 'Select data type of columns',
                             selected = 'numeric', choices = c('numeric', 'character'), multiple = TRUE)
               ),
               mainPanel(tableOutput('filtered_data')),
               position = 'left'
               )

              ),
  # third tab: data summary
    tabPanel(title = 'Summary', icon = icon('kiss-wink-heart'),
             actionButton("page32", "prev"),
             sidebarLayout(
               sidebarPanel(
                 selectInput('summary_type', 'Select summary type of dataset',
                             selected = 'numerical', choices = c('numerical', 'correlation'),
                             multiple = FALSE)
               ),
               mainPanel(verbatimTextOutput('data_summary')),
               position = 'left'
               )

             )
  )
)
options(shiny.reactlog = TRUE)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath),
           xlsx = rio::import(input$file$datapath),
           validate("Invalid file; Please upload a .csv or .xlsx file")
    )
  })
  # a %>% map_chr(class)
  # output$file_class = renderPrint(input$data_type)
  output$file_class = renderText(data() %>% map_chr(class))

  switch_tab = function(page) {
    updateTabsetPanel(session, 'wizard', selected = page)
  }

  observeEvent(input$page12, switch_tab('Filter'))
  observeEvent(input$page21, switch_tab('Upload'))
  observeEvent(input$page23, switch_tab('Summary'))
  observeEvent(input$page32, switch_tab('Filter'))

  filter_data = reactive({
    (data())[,((data()) %>% map_chr(class)) %in% input$data_type] %>%
      head(input$h)

    # data()[] %>%
    #   head(input$h)
  })

  output$filtered_data = renderTable(filter_data())

  summarized_data = reactive({
    if(input$summary_type == 'numerical') {
      data() %>% summary
    } else {
      data() %>% select_if(is_numeric) %>% cor
    }
  })

  output$data_summary = renderPrint(summarized_data())
}

shinyApp(ui, server)

reactlogReset()
reactlogShow()

# reduce duplication ----
 sliderInput01 <- function(id, label = id) {
  sliderInput(id, label, min = 0, max = 1, value = 0.5, step = 0.1)
}

if (packageVersion("htmltools") >= "0.3.6.9004") {
  vars <- c("alpha", "beta", "gamma", "delta")
  sliders <- purrr::map(vars, sliderInput01)
  ui <- fluidRow(!!!sliders)
}

ngoSelectInput <- function(var, label, multiple = TRUE) {
  choices <- sort(unique(ngo[[var]]))
  label <- paste0("Choose a ", label, ": ")
  selectInput(var, label, choices = choices, multiple = multiple)
}
boxHeader <- function(...) {
  box(width = 4, solidHeader = TRUE, ...)
}

fluidRow(
  boxHeader(ngoSelectInput("Trafficking.Type", "trafficking type")),
  boxHeader(ngoSelectInput("Trafficking.Sub.Type", "trafficking sub type")),
  boxHeader(ngoSelectInput("Victim.Gender", "gender"))
)

dfSelectInput <- function(df, var, label, multiple = TRUE) {
  choices <- sort(unique(df[[var]]))
  label <- paste0("Choose a ", label, ": ")
  selectInput(var, label, choices = choices, multiple = multiple)
}

library(purrr)
vars <- tibble::tribble(
  ~ var,                  ~ label,
  "Trafficking.Type",     "trafficking type",
  "Trafficking.Sub.Type", "trafficking sub type",
  "Victim.Gender",        "gender"
)

vars %>%
  pmap(ngoSelectInput) %>% # create one select input for each row
  map(boxHeader) %>%       # wrap each in a boxHeader()
  fluidRow(!!!.)           # collapse into a single fluidRow()

select <-  map(vars$var, function(v) expr(.data[[!!v]] == input[[!!v]]))
select

filter(ngo, !!!select)

# modules ----
library(lubridate)
#>
#> Attaching package: 'lubridate'
#> The following object is masked from 'package:base':
#>
#>     date

ui <- fluidPage(
  textInput("date", "When were you born? (yyyy-mm-dd)"),
  textOutput("error"),
  textOutput("age")
)

server <- function(input, output, session) {
  birthday <- reactive({
    req(input$date)
    ymd(input$date, quiet = TRUE)
  })

  output$error <- renderText({
    if (is.na(birthday())) {
      "Please enter valid date in yyyy-mm-dd form"
    }
  })
  age <- reactive({
    req(birthday())
    (birthday() %--% today()) %/% years(1)
  })
  output$age <- renderText({
    paste0("You are ", age(), " years old")
  })
}

shinyApp(ui, server)

# refactor this using function ----
ymdInputUI <- function(label) {
  label <- paste0(label, " (yyyy-mm-dd)")

  fluidRow(
    textInput("date", label),
    textOutput("error")
  )
}

ymdInputServer <- function(input, output, session) {
  date <- reactive({
    req(input$date)
    ymd(input$date, quiet = TRUE)
  })

  output$error <- renderText({
    if (is.na(date())) {
      "Please enter valid date in yyyy-mm-dd form"
    }
  })

  date
}

ui <- fluidPage(
  ymdInputUI("When were you born?"),
  textOutput("age")
)

server <- function(input, output, session) {
  birthday <- ymdInputServer(input, output, session)
  age <- reactive({
    req(birthday())
    (birthday() %--% today()) %/% years(1)
  })

  output$age <- renderText({
    paste0("You are ", age(), " years old")
  })
}
shinyApp(ui, server)
