library(shiny)
library(tidyverse)

# All caps values should be considered constants and not modified.
# Minimum and maximum allowable sample size:
SAMP_MIN <- 2
SAMP_MAX <- 50

# Range of x for the graphis 
XLIM_HI <- 6
XLIM_LO <- -6

# Initial control values:
DEFAULT_SAMP_N <- 30
DEFAULT_SAMP_MEAN <- 1
DEFAULT_SAMP_SD <- 2
DEFAULT_SAMP_ALPHA <- 0.05
DEFAULT_POP_MEAN <- 0

# ui:
ui <- fluidPage(
    titlePanel("T-test Visualizer"),
    # Let us use LaTeX in labels and text
    withMathJax(),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Controls describing underlying distribution"),
            sliderInput(
                inputId = "samp_mean",
                label = paste0("Sample distribution mean ",
                               "(\\(\\bar{x}\\)):"),
                min =  1 + XLIM_LO,
                max = -1 + XLIM_HI,
                step = 0.1,
                value = DEFAULT_SAMP_MEAN
            ),
            # Resample when the sd is changed.
            sliderInput(
                inputId = "samp_sd",
                label = paste0("Sample distribution sd ",
                               "(\\(s\\)):"),
                min = 0.1,
                max = 2.5,
                step = 0.1,
                value = DEFAULT_SAMP_SD
            ),
            hr(),
            h4("Controls describing the sample"),
            helpText("Size of sample to draw and test:"),
            sliderInput(
                inputId = "samp_n",
                label = paste0("Sample size ",
                               "(\\(n\\)):"),
                min = SAMP_MIN,
                max = SAMP_MAX,
                step = 1,
                value = DEFAULT_SAMP_N
            ),
            actionButton("resample", "Resample"),

            hr(),
            sliderInput(
                inputId = "samp_alpha",
                label = paste0("Sample alpha ",
                               "(\\(\\alpha\\)):"),
                min = 0.01,
                max = 1.0,
                step = 0.01,
                value = DEFAULT_SAMP_ALPHA
            ),
            
            sliderInput(
                inputId = "pop_mu",
                label = paste0("Population mean ",
                               "(\\(\\mu\\)):"),
                min =  1 + XLIM_LO,
                max = -1 + XLIM_HI,
                step = 0.1,
                value = DEFAULT_POP_MEAN
            ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h4("Sample plot:"),
            plotOutput("samplePlot", height = "300"),
            h4("Distribution plot with t-statistic:"),
            plotOutput("distPlot"),
            h4("T-statistic equation:"),
            uiOutput("t.equ"),
            h4("Raw t-test output:"),
            verbatimTextOutput("t.out")
        )
    ),
    hr(),
    h2("Disussion", id = "discussion"),
    markdown("
### Functionality

This app runs t-tests on small samples of data drawn from a normal distribution,
against a hypothesized population mean. The sample can be adjusted by size, mean,
and standard deviation, and the t-test alpha threshold can be chosen. The graph
draws the appropriate t-distribution for the sample size, and shows the mean,
the confidence intervals, the critical thresholds, and the t-statistic.

The default setting of the app sets the sample mean at 1 and the population
mean at 0, so that there will be some difference. The default standard deviation
is 2, and the sample size is 30.

### Purpose and scope

The purpose of this app was to serve as a learning tool for myself, to better
understand the underlying meaning of t-tests, and in particular what the
t-statistic represents.

Coming from an engineering background without a great deal of formal
statistics, I felt reasonably confident running t-tests in code and working
with the output, but I didn't have an intuitive sense of what the various
components meant. Also, there were many concepts I was conflating, like the
difference between confidence intervals and critical values / rejection zones,
and some other areas I would have been unclear on if called to explain, like:
which distribution curve do you plot the tails on? The population
distribution? The sample distribution? A curve of the sample itself?

Visuals are an essential part of working with statistics; sets of data that could be wildly different can have the same means, standard deviations, so visual exploration is an indispensible element of statistical analysis, for two reasons:

1. They can offer a way to distinguish (visually) differences in data that can
be hard to detect in code, or in the results of mathematical operations, and

1. They can lead to intuitive understandings in a different way than pure
mathematical analysis; many people self-describe as \"visual learners\" who
understand graphical representations of data better than tables and formulas.

The working process was to do all of the t-test calculations manually, and
compare them with the results of the R `t.test()` function. Doing so forced me
to get correct answers to all of the parts of the process that I was unsure
about.

The biggest takeaway for me personally, at this point, is the ability to see
how the t statistic changes (quickly) in response to small changes in the
sample mean. When the t-value falls into a critical zone, the p-value is below
the alpha threshold, and the result is statistically significant. This is not
a concept I could have articulated clearly before building this visualizer.

### Bugs and issues
    
- Need critical values in legend
- Need to parameterize height of graph
- get LaTeX in the legend, mu = $\\mu$
    
### Future directions

Unfortunately a rewrite of the code didn't do much to improve rendering speed.

Next, I want to add support for one-tailed tests.

Ultimately, I'd like to make a tab-based interface with similar structures for
other types of tests, like ANOVA and Chisq, and possibly even linear models if
I can work out a common interface.

### Source code

Available on GitHub:

https://github.com/slinberg-umass/t-test-visualizer

### Contact
    
Steve Linberg<br />
steve@slinberg.net<br />
https://slinberg.net

### Class comments

- Allow text inputs to controls


")
)

# server:
server <- function(input, output) {
    
    # Store a cache of the sample values. This lets us call the 
    # reactive function whenever resample_needed is touched, 
    # but react to its value (true or false) rather than just the
    # fact that it changed. We might not want to resample if it
    # changes from true to false, for instance.
    # Not sure this will really come into play, but we might add
    # a control that turns resampling off temporarily.
    sample_cache <- reactiveVal(c())
    
    # initialize resample_needed; the samp_vals reactive variable will
    # react to changes in it and force re-evaluation of the sample.
    resample_needed <- reactiveVal(FALSE)
    
    # Set resample_needed on a change to "SD"...
    observeEvent(input$samp_sd, resample_needed(TRUE))
    # ...or on a click of "Resample"
    observeEvent(input$resample, resample_needed(TRUE))
    
    # samp_vals is the array of values we're plotting and testing.
    # Resample the values when resample_needed()
    # changes to TRUE.
    samp_vals <- reactive({
        if (resample_needed()) {
            # message("recalcing sample, sd input = ", input$samp_sd)
            # create a new sample and store it in the cache.
            new_sample <- rnorm(SAMP_MAX, mean = 0, sd = input$samp_sd) 
            sample_cache(new_sample)
            # set resample_needed to false so we don't regenerate
            # if called again before it's reset to true.
            resample_needed(FALSE)
        }
        # Return the cache whether or not we changed it.
        sample_cache()
    })
    
    # The first samp_n results from the samp_vals array.
    # We do this so that we can change the sample size without
    # altering the underlying sample, so we can see what happens
    # in isolation when everything else is held the same.
    samp <- reactive(samp_vals()[1:input$samp_n] + input$samp_mean)

    # The mean, sd and n of samp()
    samp_mean <- reactive(mean(samp()))
    samp_sd <- reactive(sd(samp()))
    # Could also just use input$samp_n here, but this is cleaner
    samp_n <- reactive(length(samp()))
    samp_n_sqrt <- reactive(sqrt(samp_n()))
    samp_dataframe <- reactive({
        as.data.frame(samp()) %>%
            setNames(c("v"))
    })
    
    # Run the t test when the sample changes, or the mu/alpha inputs.
    samp_t_out <- reactive(t.test(
        samp(),
        mu = input$pop_mu,
        conf.level = 1 - input$samp_alpha
    ))
    
    # Show the t-test output
    # renderPrint instead of renderText to get the "printed" version of the
    # output; otherwise we get the list structure and its elements (which
    # we may also want somewhere else)
    output$t.out <- renderPrint(samp_t_out())

    # Show the nice LaTeX-formatted equation for the t-statistic calculation.
    output$t.equ <- renderUI({

        samp <- samp()
        samp_mean <- samp_mean()
        samp_sd <- samp_sd()
        samp_n <- samp_n()
        samp_n_sqrt <- samp_n_sqrt()
        
        withMathJax(
            helpText(paste0('$$t = \\frac{\\text{diff. in means}}{\\text{std. error}}',
                            ' = \\frac{\\bar{x} - \\mu}{s / \\sqrt{n}}',
                            # equation with values substituted
                            ' = \\frac{', round(samp_mean, 4), ' - ', input$pop_mu,
                            '}{', round(samp_sd, 4), ' / \\sqrt{', samp_n, '}}',
                            # equation with values calculated
                            ' = \\frac{', round(samp_mean, 4) - input$pop_mu,
                            '}{', round(samp_sd / samp_n_sqrt, 4), '}',
                            # result
                            ' = ', round((samp_mean - input$pop_mu)
                                         / (samp_sd / samp_n_sqrt), 4),
                            # end the expression
                            '$$'
                            ))
        )
    })
    
    # Render the sample view as a violin plot.
    # Add vertical indicators for the hypothesis mean and the local mean.
    output$samplePlot <- renderPlot({
        
        samp_dataframe <- samp_dataframe()
        
        samp_mean <- samp_mean()
        samp_sd <- samp_sd()
        samp_n <- samp_n()
        samp_n_sqrt <- samp_n_sqrt()
        
        samp_degf <- samp_n - 1
        t.crit <-
            qt(1 - input$samp_alpha / 2,
               df = samp_degf,
               lower.tail = FALSE)
        
        # se = t.out$stderr
        se <- samp_sd / samp_n_sqrt
        # me = margin of error
        # not in t.out, but is t.out$conf.int[1] - t.out$estimate or
        #                      t.out$conf.int[2] - t.out$estimate
        me <- t.crit * se
        # samp.xbar = t.out$estimate
        # ci.95 = t.out$conf.int[1:2]
        ci <- c(samp_mean - me, samp_mean + me)
        ### 
        
        ggplot(samp_dataframe, aes(x = v)) +
            coord_cartesian(xlim = c(XLIM_LO, XLIM_HI)) +
            theme_minimal() +
            theme(
                legend.position = c(0.87, 0.80),
                legend.background = element_rect(fill = alpha('blue', 0.1)),
                # Remove all y axis information, not needed
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            ) +
            scale_color_manual(
                name = "Legend",
                breaks = c("samp", "xbar", "conf"),
                values = c(
                    "samp" = "red",
                    "xbar" = "darkgreen",
                    "conf" = "orange"
                )
            ) +
            scale_x_continuous(name = "value", breaks = seq(XLIM_LO, XLIM_HI, 0.5)) +
            # scale_y_continuous(name ="density", breaks = seq(0, 0.45, 0.05)) +
            
            geom_point(data = samp_dataframe,
                       mapping = aes(x = v, y = "", color = "samp")) +
            geom_violin(data = samp_dataframe,
                        mapping = aes(x = v, y = ""),
                        alpha = 0.5) +
            geom_vline(aes(xintercept = samp_mean, color = "xbar"),
                       linetype = "dashed") +
            geom_vline(aes(xintercept = ci[1], color = "conf"),
                       linetype = "twodash") +
            geom_vline(aes(xintercept = ci[2], color = "conf"),
                       linetype = "twodash") +
            annotate(
                "rect",
                xmin = ci[1],
                xmax = ci[2],
                ymin = 0,
                ymax = 2,
                alpha = .1,
                fill = "orange"
            )
    })
    
    output$distPlot <- renderPlot({
        
        samp_dataframe <- samp_dataframe()
        samp_mean <- samp_mean()
        samp_sd <- samp_sd()
        samp_n <- samp_n()
        samp_degf <- samp_n() - 1
        samp_n_sqrt <- samp_n_sqrt()
        samp_t_out <- samp_t_out()
        t.stat <- samp_t_out()$statistic

        t.crit <-
            qt(1 - input$samp_alpha / 2,
               df = samp_degf,
               lower.tail = FALSE)
        
        base <-
            ggplot(samp_dataframe, aes(x = v)) +
            coord_cartesian(xlim = c(XLIM_LO, XLIM_HI)) +
            theme_minimal() +
            theme(legend.position = c(0.87, 0.80),
                  legend.background = element_rect(fill = alpha('blue', 0.1)),
                  # Remove all y axis information, not needed
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()) +
            scale_color_manual(
                name = "Legend",
                breaks = c("t-dist", "mu", "xbar"),
                values = c(
                    "t-dist" = "black",
                    "mu" = "darkgreen",
                    "xbar" = "red"
                )
            ) +
            scale_x_continuous(name = "value", breaks = seq(XLIM_LO, XLIM_HI, 0.5)) +
            scale_y_continuous(name = "density", breaks = )
        
        theplot <- base +

            geom_function(aes(color = "t-dist"),
                          fun = dt,
                          # args = list(df = samp_df, ncp = input$samp_mean)) +
                          args = list(df = samp_degf)) +
            
            stat_function(
                fun = "dt",
                geom = "area",
                fill = "steelblue",
                xlim = c(abs(t.crit), XLIM_HI),
                args = list(df = samp_degf)
            ) +
            stat_function(
                fun = "dt",
                geom = "area",
                fill = "steelblue",
                xlim = c(XLIM_LO, -abs(t.crit)),
                args = list(df = samp_degf)
            ) +

            geom_vline(aes(xintercept = samp_mean, color = "xbar"),
                       linetype = "dashed") +
            geom_vline(aes(xintercept = input$pop_mu, color = "mu"),
                       linetype = "dotted") +
            geom_vline(aes(xintercept = t.stat),
                       color = "black",
                       linetype = "solid")
        
        # Draw an arrow from mu to T if they're far enough apart
        
        if (abs(t.stat) > 0.4) {
            if (t.stat > XLIM_HI || t.stat < XLIM_LO) {
                xend <- ifelse(t.stat < XLIM_LO, XLIM_LO, XLIM_HI)
                head_length <- 0.8
            } else {
                xend <- t.stat
                head_length <- 0.3
            }
            theplot <- theplot +
                # if T would be off either end, cap it at the ends and 
                # make the arrowheads bigger
                geom_segment(aes(
                    x = input$pop_mu,
                    y = 0.2,
                    xend = xend,
                    yend = 0.2
                ),
                arrow = arrow(length = unit(head_length, "cm"), ends = "both"))
        }
        
        theplot

    })
}

shinyApp(ui = ui, server = server)
