#' Trend and cycle analysis in a web browser
#'
#' @description
#' \code{mhp_app()} starts the application of \pkg{mhpfilter}: load a series (the example \code{\link{us_macro}} or
#' a CSV file), filter it with the HP, modified HP or fully modified HP filter, compare the three filters, inspect
#' their weights, gains and end-point loss, filter all the series of a file at once and run the power comparison of
#' Hanif, Iqbal and Choudhary (2017). Every result page shows an interpretation computed from the results
#' (\code{\link{mhp_interpret}()}), the output as R prints it, formatted tables, plots and the R code that repeats the
#' analysis, each with a Copy button.
#'
#' @details
#' The application needs the suggested packages \pkg{shiny}, \pkg{bslib} and \pkg{DT}. Plots are sent to the browser
#' as WebP images when \pkg{magick} is installed (smaller than PNG; PNG otherwise) and can be downloaded as PNG, PDF
#' or WebP. Text is set in IBM Plex Sans and output in IBM Plex Mono when the browser can reach Google Fonts, in the
#' system fonts otherwise.
#'
#' @param launch.browser passed to \code{shiny::runApp()}: \code{TRUE} (default in an interactive session) opens the
#'   application in the web browser.
#' @param ... further arguments to \code{shiny::runApp()}, such as \code{port}.
#' @return \code{mhp_app()} is called for its side effect and returns the value of \code{shiny::runApp()} when the
#'   application is closed; \code{mhp_app_object()} returns the application object (for \code{shiny::runApp()},
#'   \code{shiny::testServer()} or deployment on a server).
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{mhp_filter}}, \code{\link{fmhp_filter}}, \code{\link{fmhp_compare}}, \code{\link{mhp_interpret}}
#' @examples
#' if (interactive()) {
#'   mhp_app(
#'       launch.browser = TRUE
#'     )
#' }
#' @export
mhp_app <- function(launch.browser = interactive(), ...) {
  .mhp_app_need()
  shiny::runApp(mhp_app_object(), launch.browser = launch.browser, ...)
}

#' @rdname mhp_app
#' @export
mhp_app_object <- function() {
  .mhp_app_need()
  shiny::shinyApp(ui = .mhp_app_ui(), server = .mhp_app_server)
}

.mhp_app_need <- function() {
  miss <- c("shiny", "bslib", "DT")[!vapply(c("shiny", "bslib", "DT"), requireNamespace, NA, quietly = TRUE)]
  if (length(miss)) .mhp_stop("mhp_app", "the application needs the package(s) ", paste(miss, collapse = ", "),
                              ": install.packages(c(", paste0("\"", miss, "\"", collapse = ", "), ")).")
}

#-------------------------------------------------------------------------------------------------------------------
# look: theme, fonts, styles, Copy buttons
#-------------------------------------------------------------------------------------------------------------------
.mhp_app_font <- function(family, kind = c("sans", "mono")) {
  fb <- switch(match.arg(kind),
               sans = c("system-ui", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
               mono = c("SFMono-Regular", "Menlo", "Consolas", "Liberation Mono", "DejaVu Sans Mono", "monospace"))
  do.call(bslib::font_collection, c(list(bslib::font_google(family, local = FALSE)), as.list(fb)))
}

.mhp_app_theme <- function() {
  bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#1f4e9e",
                  base_font = .mhp_app_font("IBM Plex Sans", "sans"),
                  heading_font = .mhp_app_font("IBM Plex Sans", "sans"),
                  code_font = .mhp_app_font("IBM Plex Mono", "mono"))
}

.mhp_app_head <- function() {
  shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste(
          "pre.shiny-text-output{font-family:var(--bs-font-monospace);font-size:.85em;line-height:1.5;white-space:pre;"
        , "  overflow-x:auto;background:#f6f8fa;border:1px solid #dde3ea;border-radius:6px;padding:10px 12px;}"
        , "table.dataTable thead th{background:#1f4e9e;color:#fff;font-weight:600;}"
        , ".mhp-bar{display:flex;gap:.4rem;justify-content:flex-end;margin-bottom:.4rem}"
        , ".dataTables_wrapper .dt-buttons{float:left;margin:0 1rem .5rem 0}"
        , ".dataTables_wrapper .dataTables_length{float:left;margin-bottom:.5rem}"
        , ".dataTables_wrapper .dataTables_filter{float:right;margin-bottom:.5rem}"
        , ".dataTables_wrapper .dataTables_length label,.dataTables_wrapper .dataTables_filter label{display:inline-flex;"
        , "  align-items:center;gap:.4rem;font-weight:400;margin:0}"
        , ".dataTables_wrapper select,.dataTables_wrapper input[type=search]{display:inline-block;width:auto}"
        , ".dataTables_wrapper .dataTables_info{float:left;padding-top:.6rem;font-size:.9em}"
        , ".dataTables_wrapper .dataTables_paginate{float:right;padding-top:.4rem}"
        , ".dataTables_wrapper table.dataTable{clear:both}"
        , ".dataTables_wrapper::after{content:'';display:table;clear:both}"
        , ".mhp-plot img{max-width:100%;height:auto}")))
    , shiny::tags$script(shiny::HTML(paste(
          "$(document).on('click', '.mhp-copy', function() {"
        , "  var el = document.getElementById($(this).data('target')); if (!el) return;"
        , "  var txt = el.innerText || el.textContent, b = $(this);"
        , "  var done = function() { b.find('.mhp-copy-label').text('Copied'); setTimeout(function() { b.find('.mhp-copy-label').text('Copy'); }, 1500); };"
        , "  if (navigator.clipboard && window.isSecureContext) { navigator.clipboard.writeText(txt).then(done); return; }"
        , "  var ta = document.createElement('textarea'); ta.value = txt; document.body.appendChild(ta); ta.select();"
        , "  document.execCommand('copy'); document.body.removeChild(ta); done();"
        , "});", sep = "\n")))
    )
}

.mhp_copy_bar <- function(target) {
  shiny::div(class = "mhp-bar",
             shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm mhp-copy",
                                `data-target` = target, shiny::icon("copy"),
                                shiny::span(class = "mhp-copy-label", "Copy")))
}

.mhp_btn <- "btn btn-sm btn-outline-secondary me-1"

# a table for the application: Copy, CSV and Excel buttons, all rows on request, numbers rounded
.mhp_dt <- function(d, title = "table") {
  d <- as.data.frame(d, check.names = FALSE)
  num <- which(vapply(d, is.numeric, NA))
  t <- DT::datatable(d, rownames = FALSE, extensions = "Buttons", style = "auto",
                     class = "compact stripe hover cell-border",
                     options = list(pageLength = 15L, scrollX = TRUE, dom = "Blfrtip",
                                    lengthMenu = list(c(15L, 50L, -1L), c("15", "50", "All")),
                                    buttons = list(list(extend = "copy", title = NULL, className = .mhp_btn),
                                                   list(extend = "csv", title = title, className = .mhp_btn),
                                                   list(extend = "excel", title = title, className = .mhp_btn))))
  pcol  <- num[grepl("^p_|^prob|p_value|^Probability", names(d)[num])]
  whole <- setdiff(num[vapply(d[num], function(v) all(is.na(v) | v == round(v)), NA)], pcol)
  dec   <- setdiff(num, c(whole, pcol))
  if (length(pcol)) t <- DT::formatRound(t, pcol, digits = 3L)
  if (length(dec)) t <- DT::formatSignif(t, dec, digits = 5L)
  if (length(whole)) t <- DT::formatRound(t, whole, digits = 0L)
  t
}

# a ggplot as an image for renderImage(): WebP through magick when installed, PNG otherwise
.mhp_plot_file <- function(p, width = 900, height = 600, res = 110) {
  png <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(png, width = width, height = height, res = res) else
    grDevices::png(png, width = width, height = height, res = res)
  print(p)
  grDevices::dev.off()
  if (.mhp_webp_ok()) {
    webp <- sub("\\.png$", ".webp", png)
    ok <- tryCatch({
      magick::image_write(magick::image_read(png), webp, format = "webp", quality = 100,
                          defines = c(`webp:lossless` = "true"))
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(list(src = webp, contentType = "image/webp", width = "100%", alt = "plot"))
  }
  list(src = png, contentType = "image/png", width = "100%", alt = "plot")
}

# a card with tabs: Interpretation, Output, Table, Plot (each with a Copy button where it is text)
.mhp_result_tabs <- function(id, plot = TRUE, table = TRUE, code = FALSE, extra = NULL) {
  ns <- shiny::NS(id)
  tabs <- list(
      bslib::nav_panel("Interpretation", .mhp_copy_bar(ns("int_text")), shiny::uiOutput(ns("int")),
                       shiny::tags$pre(id = ns("int_text"), class = "shiny-text-output", style = "display:none"))
    , if (plot) bslib::nav_panel("Plot", extra, shiny::div(class = "mhp-plot", shiny::imageOutput(ns("plot"), height = "auto")),
                                 shiny::div(shiny::downloadButton(ns("png"), "PNG", class = "btn-sm"),
                                            shiny::downloadButton(ns("pdf"), "PDF", class = "btn-sm"),
                                            shiny::downloadButton(ns("webp"), "WebP", class = "btn-sm")))
    , if (table) bslib::nav_panel("Table", DT::DTOutput(ns("table")))
    , bslib::nav_panel("Output", .mhp_copy_bar(ns("out")), shiny::verbatimTextOutput(ns("out")))
    , if (code) bslib::nav_panel("R code", .mhp_copy_bar(ns("code")), shiny::verbatimTextOutput(ns("code")))
    )
  do.call(bslib::navset_card_tab, c(list(id = ns("tabs")), Filter(Negate(is.null), tabs)))
}

.mhp_int_html <- function(it) {
  li <- function(s) if (length(s)) shiny::tags$ul(lapply(s, shiny::tags$li)) else NULL
  shiny::tagList(
      shiny::div(class = "alert alert-primary", shiny::strong(it$headline))
    , if (length(it$findings)) shiny::tagList(shiny::h6("Findings"), li(it$findings))
    , if (length(it$recommendations)) shiny::tagList(shiny::h6("Recommendations"), li(it$recommendations))
    , if (length(it$cautions)) shiny::tagList(shiny::h6("Cautions"), li(it$cautions))
    , shiny::tags$details(shiny::tags$summary("How this is read"), shiny::p(it$guide))
    )
}

# the server side of a result card: res() a mhp_result, table_fun(res) a data frame, plot_fun(res) a ggplot
.mhp_result_server <- function(id, res, table_fun = NULL, plot_fun = NULL, code_fun = NULL, hb = shiny::reactive(TRUE)) {
  force(res)
  force(table_fun)
  force(plot_fun)
  force(code_fun)
  shiny::moduleServer(id, function(input, output, session) {
    it <- shiny::reactive({
      shiny::req(res())
      mhp_interpret(res())
    })
    output$int <- shiny::renderUI(.mhp_int_html(it()))
    output$int_text <- shiny::renderText(paste(format(it(), style = "text", width = 100L), collapse = "\n"))
    output$out <- shiny::renderPrint({
      shiny::req(res())
      op <- options(width = 120L)
      on.exit(options(op))
      print(res())
    })
    if (!is.null(table_fun)) output$table <- DT::renderDT({
      shiny::req(res())
      .mhp_dt(table_fun(res()), id)
    })
    if (!is.null(plot_fun)) {
      pl <- shiny::reactive({
        shiny::req(res())
        plot_fun(res(), input)
      })
      output$plot <- shiny::renderImage(.mhp_plot_file(pl()), deleteFile = TRUE)
      output$png <- shiny::downloadHandler(paste0(id, ".png"), function(file)
        ggplot2::ggsave(file, pl(), width = 10, height = 7, dpi = 200))
      output$pdf <- shiny::downloadHandler(paste0(id, ".pdf"), function(file)
        ggplot2::ggsave(file, pl(), width = 10, height = 7))
      output$webp <- shiny::downloadHandler(paste0(id, ".webp"), function(file) mhp_save(pl(), file, width = 10, height = 7))
    }
    if (!is.null(code_fun)) output$code <- shiny::renderText(code_fun())
  })
}

#-------------------------------------------------------------------------------------------------------------------
# pages
#-------------------------------------------------------------------------------------------------------------------
.mhp_app_ui <- function() {
  side <- function(...) bslib::sidebar(width = 300, ...)
  bslib::page_navbar(
      title = "mhpfilter", id = "nav", theme = .mhp_app_theme(), fillable = FALSE, header = .mhp_app_head()
    , bslib::nav_panel("Home", .mhp_app_home())
    , bslib::nav_panel("Data", bslib::layout_sidebar(
          sidebar = side(
              shiny::radioButtons("source", "Data", c(`Example: us_macro (United States, 1959Q1 to 2009Q3)` = "example",
                                                      `CSV file` = "csv"))
            , shiny::conditionalPanel("input.source == 'csv'", shiny::fileInput("file", "CSV file (one series per column)",
                                                                                accept = ".csv"))
            , shiny::selectInput("col", "Series", NULL)
            , shiny::selectInput("freq", "Frequency", c(quarterly = "4", annual = "1", monthly = "12", other = "0"))
            , shiny::numericInput("start", "First year", 1959, step = 1)
            , shiny::numericInput("start_period", "First period of that year", 1, min = 1, step = 1)
            , shiny::checkboxInput("logs", "Filter the logarithm of the series", TRUE))
        , bslib::card(bslib::card_header("The series"), .mhp_copy_bar("data_summary"),
                      shiny::verbatimTextOutput("data_summary"), shiny::plotOutput("data_plot", height = "300px"),
                      DT::DTOutput("data_table"))))
    , bslib::nav_panel("Filter", bslib::layout_sidebar(
          sidebar = side(
              shiny::radioButtons("method", "Filter", c(`Fully modified HP (Hanif et al. 2017)` = "fmhp",
                                                        `Modified HP (McDermott 1997)` = "mhp", `HP (fixed lambda)` = "hp"))
            , shiny::conditionalPanel("input.method == 'hp'", shiny::numericInput("lambda", "lambda", 1600, min = 1))
            , shiny::conditionalPanel("input.method != 'hp'", shiny::numericInput("max_lambda", "Largest lambda searched",
                                                                                  3000, min = 10, step = 100))
            , shiny::conditionalPanel("input.method == 'mhp'", shiny::radioButtons("criterion", "Criterion",
                                      c(`approximate (published)` = "approximate", `exact GCV` = "exact")))
            , shiny::conditionalPanel("input.method == 'fmhp'",
                  shiny::checkboxGroupInput("schemes", "End-point weights", c(linear = "linear", quadratic = "quadratic"),
                                            selected = c("linear", "quadratic"), inline = TRUE)
                , shiny::numericInput("k_linear", "End values, linear weights", 15, min = 1, step = 1)
                , shiny::numericInput("k_quadratic", "End values, quadratic weights (empty: round(min(22, T/2 - 5)))",
                                      NA, min = 1, step = 1)
                , shiny::radioButtons("form", "Form of the weights", c(`rows (published code)` = "rows",
                                                                         `second differences` = "differences")))
            , shiny::actionButton("run", "Filter", class = "btn-primary w-100", icon = shiny::icon("play")))
        , .mhp_result_tabs("fit", code = TRUE, extra = shiny::selectInput("fit_plot", NULL,
                           c(decomposition = "decomposition", `criterion against lambda` = "gcv",
                             `smoothing parameters` = "lambda", `end-point loss` = "loss")))))
    , bslib::nav_panel("Compare", bslib::layout_sidebar(
          sidebar = side(shiny::numericInput("cmp_max", "Largest lambda searched", 3000, min = 10, step = 100),
                         shiny::numericInput("end_share", "Share of observations counted as ends", 0.1, min = 0.01,
                                             max = 0.45, step = 0.01),
                         shiny::actionButton("compare", "Compare", class = "btn-primary w-100"))
        , bslib::card(bslib::card_header("HP, modified HP and fully modified HP"), DT::DTOutput("cmp_table"),
                      shiny::div(class = "mhp-plot", shiny::imageOutput("cmp_plot", height = "auto")))))
    , bslib::nav_panel("Diagnostics", bslib::layout_sidebar(
          sidebar = side(shiny::radioButtons("diag", "Show", c(`end-point loss` = "loss", `gain functions` = "gain",
                                                               `filter weights` = "weights")),
                         shiny::helpText("For the smoothing parameters of the last fit on the Filter page."))
        , .mhp_result_tabs("diag", table = TRUE)))
    , bslib::nav_panel("Batch", bslib::layout_sidebar(
          sidebar = side(shiny::radioButtons("b_method", "Filter", c(`fully modified HP` = "fmhp", `modified HP` = "mhp")),
                         shiny::numericInput("b_max", "Largest lambda searched", 3000, min = 10, step = 100),
                         shiny::radioButtons("b_show", "Plot", c(cycle = "cycle", trend = "trend"), inline = TRUE),
                         shiny::actionButton("batch", "Filter every series", class = "btn-primary w-100"),
                         shiny::helpText("All numeric columns of the data (missing values only at the ends)."))
        , bslib::card(bslib::card_header("Smoothing parameters"), DT::DTOutput("b_table"),
                      shiny::div(class = "mhp-plot", shiny::imageOutput("b_plot", height = "auto")))))
    , bslib::nav_panel("Simulation", bslib::layout_sidebar(
          sidebar = side(shiny::selectizeInput("s_models", "Designs (Table 1 of the paper)", 1:30, selected = c(3, 13, 23),
                                               multiple = TRUE),
                         shiny::numericInput("s_rep", "Replicates per design", 10, min = 1, step = 1),
                         shiny::numericInput("s_T", "Quarterly observations", 200, min = 60, step = 4),
                         shiny::selectInput("s_trend", "Trend", c("linear", "nonlinear")),
                         shiny::selectInput("s_agg", "Aggregation to annual data", c("none", "systematic", "sum", "average")),
                         shiny::numericInput("s_seed", "Seed", 1, step = 1),
                         shiny::actionButton("simulate", "Run", class = "btn-primary w-100"))
        , .mhp_result_tabs("sim", code = FALSE)))
    , bslib::nav_panel("References", bslib::card(bslib::card_header("Sources of the methods"), .mhp_copy_bar("refs"),
                                                 shiny::verbatimTextOutput("refs")))
    )
}

.mhp_app_home <- function() {
  card <- function(title, text) bslib::card(bslib::card_header(title), shiny::p(text))
  shiny::tagList(
      shiny::h4("Trend and cycle of a time series")
    , shiny::p(paste("This application filters macroeconomic series with the HP filter and with its data-driven",
                     "versions: the modified HP filter chooses the smoothing parameter of each series, and the fully",
                     "modified HP filter also reduces the bias of the HP filter at the ends of the series."))
    , bslib::layout_column_wrap(width = 1 / 3,
          card("1. Data", "The example United States series or a CSV file; frequency and logarithms.")
        , card("2. Filter", "HP, modified HP or fully modified HP: decomposition, interpretation and R code.")
        , card("3. Compare", "The three filters side by side: moments of the cycles and end-point loss.")
        , card("4. Diagnostics", "Weights, gains and end-point loss of the fitted filter.")
        , card("5. Batch", "Every series of the file at once.")
        , card("6. Simulation", "The power comparison of Hanif, Iqbal and Choudhary (2017) on simulated series."))
    )
}

.mhp_app_example <- function() {
  e <- new.env()
  utils::data("us_macro", package = "mhpfilter", envir = e)
  e$us_macro
}

.mhp_app_server <- function(input, output, session) {
  raw <- shiny::reactive({
    if (identical(input$source, "csv")) {
      shiny::req(input$file)
      data.table::fread(input$file$datapath, data.table = FALSE)
    } else .mhp_app_example()
  })
  shiny::observeEvent(raw(), {
    nm <- names(raw())[vapply(raw(), is.numeric, NA)]
    nm <- setdiff(nm, c("year", "quarter", "month"))
    shiny::updateSelectInput(session, "col", choices = nm, selected = if ("gdp" %in% nm) "gdp" else nm[1L])
  })
  series <- shiny::reactive({
    d <- raw()
    shiny::req(input$col %in% names(d))
    v <- d[[input$col]]
    v <- v[min(which(!is.na(v))):max(which(!is.na(v)))]
    f <- as.integer(input$freq)
    if (f > 0L) stats::ts(v, start = c(input$start, input$start_period), frequency = f) else v
  })
  output$data_summary <- shiny::renderPrint({
    x <- series()
    cat(sprintf("%s: %d observations%s\n", input$col, length(x),
                if (stats::is.ts(x)) sprintf(", %s to %s", format(stats::time(x)[1L]), format(utils::tail(stats::time(x), 1L))) else ""))
    if (anyNA(x)) cat("The series has missing values inside it: the filters need a complete stretch.\n")
    print(summary(as.numeric(x)))
  })
  output$data_plot <- shiny::renderPlot({
    x <- series()
    d <- data.table::data.table(time = if (stats::is.ts(x)) as.numeric(stats::time(x)) else seq_along(x),
                                value = as.numeric(x))
    ggplot2::ggplot(d, ggplot2::aes(time, value)) + ggplot2::geom_line(colour = .mhp_palette[["Modified HP"]]) +
      .mhp_x_cont() + .mhp_y_cont() + ggplot2::labs(x = NULL, y = input$col) + mhp_theme()
  })
  output$data_table <- DT::renderDT(.mhp_dt(raw(), "data"))

  fit <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$run, {
    x <- series()
    r <- tryCatch(shiny::withProgress(message = "Filtering", value = 0.5, switch(input$method,
           hp   = hp_filter(x, lambda = input$lambda, as_dt = FALSE, log_scale = isTRUE(input$logs)),
           mhp  = mhp_filter(x, max_lambda = input$max_lambda, as_dt = FALSE, log_scale = isTRUE(input$logs),
                             criterion = input$criterion),
           fmhp = fmhp_filter(x, max_lambda = input$max_lambda, k_linear = input$k_linear,
                              k_quadratic = if (is.na(input$k_quadratic)) NULL else input$k_quadratic,
                              schemes = input$schemes, form = input$form, log_scale = isTRUE(input$logs),
                              as_dt = FALSE))), error = function(e) e)
    if (inherits(r, "error")) {
      shiny::showNotification(conditionMessage(r), type = "error", duration = 10)
      return()
    }
    fit(r)
  })
  .mhp_result_server("fit", fit, function(x) data.table::data.table(time = .mhp_time(x), original = x$original,
                                                                      trend = x$trend, cycle = x$cycle),
                     function(x, i) {
                       w <- input$fit_plot
                       if (w == "gcv" && !inherits(x, "mhp_mhp")) w <- "decomposition"
                       if (w %in% c("lambda", "loss") && !inherits(x, "mhp_fmhp")) w <- "decomposition"
                       plot(x, which = w)
                     },
                     shiny::reactive(.mhp_app_code(input)))
  cmp <- shiny::eventReactive(input$compare, {
    fmhp_compare(series(), frequency = switch(input$freq, `1` = "annual", `12` = "monthly", "quarterly"),
                 max_lambda = input$cmp_max, log_scale = isTRUE(input$logs), end_share = input$end_share)
  })
  output$cmp_table <- DT::renderDT(.mhp_dt(cmp(), "comparison"))
  output$cmp_plot <- shiny::renderImage({
    x <- series()
    .mhp_plot_file(plot_comparison(if (isTRUE(input$logs)) log(x) else x,
                                   frequency = switch(input$freq, `1` = "annual", `12` = "monthly", "quarterly"),
                                   max_lambda = input$cmp_max, show_cycle = TRUE, fmhp = TRUE))
  }, deleteFile = TRUE)
  dg <- shiny::reactive({
    shiny::req(fit())
    switch(input$diag, loss = hp_loss(fit()), gain = hp_gain(fit()), weights = hp_weights(fit()))
  })
  .mhp_result_server("diag", dg, function(x) switch(.mhp_cls(x), loss = x$loss, gain = x$gain,
                                                      weights = as.data.frame(x$weights[x$rows, , drop = FALSE])),
                     function(x, i) plot(x))
  bat <- shiny::eventReactive(input$batch, {
    d <- raw()
    X <- as.matrix(d[, setdiff(names(d)[vapply(d, is.numeric, NA)], c("year", "quarter", "month")), drop = FALSE])
    if (isTRUE(input$logs) && input$b_method == "mhp") X <- log(X)
    tryCatch(shiny::withProgress(message = "Filtering", value = 0.5,
             if (input$b_method == "fmhp") fmhp_batch(X, max_lambda = input$b_max, log_scale = isTRUE(input$logs)) else
               mhp_batch(X, max_lambda = input$b_max)), error = function(e) {
                 shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
                 NULL
               })
  })
  output$b_table <- DT::renderDT({
    shiny::req(bat())
    .mhp_dt(attr(bat(), "lambdas"), "lambdas")
  })
  output$b_plot <- shiny::renderImage({
    shiny::req(bat())
    .mhp_plot_file(plot_batch(bat(), show = input$b_show, facet = TRUE))
  }, deleteFile = TRUE)
  sim <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$simulate, {
    r <- tryCatch(shiny::withProgress(message = "Simulating", value = 0.5,
             fmhp_simulation(designs = mhp_designs()[as.integer(input$s_models)], n_rep = input$s_rep, T = input$s_T,
                             trend = input$s_trend, aggregate = input$s_agg, seed = input$s_seed)),
             error = function(e) e)
    if (inherits(r, "error")) {
      shiny::showNotification(conditionMessage(r), type = "error", duration = 10)
      return()
    }
    sim(r)
  })
  .mhp_result_server("sim", sim, function(x) x$rmse, function(x, i) plot(x))
  output$refs <- shiny::renderPrint({
    op <- options(width = 110L)
    on.exit(options(op))
    print(mhp_cite())
  })
}

# the R code of the current fit, every argument on its own line
.mhp_app_code <- function(input) {
  csv <- identical(input$source, "csv")
  f <- as.integer(input$freq)
  load <- c(if (csv) "dat <- data.table::fread(\"data.csv\")" else c("data(us_macro)", "dat <- us_macro"),
            if (f > 0L) sprintf("x <- ts(dat$%s, start = c(%d, %d), frequency = %d)", input$col, as.integer(input$start),
                                as.integer(input$start_period), f) else sprintf("x <- dat$%s", input$col))
  vals <- switch(input$method,
    hp   = c(x = "x", lambda = format(input$lambda), as_dt = "FALSE", log_scale = as.character(isTRUE(input$logs))),
    mhp  = c(x = "x", max_lambda = format(input$max_lambda), as_dt = "FALSE",
             log_scale = as.character(isTRUE(input$logs)), criterion = sprintf("\"%s\"", input$criterion)),
    fmhp = c(x = "x", max_lambda = format(input$max_lambda), k_linear = format(input$k_linear),
             k_quadratic = if (is.na(input$k_quadratic)) "NULL" else format(input$k_quadratic),
             alpha_linear = "seq(from = 0, to = 2000, by = 10)", alpha_quadratic = "seq(from = 0, to = 2000, by = 5)",
             schemes = sprintf("c(%s)", paste0("\"", input$schemes, "\"", collapse = ", ")),
             form = sprintf("\"%s\"", input$form), lambda = "NULL", log_scale = as.character(isTRUE(input$logs)),
             as_dt = "FALSE"))
  fun <- c(hp = "hp_filter", mhp = "mhp_filter", fmhp = "fmhp_filter")[[input$method]]
  w <- max(nchar(names(vals)))
  body <- paste0("    ", c(" ", rep(",", length(vals) - 1L)), " ", formatC(names(vals), width = -w), " = ", vals,
                 collapse = "\n")
  paste0("library(mhpfilter)\n", paste0(load, "\n", collapse = ""), "fit <-\n  ", fun, "(\n", body,
         "\n    )\nfit\nplot(x = fit)\nmhp_interpret(\n    x   = fit\n  , top = 3\n  )\n")
}
