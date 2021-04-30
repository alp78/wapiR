dbs <- list.files(path = as.character(stringr::str_glue('C:/Users/{USER}/WATTSIGHT/SQLDB')))
dbs <- dbs[which(substr(dbs, nchar(dbs)-1, nchar(dbs)) == 'db')]

init_db <- as.character(stringr::str_glue('C:/Users/{USER}/WATTSIGHT/SQLDB/{dbs[1]}'))

con <- DBI::dbConnect(RSQLite::SQLite(), init_db)
query <- "SELECT curve_name from metadata"
meta <- DBI::dbGetQuery(conn = con, statement = query)
curve_names <- sort(meta$curve_name, decreasing = F)
DBI::dbDisconnect(con)

ws_logo <- 'C:/Users/{USER}/WATTSIGHT/IMG/wattsight_logo.png'


m <- list(
    l = 80,
    r = 80,
    b = 30,
    t = 30,
    pad = 0
)

ui <- shinyUI(

    fluidPage(
        
        title="DABNAV",

        fluidRow(
            column(width = 2,
                   div(
                       selectInput(inputId = 'database', label = 'db', choices = dbs, multiple = FALSE, width = '150px'),
                       style = "margin-left:30px;margin-top:20px;display:inlin-block;float:left;"
                       )
                   ),
            column(width = 4,
                   div(
                       selectInput(inputId = 'curve_name', label = 'curve', choices = curve_names, multiple = FALSE, width = '350px'),
                       style = "margin-left:10px;margin-top:20px;float:left;"
                       )
                   ),
            column(width = 2,
                   div(
                        shiny::actionButton(inputId = 'reload', label = '', width = '40px', icon = icon('sync-alt')
                        ), style = "margin-left:0px;margin-top:39px;float:right;width:40px;"
                   )
            ),
            column(width = 4,
                   HTML('<img src="wattsight_logo.png", height="33px", width="125px" style="float:right;margin-right:40px;margin-top:30px"/>')
            )
        ),


        tags$style(HTML("div#table.datatables.html-widget.html-widget-output.shiny-bound-output
                         {
                            margin-top: 0px;
                            margin-bottom: 0px;
                            margin-left: 35px;
                            margin-right: 0px;
                            padding: 0px 0px;
                         }")),

        htmlOutput(outputId = 'format_tags_1'),
        htmlOutput(outputId = 'format_tags_2'),

        tags$style(HTML("label
                         {
                            margin-top: 0px;
                            margin-bottom: 0px;
                            margin-left: 5px;
                            margin-right: 0px;
                            padding: 0px 0px;

                         }")),

        tags$style(HTML("td

                         {
                            background-color: #FFFFFF

                         }")),

        fluidRow(htmlOutput(outputId = 'title')),

        fluidRow(

            mainPanel(

                fluidRow(

                    plotly::plotlyOutput('plot', width = '100%')

                    ),

                fixedRow(
                    column(width = 6,
                           div(DT::dataTableOutput(outputId = 'table', width = "360px"),
                               style = "margin:30px;float:left;")
                           ),
                    column(width = 6,
                           div(shiny::tableOutput(outputId = 'metadata'),
                               style = "margin:30px;float:middle;")
                           )
                    ),
                width = '100%'
                )
            )
        )
    )



#### SERVER ####

server <- shinyServer(function(input, output, session) {

    database <- reactiveValues(path = '')
    curve <- reactiveValues(meta = '', tb = '')

    observeEvent(input$database, {
        db_path <- as.character(stringr::str_glue('C:/Users/{USER}/WATTSIGHT/SQLDB/{input$database}'))
        database$path <- db_path

        meta <- SqlGetMetadataTable(db_path = db_path)
        curve_names <- sort(meta$curve_name, decreasing = F)

        updateSelectInput(session, 'curve_name', choices = curve_names)

        curve$meta <- SqlGetCurveMetadata(db_path = database$path, curve_name = curve_names[1])

        curve$tb <- SqlGetCurveTable(db_path = database$path, table_name = curve$meta$table_name)

    })


    observeEvent(input$curve_name, {

        curve$meta <- SqlGetCurveMetadata(db_path = database$path, curve_name = input$curve_name)
        curve$tb <- SqlGetCurveTable(db_path = database$path, table_name = curve$meta$table_name)

    })


    observeEvent(input$reload, {

        curve$tb <- SqlGetCurveTable(db_path = database$path, table_name = curve$meta$table_name)
        curve$meta <- SqlGetCurveMetadata(db_path = database$path, table_name = curve$meta$table_name)

    })


    ### title output
    titleOutput <- function(curve_name) {
        titlePanel(
            title = h2(as.character(stringr::str_glue("{curve_name}")), align = 'center', style = "color:navy; font-weight:bold; padding:0px; margin:0px")
        )
    }

    output$title <- renderUI({ titleOutput(curve_name = curve$meta$curve_name) })


    ### metadata output
    dataMetaOutput <- function(meta) {

        meta_t <- as_tibble(cbind(names(meta), t(meta)))
        colnames(meta_t) <- c('attribute', 'value')

        regexpat <- '\\d{4}-\\d{2}-\\d{2}T.*'

        for (i in 1:length(meta_t$value)) {
            match <- NA
            match <- stringr::str_extract(meta_t$value[i], regexpat)
            if (!is.na(match)) {
                dt <- suppressMessages(lubridate::ymd_hms(meta_t$value[i], tz = tz))
                dt <- strftime(dt, "%Y-%m-%d %H:%M")
                meta_t$value[i] <- dt
            }
        }

        dt <- data.table::data.table(meta_t, keep.rownames = F, stringsAsFactors = F)
        na.omit(dt)
    }

    output$metadata <- shiny::renderTable({dataMetaOutput(curve$meta)}, server = F)


    ### data table output
    dataTableOutput <- function(tb, meta) {


        ts <- tb
        unit <- meta$unit

        ts$times <- suppressMessages(lubridate::ymd_hms(ts$times, tz = tz))
        ts$times <- format(ts$times, "%Y-%m-%d %H:%M")

        ts$values <- round(ts$values, digits = 2)

        dt <- DT::datatable(ts,
                            options = list(info = TRUE,
                                           dom  = 'ilpt',
                                           lengthMenu = list(c(15, 30, 60, -1), c('15', '30', '60', 'All')),
                                           pageLength = 15,
                                           pagingType = 'simple',
                                           autoWidth = FALSE,
                                           columnDefs = list(list(className = 'dt-body-left', targets = -1)),
                                           language = list(
                                               paginate = list(previous = '<<', `next` = '>>'),
                                               info = '[_TOTAL_]',
                                               lengthMenu = '_MENU_'
                                               )
                                           ),
                            selection = 'none',
                            rownames = FALSE) %>%

            DT::formatStyle(columns = c('times', 'values'),
                            target = 'row',
                            lineHeight = '70%')
        return(dt)
    }


    output$table <- DT::renderDataTable({dataTableOutput(curve$tb, curve$meta)}, server = TRUE)


    ### plot output
    dataPlotOuput <- function(tb, meta) {

        tp <- tb
        unit <- meta$unit

        tp$times <- suppressMessages(lubridate::ymd_hms(tp$times, tz = tz))

        fig <- plotly::plot_ly(data = tp, x = ~tp$times, y = ~tp$values, type = "scatter", mode = "lines", line = list(color = 'crimson', width = 1.3)) %>%

            layout(yaxis = list(title = '',
                                showgrid = FALSE,
                                gridcolor = 'darkgray',
                                automargin = TRUE,
                                range = list(min(tp$values), max(tp$values)),
                                showline = TRUE,
                                linewidth = 1.2,
                                linecolor = 'navy',
                                mirror = TRUE),

                   xaxis = list(title = '',
                                showgrid = TRUE,
                                showline = TRUE,
                                linecolor = 'navy',
                                range = list(min(tp$times), max(tp$times)),
                                linewidth = 1.2,
                                mirror = TRUE,
                                type = 'date',
                                tickformat = "%d %b-%y<br>%H:%M"),
                   margin = m) %>%
            layout(hovermode = 'x unified') %>%

            plotly::config(displayModeBar = FALSE, showLink = FALSE)

        return(fig)
    }

    output$plot <- plotly::renderPlotly({dataPlotOuput(curve$tb, curve$meta)})



    tagCount <- reactiveValues(n = -1)

    observeEvent(input$curve_name, {
        tagCount$n <- tagCount$n + 1
    })


    paginateOutput_1 <- function(tag_count) {

        tags$style(HTML(as.character(stringr::str_glue("a#DataTables_Table_{tag_count}_next.paginate_button.next,
                                                       a#DataTables_Table_{tag_count}_next.paginate_button.next.disabled,
                                                       a#DataTables_Table_{tag_count}_previous.paginate_button.previous,
                                                       a#DataTables_Table_{tag_count}_previous.paginate_button.previous.disabled,
                                                       label
                                                       {{
                                                          color: #00000 !important;
                                                          font-family: Arial,Helvetica,sans-serif !important;
                                                          font-weight: normal !important;
                                                       }}"
                                                       )
                                     )
                        )
                   )
    }

    output$format_tags_1 <- renderUI({paginateOutput_1(tag_count = tagCount$n)})

    paginateOutput_2 <- function(tag_count) {

        tags$style(HTML(as.character(stringr::str_glue("div#DataTables_Table_{tag_count}_length.dataTables_length,
                                                       div#DataTables_Table_{tag_count}_paginate.dataTables_paginate.paging_simple,
                                                       div#DataTables_Table_{tag_count}_paginate.dataTables_paginate,
                                                       div#DataTables_Table_{tag_count}_info.dataTables_info,
                                                       a#DataTables_Table_{tag_count}_next.paginate_button,
                                                       a#DataTables_Table_{tag_count}_previous.paginate_button.previous,
                                                       .paginate_button

                                                       {{
                                                          margin-top: 0px;
                                                          margin-bottom: 0px;
                                                          margin-left: 5px;
                                                          margin-right: 5px;
                                                          padding: 0px;
                                                          padding-left: 5px;
                                                          padding-right: 5px;
                                                       }}")
                                     )
                        )
                   )

    }

    output$format_tags_2 <- renderUI({paginateOutput_2(tag_count = tagCount$n)})



})

shinyApp(ui, server)
