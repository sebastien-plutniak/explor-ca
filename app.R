pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

shiny::shinyApp(
  ui = navbarPage("Correspondance Analysis", # UI ----
                  theme = shinythemes::shinytheme("flatly"),
                  tabPanel("Import",
                        fluidRow(
                            column(3),
                             column(6,
                           h2("Presentation"),
                           HTML(paste(
                           "<p> This app runs a 
                           <a href=https://en.wikipedia.org/wiki/Correspondence_analysis target=_blank>Correspondance Analysis</a>
                            and allows to interactively explore  the results.
                           </p>
                           <p>
                           An adapted version of 
                           the <a href=https://cran.r-project.org/package=explor target=_blank><i>explor</i></a> R package, this application is designed to work with archaeological data exported from the <a href=https://cran.r-project.org/package=archeoViz target=_blank>archeoViz</a> application. However, data can also be imported using the buttons below.
                           </p>
                           <p>  <i>explor</i> is developped by <a href=https://github.com/juba/explor target=blank>Julien Barnier</a>. The code of this adapted version is available on <a href=https://github.com/sebastien-plutniak/explor-ca target=_blank>github</a>.</p>")), 
                           h2("Imported Data"),
                           DT::DTOutput("importedData"),
                           fileInput('input.file', "Import a CSV file",
                                                 accept=c('text/csv', 'text/comma-separated-values, text/plain')),
                           radioButtons(inputId = 'sep1',
                                                    label = "Separator",
                                                    choices = c("," =',', ";"=';',"tab"='\t'),
                                                    inline=T, selected = ','),
                           radioButtons(inputId = 'dec.sep1',
                                                    label = "Decimal",
                                                    choices = c("." ='.', ","),
                                                    inline=T, selected = '.')
                             ))
                           ), # end panel 
                  
                  tabPanel(gettext("Eigenvalues"),
                           uiOutput("explor_multi_eigenUI")
                           ),
                  
                  tabPanel(gettext("Plot"),
                           fluidRow(
                             column(2,
                                    wellPanel(
                                        uiOutput("explor_multi_axes_input"),
                                        uiOutput("var_lab_size"),
                                        uiOutput("explor_multi_auto_labels_input"),
                                        uiOutput("var_point_size"),
                                        uiOutput("explor_multi_min_contrib_input"), 
                                        uiOutput("explor_multi_var_col_input"),
                                        uiOutput("explor_multi_var_symbol_input"),
                                        uiOutput("explor_multi_var_size_input"),
                                        uiOutput("var_hide"),
                                        uiOutput("lev_sup"),
                                        uiOutput("var_sup"),
                                        uiOutput("input.var_sup"),
                                        uiOutput("explor_multi_sidebar_footer")
                                        
                  )),
                             column(10,
                                    scatterD3Output("varplot", width = "100%"))
                           )),
                  
                  tabPanel(gettext("Data"),
                           uiOutput("explor_multi_var_dataUI")
                  )
  ),
  
  server = function(input, output, session) {  # SERVER----

    # LOAD DATA ----
    
    input.file <- reactive({
      # waiting objects csv file:
      validate(need(input$input.file, message = ""))
      input$input.file
    })
    
    dataset <- reactive({
      query <- shiny::parseQueryString(session$clientData$url_search)
      
      if( length(query) != 0 ){
        df <- utils::read.table(url(as.character(query)),
                              row.names = 1, header = T, sep = ",")
      } else{
        df <- utils::read.csv(input.file()$datapath, 
                              header=T, #quote = "",
                              # sep = input$sep1, dec = input$dec.sep1,
                              stringsAsFactors = F)
      }
      
      # library(FactoMineR)
      # data(children)
      # df <- children
      # df <- children[1:10, 1:7]
      df
    })
    
    output$importedData <- DT::renderDT({
      dataset()
    })
    
    
    # Compute CA ----
    obj <- reactive({ 
      try(ca.res <- FactoMineR::CA(dataset(), graph = FALSE))
      
      if( ! exists("ca.res")){
        showNotification("Impossible to compute a CA with this dataset. Please, edit the data in archeoViz and resubmit",
                         type = "error",
                         duration = 99)
        return()
      } 
      ca.res
    })
    
    #  res  object ----
    res <- reactive({
      req(obj())
      prepare_results(obj())
    })
      
    # settings object----
    settings <- reactive({
      req(obj() )
      obj <- obj()
      
      settings <- list()
      settings$var_columns <- c("Level", "Position", "Coord", "Contrib", "Cos2", "Count")
      settings$varsup_columns <- c("Level", "Position", "Coord", "Cos2", "Count")
      settings$obj_name <- deparse(substitute(obj))    
      
      settings$has_count <- TRUE
      settings$has_contrib <- TRUE
      settings$has_cos2 <- TRUE
      settings$has_var_eta2 <- FALSE
      settings$has_varsup_eta2 <- FALSE
      
      settings$has_sup_levels <- "Supplementary level" %in% res()$vars$Type
      settings$has_sup_vars <- "Supplementary variable" %in% res()$vars$Type
      settings$type <- "CA"
      
      settings
    })
    
    # 
    output$tabi <- renderTable({
      res()$eig
    })
    
    
    
    # VARIABLE PLOT ----
    # variables plot code ----
    varplot_code <- reactive({
      req(input$var_col, input$var_size, settings(), res() )
      settings <- settings()
      res <- res()
      
      col_var <- if (input$var_col == "None") NULL else input$var_col
      symbol_var <- if (input$var_symbol == "None") NULL else input$var_symbol
      size_var <- if (is.null(input$var_size) || input$var_size == "None") NULL else input$var_size
      size_range <- if (is.null(input$var_size) || input$var_size == "None") c(10,300) else c(30,400) * input$var_point_size / 32
      var_auto_labels <- if (!is.null(input$var_auto_labels) && input$var_auto_labels) "\"auto\"" else "NULL"
      var_sup <- settings$has_sup_vars && input$var_sup
      var_sup_choice <- if(var_sup) paste0(utils::capture.output(dput(input$var_sup_choice)), collapse="") else NULL
      
      
      paste0("explor::CA_var_plot(res(), ",
             "xax = ", input$var_x, 
             ", yax = ", input$var_y,
             ", lev_sup = ", settings$has_sup_levels && input$lev_sup,
             ", var_sup = ", var_sup,
             ", var_sup_choice = ", var_sup_choice,
             ", var_hide = '", input$var_hide, "'",
             ", var_lab_min_contrib = ", input$var_lab_min_contrib,
             ", col_var = ", deparse(substitute(col_var)),
             ", symbol_var = ", deparse(substitute(symbol_var)),
             ", size_var = ", deparse(substitute(size_var)),
             ", size_range = ", deparse(size_range),
             ", labels_size = ", input$var_lab_size,
             ", point_size = ", input$var_point_size,
             ", transitions = ", input$var_transitions,
             ", labels_positions = ", var_auto_labels)
    })
    
    # Variables plot ----
    output$varplot <- scatterD3::renderScatterD3({
      req(varplot_code())
      code <- paste0(varplot_code(), ", in_explor = TRUE)")
      eval(parse(text = code))
    })
    
    ## Variables plot code export modal dialog
    observeEvent(input$explor_var_plot_code, {
      showModal(code_modal(settings()$obj_name, 
                           varplot_code(),
                           explor_multi_zoom_code(input$var_zoom_range)
      ))
    })
    
    # DATA MODULE ----
      # callModule(explor_multi_var_data,
      #          "var_data",
      #          reactive(res),
      #          reactive(settings)
      #          )
    
    table_options <- list(lengthMenu = c(10,20,50,100),
                          pageLength = 10, orderClasses = TRUE,
                          autoWidth = FALSE, searching = TRUE)
    # Active variables
    varTable <- reactive({
      tmp <- res()$vars %>% 
        filter(Type == "Active", Axis == input$vardim) %>%
        select(all_of(settings()$var_columns))
      ## CA data hide option
      if (settings()$type == "CA" && input$var_tab_hide != "None") {
        tmp <- tmp %>% filter(Position != input$var_tab_hide)
      }
      data.frame(tmp)
    })
    vartable_sort <- reactive({
      if(settings()$has_contrib) "Contrib" else "Coord"
    })
    output$vartable <- DT::renderDT(
      explor_multi_table(varTable(), table_options, vartable_sort()))
    
    ## Supplementary variables
    varTableSup <- reactive({
      tmp <- res()$vars %>% 
        filter(grepl("Supplementary", Type), Axis == input$vardim) %>%
        mutate(Level = ifelse(Class == "Quantitative", "-", Level))
      ## CA data hide option
      if (settings()$type == "CA" && input$var_tab_hide != "None") {
        tmp <- tmp %>% filter(Position != input$var_tab_hide)
      }
      ## PCA with qualitative supplementary
      if (settings()$type == "PCA" && settings()$has_quali_sup_vars) {
        tmp <- tmp %>% filter(Class == "Quantitative")
      }
      tmp <- tmp %>% select(all_of(settings()$varsup_columns))
      data.frame(tmp)
    })
    
    output$vartablesup <- DT::renderDT(
      explor_multi_table(varTableSup(), table_options, "Coord"))
    
    ## PCA qualitative supplementary variable
    varTableQualiSup <- reactive({
      if (settings()$type == "PCA" && settings()$has_quali_sup_vars) {
        tmp <- res()$vars %>% 
          filter(Type == "Supplementary", Class == "Qualitative",
                 Axis == input$vardim) %>%
          select(all_of(settings()$varsup_quali_columns))
        data.frame(tmp)
      }
    })
    output$vartablequalisup <- DT::renderDT(
      explor_multi_table(varTableQualiSup(), table_options, "Coord"))
    
    ## Variables eta2
    varTableEta2 <- reactive({
      if (settings()$has_var_eta2) {
        res()$vareta2 %>% filter(Type == "Active", Axis == input$vardim) %>%
          select(all_of(settings()$vareta2_columns)) %>%
          arrange(eta2) %>%
          mutate(eta2 = format(eta2, scientific = FALSE, nsmall = 3, digits = 1))
      }
    })
    output$vartableeta2 <- DT::renderDT(
      explor_multi_table(varTableEta2(), table_options, "eta2"))
    
    ## Supplementary variables eta2
    varTableSupEta2 <- reactive({
      if (settings()$has_varsup_eta2) {
        res()$vareta2 %>% filter(Type == "Supplementary",
                                 Class == "Qualitative",
                                 Axis == input$vardim) %>%
          select(all_of(settings()$varsupeta2_columns)) %>%
          arrange(eta2) %>%
          mutate(eta2 = format(eta2, scientific = FALSE, nsmall = 3, digits = 1))
      }
    })
    output$vartablesupeta2 <- DT::renderDT(
      explor_multi_table(varTableSupEta2(), table_options, "eta2"))
    
    
    ## Lasso modal dialog ----
    observeEvent(input$show_lasso_modal, {
      showModal(modalDialog(
        title = gettext("Lasso selection"),
        HTML(input$show_lasso_modal),
        easyClose = TRUE
      ))
    })

    output$explor_multi_axes_input <- renderUI({
      explor_multi_axes_input(res(), "var")
    })
    
    output$var_lab_size <- renderUI({
      sliderInput("var_lab_size",
                  gettext("Labels size"),
                  0, 20, 10)
    })
    
    output$explor_multi_auto_labels_input <- renderUI({
      explor_multi_auto_labels_input(res()$vars, "var")
    })
    
    output$var_point_size <- renderUI({
      sliderInput("var_point_size",
                  gettext("Points size"),
                  4, 128, 56)
    })
    
    output$explor_multi_min_contrib_input <- renderUI({
      explor_multi_min_contrib_input(res()$vars, settings(), "var")
    })                 
    
    output$explor_multi_var_col_input <- renderUI({
      explor_multi_var_col_input(settings())
    })
    
    output$explor_multi_var_symbol_input <- renderUI({
      explor_multi_var_symbol_input(settings())
    })
    
    output$explor_multi_var_size_input <- renderUI({
      explor_multi_var_size_input(settings())
    })
    
    output$var_hide <- renderUI({
      selectInput("var_hide",
                  gettext("Hide :"),
                  choices = explor_multi_hide_choices(),
                  selected = "None")
    })
    
    output$lev_sup <- renderUI({
      if(settings()$has_sup_levels)
        checkboxInput("lev_sup",
                      HTML(gettext("Supplementary levels")),
                      value = TRUE)
    })
    
    output$var_sup <- renderUI({
      if(settings()$has_sup_vars)
        checkboxInput("var_sup",
                      HTML(gettext("Supplementary variables")),
                      value = TRUE)
    })
    
    output$input.var_sup <- renderUI({
      if(settings()$has_sup_vars)
        conditionalPanel("input.var_sup",
                         explor_multi_var_sup_choice_input(res()$vars, settings() ))
    })
    
    
    output$explor_multi_sidebar_footer <- renderUI({
      explor_multi_sidebar_footer(type = "var")
    })
    
    
    ## data explorer ----
    output$explor_multi_var_dataUI <- renderUI({
      req(settings(), res())
      
      explor_multi_var_dataUI2 <- function(id, settings, axes) {
        ns <- NS(id)
        fluidRow(
          column(2,
                 wellPanel(
                   selectInput("vardim", 
                               gettext("Dimension"),
                               choices = axes, selected = "1"),
                   if (settings$type == "CA") {
                     selectInput("var_tab_hide", 
                                 gettext("Hide :"),
                                 choices = explor_multi_hide_choices(),
                                 selected = "None")
                   }
                 )),
          column(10,
                 h4(if(settings$type == "CA") gettext("Active levels")                   
                    else gettext("Active variables")),
                 DT::DTOutput("vartable"),
                 if (settings$has_sup_vars || (settings$type == "CA" && settings$has_sup_levels)) {
                   list(h4(
                     if(settings$type == "CA") {
                       gettext("Supplementary elements")
                     } else {
                       gettext("Supplementary variables")
                     }),
                     DT::DTOutput("vartablesup"))
                 },
                 if (settings$has_var_eta2) {
                   list(h4(withMathJax(gettext("Variables \\(\\eta^2\\)"))),
                        DT::DTOutput("vartableeta2"))
                 },
                 if (settings$has_sup_vars && settings$has_varsup_eta2) {
                   list(h4(gettext("Supplementary variables \\(\\eta^2\\)")),
                        DT::DTOutput("vartablesupeta2"))
                 },
                 if (settings$type == "PCA" && settings$has_quali_sup_vars) {
                   list(h4(gettext("Qualitative supplementary variables")),
                        DT::DTOutput("vartablequalisup"))
                 }
          )
        )
      }
      
      explor_multi_var_dataUI2("var_data",
                               settings(), 
                               res()$axes)
    })
    
    # EIGEN MODULE ----
    
    # Eigenvalues ----
    # reactive({callModule(explor_multi_eigen,
    #            "eigen",
    #            reactive(res$eig)
    #            )
    # })
    
    
    eig <- reactive({
      req(res())
      res()$eig
      })
    
    output$explor_multi_eigenUI <- renderUI({
      req(eig())
      
      explor_multi_eigenUI2 <- function(id, eig) {
        ns <- NS(id)
        fluidRow(
          column(2,
                 wellPanel(
                   numericInput("eig_nb", 
                                        gettext("Dimensions to plot"), 
                                        min = 2, max = max(eig$dim), value = max(eig$dim), 
                                        step = 1))
                 ),
          column(5,
                 h4(gettext("Eigenvalues histogram")),
                 plotOutput("eigplot", height = "500px")),
          column(3, offset = 1,
                 h4(gettext("Eigenvalues table")),
                 DT::DTOutput("eigtab")))
      }
      
      explor_multi_eigenUI2("eigen", eig())
    })
    

    
    
    nb <- reactive({
      req(eig())
      ifelse(is.null(input$eig_nb),
             nrow(eig()),
             input$eig_nb)
      })


    output$eigplot <- renderPlot({
      req(eig(), nb())

      tmp <- eig()[1:nb(),]
      tmp$dim <- factor(tmp$dim)
      ggplot(data = tmp) +
        geom_bar(aes_string(x = "dim", y = "percent"), stat = "identity") +
        scale_x_discrete(gettext("Axis")) +
        scale_y_continuous(gettext("Percentage of inertia"))
    })

    # eigen table ----
    output$eigtab <- DT::renderDT({
      req(eig(), nb())

      tmp <- eig()[1:nb(),]
      tmp$cumpercent <- cumsum(tmp$percent)
      names(tmp) <- c(gettext("Axis"), "%", "Cum. %")
      dt <- DT::datatable(tmp, rownames = FALSE,
                          options = list(dom = 't', pageLength = nb()))
      dt %>% DT::formatRound(c("%", "Cum. %"), digits = 1)
    })
     
  }
)
