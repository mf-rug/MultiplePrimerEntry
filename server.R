server <- function(input, output) {
  
  observeEvent(input$format, {
    if (input$tabs == "Merck") {
      if (input$format != 'Dry') {
        enable('conc')
      } else {
        disable('conc')
      }
    }
  })
  
  d_i_t <- eventReactive(input$seqs, {
    out <- 'Delimiter between name and sequence in the input'
    if (input$seqs != '') {
      show('hidehr')
      possible_splits <- str_split(str_replace_all(input$seqs, '[A-Za-z0-9-_]+', 'a'), 'a')[[1]]
      possible_splits <- unique(possible_splits[possible_splits != ""])
      if (!identical(possible_splits, character(0))) {
        stri <- str_split(str_remove(input$seqs, '(?:\\n)*$'), '\\n')[[1]]
        stri.df <- sapply(1:length(possible_splits), function(y) sapply(stri, function(x) str_count(x, str_replace_all(possible_splits[y], '(.)', '\\\\\\1')))) %>% as.data.frame()
        guess <- possible_splits[which(apply(stri.df, 2, sum) == length(stri))]
        if (length(guess) == 1) {
          updateTextInput(session = getDefaultReactiveDomain(), inputId = 'delim', value = guess)
          out <- 'Delimiter between name and sequence in the input (auto detected)'
        }
      }
    } else {
      hide('hidehr')
    }
    out
  })
  
  output$delim_in_text <- renderUI({
    HTML(paste0('<strong>', d_i_t(), '</strong>'))
  })
  
  output$clip <- renderUI({
    out <- ''
    if (input$seqs != '') {
      pat <- paste0(input$delim, '|\\n')
      process <- str_split(str_remove(gsub('  *', ' ', input$seqs), '(?:\\n)+$'), pattern = pat)[[1]]
      if (str_count(str_remove(input$seqs, '(?:\\n)+$'), '\\n') + 1 != length(process) /2) { 
        out <- ''
      } else {
        process <- matrix(process, ncol = str_count(str_remove(input$seqs, '(?:\\n)+$'), '\n') +1) %>% t() %>% as.data.frame()
        sep <- sample(setdiff(str_split('_"$&+,:;=?@#|\'<>.*()%!/{}-', '')[[1]], unique(str_split(input$seqs, '')[[1]])), 1)
        if (input$tabs == "Generic") {
          out <- data.frame("name" = process[,1],
                            'seqs' = process[,2]) %>% format_delim(delim =sep, col_names = FALSE)
        } else if (input$tabs == "Merck") {
          out <- data.frame("name" = process[,1],
                            '5p' = '',
                            'seqs' = process[,2],
                            '3p' = '',
                            'scale' = input$scale,
                            'pure' = input$purification,
                            'format' = input$format,
                            'conc' = ifelse(input$format == "Dry", "None", input$conc)) %>% format_delim(delim =sep, col_names = FALSE)
        } else {
          out <- data.frame("name" = process[,1],
                            'seqs' = process[,2],
                            'scale' = input$scaleIDT,
                            'pure' = input$purificationIDT) %>% format_delim(delim =sep, col_names = FALSE)
        }
        #looks weird, but need to take care of backslashes and replace \t with actual tabs.
        print(paste('sep', sep, 'input$delim_out', input$delim_out))
        out <- str_replace_all(out, paste0('[',sep,']'), str_replace_all(str_replace(input$delim_out, '[\\\\]', '\\\\\\\\'), '\\\\t', ' \t'))
        cat(out)
      }
    }
    rclipButton("clipbtn", HTML('<small><font color="grey">Copy to clipboard</font></small>'), out, modal = FALSE, icon("copy"), )
  })
  
  output$out <- renderDT({
    if (input$seqs != '') {
      pat <- paste0(input$delim, '|\\n')
      process <- str_split(str_remove(gsub('  *', ' ', input$seqs), '(?:\\n)+$'), pattern = pat)[[1]]
      if (str_count(str_remove(input$seqs, '(?:\\n)+$'), '\\n') + 1 != length(process) /2) { 
        hide('hidehr')
        out <- "<i>Problem parsing input</i>"
        datatable(data.frame('Problem' = out), options = list(dom = 't',
                                                              headerCallback = JS(
                                                                "function(thead, data, start, end, display){",
                                                                "  $(thead).remove();",
                                                                "}")),
                  rownames = FALSE,
                  escape = FALSE,
                  selection = 'none') %>% formatStyle(1, backgroundColor = 'white', border = 'none', color = 'grey')
      } else {
        show('hidehr')
        process <- matrix(process, ncol = str_count(str_remove(input$seqs, '(?:\\n)+$'), '\n') +1) %>% t() %>% as.data.frame()
        if (input$tabs == "Generic") {
          out <- data.frame("name" = process[,1],
                            'seqs' = process[,2])
        } else if (input$tabs == "Merck") {
          out <- data.frame("name" = process[,1],
                            '5p' = '',
                            'seqs' = process[,2],
                            '3p' = '',
                            'scale' = input$scale,
                            'pure' = input$purification,
                            'format' = input$format,
                            'conc' = ifelse(input$format == "Dry", "None", input$conc))
        } else {
          out <- data.frame("name" = process[,1],
                            'seqs' = process[,2],
                            'scale' = input$scaleIDT,
                            'pure' = input$purificationIDT)
        }
        datatable(out, options = list(dom = 't',
                                      pageLength = 50000,
                                      headerCallback = JS(
                                        "function(thead, data, start, end, display){",
                                        "  $(thead).remove();",
                                        "}")),
                  rownames = FALSE,
                  selection = 'none') %>% formatStyle(colnames(out), backgroundColor = 'white', border = 'none')
      }
    }
  })
}