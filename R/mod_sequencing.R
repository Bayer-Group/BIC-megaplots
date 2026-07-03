# UI function for Sequencing module
sequencing_ui <- function(id) {
  ns <- shiny::NS(id) # Create a namespace function for the module

  bslib::accordion_panel(
    title = "Sequencing",
    icon = bsicons::bs_icon("bar-chart-steps"),

    # Seriation Method Input
    shinyWidgets::pickerInput(
      inputId = ns('sequencing_seriation_method'),
      label = 'Seriation method',
      choices = sort(
        c(
          'GW_average',
          'GW_complete',
          'GW_single',
          'GW_ward',
          'HC_single',
          'HC_average',
          'HC_complete',
          'HC_ward',
          'OLO_average',
          'OLO_complete',
          'OLO_single',
          'OLO_ward',
          'VAT',
          'TSP',
          'ARSA'
        )
      ),
      selected = 'GW_average',
      multiple = FALSE,
      options = list(
        `live-search` = TRUE,
        `header` = 'Select item'
      )
    ),

    # Select Events for Sequencing
    shiny::selectInput(
      inputId = ns("sequencing_events"),
      label = "Select Events for Sequencing",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),

    # Distance Measure Input
    shinyWidgets::pickerInput(
      inputId = ns("sequencing_distmeasure_name"),
      label = 'Distance Measure',
      choices = c(
        "OM",
        "OMloc",
        "OMslen",
        "OMspell",
        "OMstran",
        "CHI2",
        "EUCLID",
        "LCS",
        "LCP",
        "RLCP",
        "HAM",
        "DHD",
        "DHD",
        "VAT",
        "TSP"
      ),
      selected = 'OM',
      multiple = FALSE,
      options = list(`live-search` = TRUE, `header` = 'Select item')
    ),

    # Parameters Accordion
    bslib::accordion_panel(
      title = "Parameters",
      icon = bsicons::bs_icon("plus-circle"),

      # Substitution Cost Input
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "']== 'OM' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMloc' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMslen' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMspell' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMstran' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'DHD' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'HAM'"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_substitution_cost"),
          label = "Substitution Cost",
          choices = sort(c(
            "CONSTANT",
            "INDELS",
            "INDELSLOG",
            "TRATE",
            "ORDINAL"
          )),
          selected = 'CONSTANT',
          multiple = FALSE,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        )
      ),

      # Insertion/Deletion Cost Input
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "']== 'OM' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMslen' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMspell' ||
                     input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMstran'"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_insertion_deletion_cost"),
          label = "Insertion/Deletion Cost",
          choices = sort(c("auto", "numeric value")),
          selected = "auto",
          multiple = FALSE,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        ),
        shiny::conditionalPanel(
          condition = paste0(
            "input['",
            ns("sequencing_distmeasure_name"),
            "']==  'numeric value'"
          ),
          shiny::numericInput(
            inputId = ns("sequencing_insertion_deletion_cost_numeric"),
            label = "Insertion/Deletion Cost (double)",
            value = 1,
            min = 0
          )
        )
      ),

      # Normalization Input
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "'] !=  'HAM'"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_normalization"),
          label = "Normalization",
          choices = sort(c(
            "auto",
            "none",
            "maxlength",
            "gmean",
            "maxdist",
            "YujianBo"
          )),
          selected = 'auto',
          multiple = FALSE,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        )
      ),

      # Additional Parameter Inputs
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "']==  'OMloc' || input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMspell'"
        ),
        shiny::numericInput(
          inputId = ns("sequencing_cost_spell_length_transformation"),
          label = "Cost of spell length transformation",
          value = 0.5,
          min = 0,
          step = 0.1
        )
      ),

      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMloc'"
        ),
        shiny::numericInput(
          inputId = ns("sequencing_local_insertion_cost"),
          label = "Local Insertion Cost",
          value = 0,
          min = 0
        )
      ),

      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMslen'"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_substitution_costs_function"),
          label = "Substitution Costs Function",
          choices = sort(c("mean", "gmean")),
          selected = 'mean',
          multiple = FALSE,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        )
      ),

      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMslen' || input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMspell'"
        ),
        shiny::numericInput(
          inputId = ns("sequencing_exponential_weight_spell_length"),
          label = "Exponential weight of spell length",
          value = 0.5,
          min = 0
        )
      ),

      # Additional Parameters for OMstran
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'OMstran'"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_transition_indel_cost_method"),
          label = "Transition Indel Cost Method",
          choices = sort(c("constant", "subcost", "prob")),
          selected = 'constant',
          multiple = FALSE,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_account_transition_previous_state"),
          label = "Account for the transition from the previous state",
          choices = sort(c("TRUE", "FALSE")),
          selected = 'FALSE',
          multiple = FALSE,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_duplicate_last_column"),
          label = "Duplicate the last column",
          choices = sort(c("TRUE", "FALSE")),
          selected = 'TRUE',
          multiple = FALSE,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        ),
        shiny::numericInput(
          inputId = ns("sequencing_origin_transition_trade_off_weight"),
          label = "Origin-Transition Trade-Off Weight",
          value = 0.5,
          min = 0,
          step = 0.1,
          max = 1
        )
      ),

      # Parameters for CHI2 and EUCLID
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'CHI2' || input['",
          ns("sequencing_distmeasure_name"),
          "'] == 'EUCLID'"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_intervals_overlapping"),
          label = "Intervals overlapping",
          choices = sort(c("TRUE", "FALSE")),
          selected = 'FALSE',
          multiple = FALSE,
          width = 150,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        ),
        shiny::numericInput(
          inputId = ns("sequencing_interval_length"),
          label = "Interval Length",
          value = 1,
          min = 1,
          step = 1
        )
      ),

      # Additional parameters for CHI2
      shiny::conditionalPanel(
        condition = paste0(
          "input['",
          ns("sequencing_distmeasure_name"),
          "'] =='CHI2'"
        ),
        shinyWidgets::pickerInput(
          inputId = ns("sequencing_distribution_states_weights"),
          label = "Distribution of states as weights",
          choices = sort(c("TRUE", "FALSE")),
          selected = 'TRUE',
          multiple = FALSE,
          options = list(`live-search` = TRUE, `header` = 'Select item')
        )
      ),

      # Missing Method Input
      shinyWidgets::pickerInput(
        inputId = ns("sequencing_missing_method"),
        label = 'Missing method',
        choices = c("new state", "last observed state"),
        selected = "new state",
        multiple = FALSE,
        options = list(`live-search` = TRUE, `header` = 'Select item')
      )
    ),

    # Warning for specific distance measures
    shiny::conditionalPanel(
      condition = paste0(
        "input['",
        ns("sequencing_distmeasure_name"),
        "'] == 'HAM' || input['",
        ns("sequencing_distmeasure_name"),
        "'] == 'DHD'"
      ),
      shiny::span(
        shiny::HTML(
          gsub(
            '\n',
            '<br/>',
            stringr::str_wrap(
              'This distance measure only works for sequences of the same length!',
              width = 30
            )
          )
        ),
        style = 'color:#e6250b'
      ),
      shiny::br()
    ),

    # Apply Button
    shiny::actionButton(
      inputId = ns("sequencing_button"),
      label = "Apply"
    ),

    # Sequencing Switch
    shinyWidgets::prettySwitch(
      inputId = ns("sequencing_switch"),
      label = "On/Off Sequencing Sorting",
      value = FALSE,
      status = "primary"
    ) #,
    # # Circular_vision Switch
    # shinyWidgets::prettySwitch(
    #   inputId = ns("circular_vision"),
    #   label = "On/Off Circular Vision",
    #   value = FALSE,
    #   status = "primary"
    # )
  )
}

# Server function for Sequencing module
sequencing_server <- function(input, output, session, megaplot_filtered_data) {
  # Observe changes in filtered data and update event choices
  shiny::observeEvent(megaplot_filtered_data(), {
    shiny::req(megaplot_filtered_data())

    choices_data <- megaplot_filtered_data() |>
      dplyr::select(
        .data$megaplots_selected_event,
        .data$megaplots_selected_event_group
      ) |>
      dplyr::distinct()

    shiny::updateSelectInput(
      inputId = "sequencing_events",
      choices = split(
        choices_data$megaplots_selected_event,
        choices_data$megaplots_selected_event_group
      ),
      selected = NULL
    )
  })

  # Reactive values to store sequencing order data
  sequencing_object <- shiny::reactiveValues(val = NULL)

  # Update substitution cost and normalization choices based on distance measure selection
  shiny::observeEvent(input$sequencing_distmeasure_name, {
    if (input$sequencing_distmeasure_name == "DHD") {
      choices_sub_cost <- sort(c("INDELS", "INDELSLOG", "TRATE"))
      selected_sub_cost <- "INDELS"
    } else if (
      input$sequencing_distmeasure_name %in%
        c("OM", "OMloc", "OMslen", "OMspell", "OMstran", "HAM")
    ) {
      choices_sub_cost <- sort(c(
        "CONSTANT",
        "INDELS",
        "INDELSLOG",
        "TRATE",
        "ORDINAL"
      ))
      selected_sub_cost <- "CONSTANT"
    }

    if (
      input$sequencing_distmeasure_name %in%
        c("OM", "OMloc", "OMslen", "OMspell", "OMstran", "DHD", "HAM")
    ) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "sequencing_substitution_cost",
        choices = choices_sub_cost,
        selected = selected_sub_cost
      )
    }

    # Update normalization choices based on distance measure
    if (input$sequencing_distmeasure_name %in% c("CHI2", "EUCLID")) {
      choices_norm <- sort(c("auto", "none"))
      selected_norm <- "auto"
    } else if (
      input$sequencing_distmeasure_name %in%
        c(
          'OM',
          'OMloc',
          'OMslen',
          'OMspell',
          'OMstran',
          'DHD',
          'LCS',
          'LCP',
          'RLCP'
        )
    ) {
      choices_norm <- sort(c(
        "auto",
        "none",
        "maxlength",
        "gmean",
        "maxdist",
        "YujianBo"
      ))
      selected_norm <- "auto"
    }

    if (
      input$sequencing_distmeasure_name %in%
        c(
          "OM",
          "OMloc",
          "OMslen",
          "OMspell",
          "OMstran",
          "DHD",
          "LCS",
          "LCP",
          "RLCP",
          "CHI2",
          "EUCLID"
        )
    ) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "sequencing_normalization",
        label = "Normalization",
        choices = choices_norm,
        selected = selected_norm
      )
    }
  })

  # Handle the Apply button event
  shiny::observeEvent(input$sequencing_button, {
    shiny::req(input$sequencing_events)

    # Turn off the sequencing switch before processing
    shinyWidgets::updatePrettySwitch(
      session,
      inputId = "sequencing_switch",
      value = FALSE
    )

    selected_event_for_sequencing <- input$sequencing_events

    # Filter megaplot data for selected events
    megaplot_data <- megaplot_filtered_data() |>
      dplyr::filter(.data$event %in% selected_event_for_sequencing)

    n_matrix <- length(unique(megaplot_data$megaplots_selected_subjectid))
    m_matrix <- length(
      min(megaplot_data$megaplots_selected_event_time, na.rm = TRUE):max(
        megaplot_data$megaplots_selected_event_time_end,
        na.rm = TRUE
      )
    )
    dist_list <- list()

    index_n <- megaplot_data |>
      dplyr::filter(.data$event %in% selected_event_for_sequencing) |>
      nrow()

    index <- 1 / index_n

    # Progress bar for the seriation process
    shiny::withProgress(message = "Apply seriation", value = 0, {
      for (i in seq_along(selected_event_for_sequencing)) {
        dist_init <- matrix(NA, n_matrix, m_matrix)
        rownames(dist_init) <- unique(
          megaplot_data$megaplots_selected_subjectid
        )
        colnames(dist_init) <- paste0(
          min(megaplot_data$megaplots_selected_event_time):max(
            megaplot_data$megaplots_selected_event_time_end
          )
        )

        megaplot_data_tmp <- megaplot_data |>
          dplyr::filter(
            .data$megaplots_selected_event %in% selected_event_for_sequencing[i]
          )

        for (j in seq_len(nrow(megaplot_data_tmp))) {
          dist_init[
            paste0(megaplot_data_tmp[j, ]$megaplots_selected_subjectid),
            paste0(
              megaplot_data_tmp[
                j,
              ]$megaplots_selected_event_time:megaplot_data_tmp[
                j,
              ]$megaplots_selected_event_time_end
            )
          ] <- 1

          shiny::incProgress(amount = index)
        }

        # Parameters for distance function
        par <- list(
          distmeasure = input$sequencing_distmeasure_name,
          sm = input$sequencing_substitution_cost,
          smDHD = input$sequencing_substitution_cost,
          norm = input$sequencing_normalization,
          indel = input$sequencing_insertion_deletion_cost,
          expcost = input$sequencing_exponential_weight_spell_length,
          context = input$sequencing_local_insertion_cost,
          link = input$sequencing_substitution_costs_function,
          transindel = input$sequencing_sequencing_transition_indel_cost_method,
          otto = input$sequencing_origin_transition_trade_off_weight,
          previous = input$sequencing_account_transition_previous_state,
          add.column = input$sequencing_duplicate_last_column,
          overlap = input$sequencing_intervals_overlapping,
          step = input$sequencing_interval_length,
          weighted = input$sequencing_distribution_states_weights,
          methMissing = input$sequencing_missing_method
        )

        # Check for OMloc and empty sequences
        if (
          par$distmeasure == "OMloc" &&
            any(apply(dist_init[, -1], 1, function(x) all(is.na(x))))
        ) {
          par$methMissing <- "new state"
          dist_init[is.na(dist_init)] <- 0
        }

        seq <- suppressMessages(TraMineR::seqdef(dist_init, 2:ncol(dist_init)))
        seqargs_all <- get_parameters(seq, par)

        # Calculate pairwise distances for this variable
        dist_list[[i]] <- suppressMessages(do.call(
          TraMineR::seqdist,
          seqargs_all
        ))
      }
    })

    # Combine distance matrices
    dist <- Reduce(`+`, dist_list)
    ddist <- stats::as.dist(dist) # Convert to dist object

    # Compute the order using the seriation package
    sq <- suppressMessages(seriation::seriate(
      ddist,
      method = input$sequencing_seriation_method
    ))

    # Update the sequencing switch to indicate processing is complete
    shinyWidgets::updatePrettySwitch(
      session,
      inputId = "sequencing_switch",
      value = TRUE
    )
    sequencing_object$val <- sq # Store the result in reactive values
  })

  return(list(
    sequencing_object = shiny::reactive({
      sequencing_object$val
    }),
    sequencing_switch = shiny::reactive({
      input$sequencing_switch
    }),
    #circular_vision = shiny::reactive({input$circular_vision})
    circular_vision = shiny::reactive({
      FALSE
    }) #don't display circular version
  ))
}
