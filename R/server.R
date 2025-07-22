require(shiny)
require(ggplot2)
require(colorSpec)

server <- shiny::shinyServer(function(input, output, session) {

    onBookmark(function(state) {
      if (grepl("^CUSTOM:", new_spectra_selection())) {
        state$values$custom_spectra <- input$upload_spectra$name
      }
    })

  onRestored(function(state) {
    if ("custom_spectra" %in% names(state$values)) {
      showModal(
        modalDialog(
          h2(paste("Please upload file:", state$values$custom_spectra)),
          br(),
          fileInput(
            "upload_spectra",
            NULL,
            buttonLabel = "Upload spectra",
            multiple = FALSE
          ),
          hr(),
          h4("Why do you have to do that?"),
          p("You are starting the app via a bookmarked link that refers to an emission-spectra-file on the local file system. Due to security reasons, the browser cannot directly access the local file system by itself."),
          p('I you want to continue with a set of the integrated emission spectra, press "Dismiss".')
      ))
    }
    lapply(1:5, function(x) updateSliderInput(session, paste0("luminance_primary_", x),
                                              min = input[[paste0("lower", x)]],
                                              max = input[[paste0("upper", x)]],
                                              step = input[[paste0("step", x)]]))
    updateTabsetPanel(inputId = "switcher", selected = tab_titles[1])
    selected_tab(1)
  })

    init_spectra <- get_normalized_spectrum_matrix(four_primary_LEDs)

    new_spectra_choices <- reactiveVal(
      list.files(system.file("primary_spectra",
                                       package = "silentSubstitutor"))
    )

    new_spectra_selection <- reactiveVal(head(new_spectra_choices, 1))

    new_custom_spectra <- reactiveVal()

    tab_titles <- c("1. Set primaries",
                    "3. Set desired contrasts",
                    "4. Estimate errors"
                    )

    selected_tab <- reactiveVal(1)

    observeEvent(input$a_matrix,
                 updateTabsetPanel(inputId = "switcher", selected = "2. A_matrices"))

    observeEvent(input$back_from_a_matrix, {
      updateTabsetPanel(inputId = "switcher", selected = tab_titles[1])
      selected_tab(1)
    })

    observeEvent(input$setupStimulator, {
      updateTabsetPanel(inputId = "switcher", selected = tab_titles[1])
      selected_tab(1)
    })

    observeEvent(input$exploreErrors, {
      updateTabsetPanel(inputId = "switcher", selected = tab_titles[3])
      selected_tab(3)
    })

    observeEvent(input$createStimuli, {
      updateTabsetPanel(inputId = "switcher", selected = tab_titles[2])
      selected_tab(2)
    })

    observeEvent(input$advanced_settings, {
      updateTabsetPanel(inputId = "switcher", selected = "advanced_settings")
    })

    observeEvent(input$nxt, {
      if (isolate(selected_tab()) == length(tab_titles))
        selected_tab(1)
      else
        selected_tab(isolate(selected_tab()) + 1)
      nxt <- selected_tab()
      updateTabsetPanel(inputId = "switcher", selected = tab_titles[nxt])
    })

    observeEvent(input$lower1, updateSliderInput(inputId = "luminance_primary_1", min = input$lower1))
    observeEvent(input$lower2, updateSliderInput(inputId = "luminance_primary_2", min = input$lower2))
    observeEvent(input$lower3, updateSliderInput(inputId = "luminance_primary_3", min = input$lower3))
    observeEvent(input$lower4, updateSliderInput(inputId = "luminance_primary_4", min = input$lower4))
    observeEvent(input$lower5, updateSliderInput(inputId = "luminance_primary_5", min = input$lower5))

    observeEvent(input$upper1, updateSliderInput(inputId = "luminance_primary_1", max = input$upper1))
    observeEvent(input$upper2, updateSliderInput(inputId = "luminance_primary_2", max = input$upper2))
    observeEvent(input$upper3, updateSliderInput(inputId = "luminance_primary_3", max = input$upper3))
    observeEvent(input$upper4, updateSliderInput(inputId = "luminance_primary_4", max = input$upper4))
    observeEvent(input$upper5, updateSliderInput(inputId = "luminance_primary_5", max = input$upper5))

    observeEvent(input$step1, updateSliderInput(inputId = "luminance_primary_1", step = input$step1))
    observeEvent(input$step2, updateSliderInput(inputId = "luminance_primary_2", step = input$step2))
    observeEvent(input$step3, updateSliderInput(inputId = "luminance_primary_3", step = input$step3))
    observeEvent(input$step4, updateSliderInput(inputId = "luminance_primary_4", step = input$step4))
    observeEvent(input$step5, updateSliderInput(inputId = "luminance_primary_5", step = input$step5))

    output$select_primary <- renderUI(
      choices <-
        selectInput("new_spectra",
                    "Pre-defined spectra",
                    choices = new_spectra_choices(),
                    selected = new_spectra_selection())
    )

    output$download_json <- downloadHandler(
      filename = function()
        paste0("SilentSubstiTutor-", Sys.Date(), ".json"),
      content = function(file) {
        jsonlite::write_json(led_settings(), file)
      }
    )

    output$download_excel <- downloadHandler(
      filename = function()
        paste0("SilentSubstiTutor-", Sys.Date(), ".xlsx"),
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "StimulusConditions")
        openxlsx::writeData(wb, "StimulusConditions", led_settings())
        openxlsx::addWorksheet(wb, "Variables")
        openxlsx::writeData(wb, "Variables",
                            data.frame(Variable = c("Primary", "PeakWavelength", "Luminance", "Power", "MichelsonContrast", "MaxLumiance"),
                                       Explanation = c("Name of the primary",
                                                       "Peak wavelengths of the primary [nm]",
                                                       "Time-averaged Luminance [cd/m^2]",
                                                       "Time-averaged Power [W/m^2]",
                                                       "Michelson contrast [%]",
                                                       "Peak Luminance [cd/m^2]")))
        openxlsx::saveWorkbook(wb, file, overwrite = FALSE)
      }
    )

    globalVars <- reactiveValues(
      led_spectra = init_spectra,
      n_primaries = ncol(init_spectra),
      primary_names = colnames(init_spectra),
      fundamentals = silentSubstitutor::photoreceptor_spectral_sensitivities[, -1]
    )

    ### Reactive calculations ###
    modified_fundamentals <- reactive({
      silentSubstitutor::photoreceptor_spectral_sensitivities |>
        shift_cone_fundamentals("lcone", input$l_cone_shift) |>
        shift_cone_fundamentals("mcone", input$m_cone_shift) |>
        lens_age(input$lens_age) |>
        macular_pigment(input$mac_pigment - 0.3, silentSubstitutor::mpod_spectrum)
    })

    get_luminances <- reactive({
      # Requirements
      req(
        input$luminance_primary_1,
        input$luminance_primary_2,
        input$luminance_primary_3
      )
      if (globalVars$n_primaries > 3)
        req(input$luminance_primary_4)
      if (globalVars$n_primaries > 4)
        req(input$luminance_primary_5)

      # Calculations
      luminances <- c(
        input$luminance_primary_1,
        input$luminance_primary_2,
        input$luminance_primary_3,
        input$luminance_primary_4,
        input$luminance_primary_5
      )
      names(luminances)[seq(globalVars$n_primaries)] <- globalVars$primary_names

      # Return Values
      luminances[seq(globalVars$n_primaries)]
    })

    led_spectra_long_table <- reactive({
      calculate_power_spectra(get_luminances(), globalVars$led_spectra) |>
        data.frame(wavelength_nm = wavelengths) |>
        data.table::data.table() |>
        data.table::melt(id = "wavelength_nm", variable.name = "led", value.name = "power")
    })

    # Creates a table from the photoreceptor contrast sliders
    photoreceptor_contrasts_percent <- reactive({
      # Requirements
      req(input$lcone, input$mcone, input$scone)
      if (globalVars$n_primaries > 3)
        req(input$rod)
      if (globalVars$n_primaries > 4)
        req(input$melanopsin)

      # Calculations
      photoreceptor_contrasts <-
        c(
          lcone = input$lcone,
          mcone = input$mcone,
          scone = input$scone,
          rod = input$rod,
          melanopsin = input$melanopsin
        )

      # Return Values
      contrasts <- photoreceptor_contrasts[globalVars$n_primaries:1]

      for (i in 1:globalVars$n_primaries) {
          shinyFeedback::feedbackWarning(names(contrasts[i]),
                                         abs(contrasts[i]) > 100,
                                         "Contrast must be between -100% and 100%!")
        req(abs(contrasts[i]) <= 100)
      }

      contrasts
    })

    led_peak_wavelengths <- reactive({
      peak_wavelengths <-
        globalVars$led_spectra |>
        apply(2, function(x)
          wavelengths[which.max(x)]) |>
        round(0) |>
        as.integer()
    })

    led_settings <-
      reactive({
        l_out <- data.frame(
          Primary = names(led_contrasts_percent()),
          PeakWavelengths = led_peak_wavelengths(),
          Luminance = get_luminances(),
          Power = convert_luminance_to_power(get_luminances(), globalVars$led_spectra),
          MichelsonContrast = led_contrasts_percent()
        )
        l_out$MinLuminance <- l_out$Luminance * (1 - abs(l_out$MichelsonContrast / 100))
        l_out$MaxLuminance <- l_out$Luminance * (1 + abs(l_out$MichelsonContrast / 100))

        return(l_out)
      })

    led_contrasts_percent <- reactive({
      100 * find_LED_contrasts(photoreceptor_contrasts_percent() / 100,
                               A_matrix())
    })

    A_matrix <- reactive({
      create_A_matrix(
        calculate_power_spectra(get_luminances(), globalVars$led_spectra),
        as.matrix(globalVars$fundamentals)
      )
    })

    full_receptor_A_matrix <- reactive({
      create_A_matrix(
        calculate_power_spectra(get_luminances(), globalVars$led_spectra),
        as.matrix(modified_fundamentals())
      )
    })

    color_palette <- reactive({
      calculate_primary_RGBs(globalVars$n_primaries, globalVars$led_spectra, wavelengths)
    })

    spectrum_300cd_per_m2 <- reactive({

      normalize_to_300 <- function(x) {
        x * 300 / sum(x)
      }

      calculate_power_spectra(get_luminances(),
                              globalVars$led_spectra) |>
        apply(1, sum) |>
        normalize_to_300() |>
        colorSpec::colorSpec(wavelengths, specnames = "S1")
    })

    test_field_RGB <- reactive({
      rgb_specs <- spectrum_300cd_per_m2() |>
        colorSpec::product(colorSpec::BT.709.RGB, wavelength = "auto") |>
        colorSpec::DisplayRGBfromLinearRGB()

      rgb(rgb_specs[1], rgb_specs[2], rgb_specs[3])
    })

    test_field_xyz_coords <- reactive({
      XYZ <- spectrum_300cd_per_m2() |>
        colorSpec::product(colorSpec::xyz1964.1nm, wavelength = "auto")

      list(x = XYZ[1] / sum(XYZ), y = XYZ[2] / sum(XYZ))
    })

    led_limits <- reactive({
      data.frame(LED = factor(globalVars$primary_names),
                 minLimits = sapply(1:globalVars$n_primaries, function(n) input[[paste0("lower", n)]]),
                 maxLimits = sapply(1:globalVars$n_primaries, function(n) input[[paste0("upper", n)]]))
    })

    led_luminances_over_time <- reactive({

      led_out <- data.frame(led = names(led_contrasts_percent()),
                            contrast = led_contrasts_percent())

      x <- seq(0, 2 * pi, .01)

      ftNRG <- data.frame()

      for (i in x) {
        ftNRG <- rbind(ftNRG,
                       data.frame(
                         LED = led_out$led,
                         x = i,
                         lum = get_luminances() * (1 + led_out$contrast * sin(i) / 100)
                       ))
      }

      ftNRG
    })

    ##### Tab 1 #####

    ### Upload spectra
    load_spectra_from_path <- function(path) {
      spectra <- load_led_spectra(path, wavelengths)

      if (inherits(spectra, "try-error")) {
        showModal(modalDialog(p(spectra),
                              title = "Input file not valid"))
      } else {
        globalVars$led_spectra <- spectra
        globalVars$primary_names <- colnames(globalVars$led_spectra)
        globalVars$n_primaries <- ncol(globalVars$led_spectra)
        globalVars$fundamentals <- silentSubstitutor::photoreceptor_spectral_sensitivities[, seq(5 - globalVars$n_primaries + 1, 5)]
      }
    }

    observeEvent(input$upload_spectra, {
      load_spectra_from_path(input$upload_spectra$datapath)
      new_spectra_choices(
        c(new_spectra_choices()[!grepl("^CUSTOM:", new_spectra_choices())],
          paste("CUSTOM:", basename(input$upload_spectra$name)))
      )
      new_spectra_selection(tail(new_spectra_choices(), 1))
      # selectInput is automatically updated when going back to the main tab via renderUI
    })

    observeEvent(input$new_spectra, {
      grepl("^CUSTOM:", input$new_spectra)
      if (!grepl("^CUSTOM:", input$new_spectra)) {
        load_spectra_from_path(system.file(paste0(
          "primary_spectra/", input$new_spectra
        ), package = "silentSubstitutor"))
      } else {
        load_spectra_from_path(input$upload_spectra$datapath)
      }
    })

    output$luminance_primary_1 <- renderUI({
      req(globalVars$n_primaries > 3)
      luminance_slider(1, globalVars$primary_names[1],
                       value = input$upper1 / 2, min = input$lower1, max = input$upper1, step = input$step1)
    })

    output$luminance_primary_2 <- renderUI({
      luminance_slider(2, globalVars$primary_names[2],
                       value = input$upper2 / 2, min = input$lower2, max = input$upper2, step = input$step2)
    })

    output$luminance_primary_3 <- renderUI({
      luminance_slider(3, globalVars$primary_names[3],
                       value = input$upper3 / 2, min = input$lower3, max = input$upper3, step = input$step3)
    })

    output$luminance_primary_4 <- renderUI({
      req(globalVars$n_primaries > 3)
      luminance_slider(4, globalVars$primary_names[4],
                       value = input$upper4 / 2, min = input$lower4, max = input$upper4, step = input$step4)
    })

    output$luminance_primary_5 <- renderUI({
      req(globalVars$n_primaries > 4)
      luminance_slider(5, globalVars$primary_names[5],
                       input$upper5 / 2, min = input$lower5, max = input$upper5, step = input$step5)
    })

    output$color_bx <- renderUI({
      div(
        "",
        style = htmltools::css(
          isplay = "inline-block",
          width = "100px",
          height = "60px",
          border = "2px solid black",
          padding = "10px",
          background_color = test_field_RGB()
        )
      )
    })

    output$cie_x <- renderText({
      sprintf("x = %s", round(test_field_xyz_coords()$x, 3))
    })

    output$cie_y <- renderText({
      sprintf("y = %s", round(test_field_xyz_coords()$y, 3))
    })


    ### Plot LED spectra
    output$led_spectra <- renderPlot({
      req(globalVars$led_spectra)

      led_spectra_long_table() |>
        ggplot2::ggplot(ggplot2::aes(
          x = wavelength_nm,
          y = power,
          group = led,
          color = led
        )) +
        ggplot2::geom_line() +
        ggplot2::theme_bw(20) +
        ggplot2::scale_color_manual(values = unlist(color_palette())) +
        ggplot2::scale_x_continuous("Wavelength [nm]") +
        ggplot2::scale_y_continuous("Power [W/m^2]")
    })

    ##### Tab 2 #####

    ### Print A-Matrix
    output$a_matrix <- renderTable({
      A_matrix()
    }, rownames = TRUE, digits = 3)

    output$a_matrix_b <- renderTable({
      full_receptor_A_matrix()
    }, rownames = TRUE, digits = 3)

    ##### Tab 3 #####

    output$rod_slider <- renderUI({
      req(globalVars$n_primaries > 3)
      contrast_slider("rod", "Rod contrast:")
    })

    output$melanopsin_slider <- renderUI({
      req(globalVars$n_primaries > 4)
      contrast_slider("melanopsin", "Melanopsin contrast:")
    })

    output$led_contrasts <- renderTable({
      tab_out <- led_settings()
      for (i in 1:globalVars$n_primaries) {
        if (as.numeric(tab_out$MaxLuminance[i]) > input[[paste0("upper", i)]]) {
          tab_out$MaxLuminance[i] <- paste0("<div style='color: white; background-color: red;'>",
                                            round(as.numeric(tab_out$MaxLuminance[i]), 3),
                                            "</div>")
        } else {
          tab_out$MaxLuminance[i] <- round(as.numeric(tab_out$MaxLuminance[i]), 3)
        }
        if ((as.numeric(tab_out$MinLuminance[i]) - input[[paste0("lower", i)]]) < -.5) {
          tab_out$MinLuminance[i] <- paste0("<div style='color: white; background-color: red;'>",
                                            round(as.numeric(tab_out$MinLuminance[i]), 3),
                                            "</div>")
        } else {
          tab_out$MinLuminance[i] <- round(as.numeric(tab_out$MinLuminance[i]), 3)
        }
        if (abs(as.numeric(tab_out$MichelsonContrast[i])) > 100.0001) {
          tab_out$MichelsonContrast[i] <- paste0("<div style='color: white; background-color: red;'>",
                                            round(as.numeric(tab_out$MichelsonContrast[i]), 3),
                                            "</div>")
        } else {
          tab_out$MichelsonContrast[i] <- round(as.numeric(tab_out$MichelsonContrast[i]), 3)
        }
      }
      tab_out
    },
    striped = TRUE,
    align = "lrrrrrr",
    caption = "Primary settings for achieving silent substitution", digits = 3,
    sanitize.text.function = function(x) x
    )

    ### Plot LED outputs over time
    output$led_luminances <- renderPlot({

      ggplot2::ggplot(led_luminances_over_time(), ggplot2::aes(x = x, y = lum, fill = LED)) +
        ggplot2::geom_area() +
        ggplot2::geom_hline(data = led_limits(), ggplot2::aes(yintercept = minLimits), size = 1.5) +
        ggplot2::geom_hline(data = led_limits(), ggplot2::aes(yintercept = maxLimits), size = 1.5) +
        ggplot2::facet_wrap(~ factor(LED, levels = globalVars$primary_names, ordered = TRUE),
                   ncol = globalVars$n_primaries) +
        ggplot2::scale_y_continuous("LED Luminance [cd/m^2]") +
        ggplot2::scale_x_continuous("Time", labels = NULL) +
        ggplot2::scale_fill_manual(values = color_palette()) +
        ggplot2::theme(text = ggplot2::element_text(size = 20)) +
        ggplot2::guides(fill = "none") +
        ggplot2::theme_bw(20)
    })

    observeEvent(input$maximize, {
      lapply(c("lcone", "mcone", "scone", "rod", "melanpsin"), function(x)
      shinyFeedback::feedbackWarning(x, all(led_contrasts_percent() == 0),
                              "For maximizig, at least one contrast must be larger than zero!"))
      req(any(led_contrasts_percent() > 0))

      scale_factor <- 100 / max(abs(led_contrasts_percent()))

      updateNumericInput(session, "lcone", value = input$lcone * scale_factor)
      updateNumericInput(session, "mcone", value = input$mcone * scale_factor)
      updateNumericInput(session, "rod", value = input$rod * scale_factor)
      updateNumericInput(session, "scone", value = input$scone * scale_factor)
      updateNumericInput(session, "melanopsin", value = input$melanopsin * scale_factor)
    })

    ##### Tab 4 #####

    output$photoreceptor_contrasts <- renderTable({
      led_settings()$MichelsonContrast %*% full_receptor_A_matrix()
    }, digits = 3)

    ### Plot underlying cone fundamentals
    output$cone_fundamentals <- renderPlot({

      modified_fundamentals() |>
        cbind(Wavelength = wavelengths) |>
        data.table::data.table() |>
        data.table::melt(id = "Wavelength",
             variable.name = "photoreceptor",
             value.name = "sensitivity") |>

        ggplot2::ggplot(ggplot2::aes(x = Wavelength, y = sensitivity)) +
        ggplot2::geom_line(ggplot2::aes(color = photoreceptor,
                      linetype = photoreceptor)) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous("Wavelength [nm]") +
        ggplot2::scale_y_continuous("Relative Sensitivity") +
        ggplot2::scale_color_manual(
          "",
          values = c(
            lcone = "red",
            mcone = "green",
            scone = "blue",
            rod = "black",
            melanopsin = "cyan"
          )
        ) +
        ggplot2::scale_linetype("")
    })

    ### Plot photoreceptor excitation over time
    output$photoreceptors <- renderPlot({
      req(photoreceptor_contrasts_percent())

      lumCurve <- function(iLED, iPhotoreceptor) {
        x <- seq(0, 2 * pi, .01)

        data.frame(
          LED = iLED,
          photoreceptor = iPhotoreceptor,
          x = x,
          y = full_receptor_A_matrix()[iLED, iPhotoreceptor] *
            (1 + led_settings()$MichelsonContrast[led_settings()$Primary == iLED] * sin(x) / 100)
        )
      }

      fTLum <- data.frame()
      for (receptor in colnames(full_receptor_A_matrix())) {
        for (primary in led_settings()$Primary)
          fTLum <- rbind(fTLum, lumCurve(primary, receptor))
      }

      fTLum$LED <- factor(fTLum$LED,
                          levels = led_settings()$Primary,
                          ordered = TRUE)
      fTLum$photorecepor <- factor(
        fTLum$photoreceptor,
        levels = c("lcone", "mcone", "rod", "melanopsin", "scone"),
        ordered = TRUE
      )

      ggplot2::ggplot(fTLum, ggplot2::aes(x = x, y = y, fill = LED)) +
        ggplot2::geom_area(alpha = .6) +
        ggplot2::facet_wrap( ~ photoreceptor, nrow = 1) +
        ggplot2::scale_y_continuous("Photoreceptor Excitation", labels = NULL) +
        ggplot2::scale_x_continuous("Time", labels = NULL) +
        ggplot2::scale_fill_manual(values = color_palette()) +
        ggplot2::theme(text = ggplot2::element_text(size = 20)) +
        ggplot2::guides(fill = "none") +
        ggplot2::theme_bw()

    })

  })
