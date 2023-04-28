library(shiny)
library(tidyverse)

our_data <- readxl::read_excel("Master_1.xlsx",
                               sheet = "Lens_details")

oem_data <- readxl::read_excel("Master_1.xlsx",
                               sheet = "Laser_types") %>% 
  drop_na(`Eyewear Requirement`, `Compatible Lens 1`)

od_data <- readxl::read_excel("OD_Measurements.xlsx")%>%
  pivot_longer(!Wavelength, names_to = "Lens", values_to = "Level of Protection (OD rating)")%>% 
  mutate(`Optical Density` = if_else(`Level of Protection (OD rating)` > 10, 10, `Level of Protection (OD rating)`))%>% 
  mutate("%T" = floor(100*10^(-`Optical Density`)))%>% 
  mutate("Color" = if_else(Wavelength < 431, "violet",
                           if_else(Wavelength < 501, "blue",
                                   if_else(Wavelength < 521, "cyan",
                                           if_else(Wavelength <566, "green",
                                                   if_else(Wavelength < 581, "yellow",
                                                           if_else(Wavelength < 626, "orange",
                                                                   "red"))))))) %>%
  mutate("Color" = as.factor(Color))

wl_data <- readxl::read_excel("Master_1.xlsx",
                              sheet = "Applications")

shinyServer(function(input, output, session) {
  output$logo <- renderImage(list(src = "www/logo_wide.PNG", height = "50px", width = "180px"))
  
  observeEvent(input$mfg,{
    freezeReactiveValue(input, "mod")
    # filter oem data to select mfg
    mfg_filtered_oem_data <- oem_data %>% 
      filter(`Mfg` == input$mfg)
    # update select input - laser model
    updateSelectInput(inputId = "mod",
                      choices = sort(unique(mfg_filtered_oem_data$`Mod`)),
                      selected = NULL)
  })
  selected_data <- eventReactive(input$mod,{
    req(input$mfg)
    oem_data %>% 
      filter(`Mfg` == input$mfg,
             `Mod` == input$mod) 
  })
  
  laser_info <- eventReactive(input$mod,{
    tibble("Device" = paste0(input$mfg, " ", input$mod),
          "Specifications (nm)" = head(selected_data()$`Eyewear Requirement`,1))
  })
  
  output$laserInfo <- renderTable(bordered = T,
                                 align = "c",
                                 striped=T,
                                 spacing='s',
                                 hover = F,
                                 {laser_info()})
  output$devName <- renderText({
    paste0("Discover lenses options compatible with the ",
           input$mfg, " ", input$mod
           )
  })
  specs_data <- eventReactive(input$mod,{
    req(input$mfg)
    req(input$mod)
      tibble(our_data %>% 
                      filter(`Lens` == head(selected_data()$`Compatible Lens 1`,1) | 
                               `Lens` == head(selected_data()$`Compatible Lens 2`,1) |
                               `Lens` == head(selected_data()$`Special Case`,1)) %>%
               mutate(VLT = scales::percent(as.numeric(VLT)),
                      "Part number" = Lens)) %>% 
                      select("Part number", "OD", "CE", "VLT", "Summary")
    
  })
  
  lens_location1 <- eventReactive(input$mod,{
    our_data %>% 
    filter(`Lens` == head(selected_data()$`Compatible Lens 1`,1)) %>% 
    select(Image, Lens, Website)
  })
  lens_location2 <- eventReactive(input$mod,{
    our_data %>% 
      filter(`Lens` == head(selected_data()$`Compatible Lens 2`,1)) %>% 
      select(Image, Lens, Website)
  })
  lens_location3 <- eventReactive(input$mod,{
    our_data %>% 
      filter(`Lens` == head(selected_data()$`Special Case`,1)) %>% 
      select(Image, Lens, Website)
  })
  output$image_Lens1 <- renderText({
    req(input$mfg)
    req(input$mod)
    if(length(lens_location1()$Image) > 0){c('<img src="',
      lens_location1()$Image[[1]],
      '", height = 250px>')
      }
  })
  output$link_Lens1 <- renderText({
    req(input$mfg)
    req(input$mod)
    c('<a href="',
    lens_location1()$Website[[1]],
      '">',lens_location1()$Lens[[1]],' - click to browse frame options</a>')
    
  })
  output$image_Lens2 <- renderText({
    req(input$mfg)
    req(input$mod)
    if(length(lens_location2()$Image) > 0){c('<img src="',
      lens_location2()$Image[[1]],
      '", height = 250px>')
    }
  })
  output$link_Lens2 <- renderText({
    req(input$mfg)
    req(input$mod)
    if(length(lens_location2()$Website) > 0){c('<a href="',
      lens_location2()$Website[[1]],
      '">',lens_location2()$Lens[[1]],' - click to browse frame options</a>')
      }
  })
  output$image_Lens3 <- renderText({
    req(input$mfg)
    req(input$mod)
    if(length(lens_location3()$Image) > 0){c('<img src="',
      lens_location3()$Image[[1]],
      '", height = 250px>')}
  })
  output$link_Lens3 <- renderText({
    req(input$mfg)
    req(input$mod)
    if(length(lens_location3()$Website) > 0){c('<a href="',
      lens_location3()$Website[[1]],
      '"> Browse ',lens_location3()$Lens[[1]],' IPL eyewear</a>')}
  })
  od_data_1 <- eventReactive(input$mod,{
    req(input$mfg)
    req(input$mod)
    od_data %>% 
      filter(Lens == head(selected_data()$`Compatible Lens 1`,1),
             Wavelength >= selected_data()$MinimumWL & Wavelength <= selected_data()$MaximumWL | Wavelength > 379 & Wavelength < 741)%>% 
      drop_na()
  })
  od_data_2 <- eventReactive(input$mod,{
    req(input$mfg)
    req(input$mod)
    od_data %>% 
      filter(Lens == selected_data()$`Compatible Lens 2`,
             Wavelength >= selected_data()$MinimumWL & Wavelength <= selected_data()$MaximumWL | Wavelength > 379 & Wavelength < 741)%>% 
      drop_na() 
  })
  output$specs_table <- renderTable(bordered = T,
                                      align = "rccc",
                                      striped=T,
                                    hover = F,
                                    width = "100%",
                                    colnames = F, na = "-",
                                      {
                                        tibble(" " = c("Part Number", "OD", "CE", "VLT", "Summary"),
                                        "Laser option 1" = t(specs_data()[1,]),
                                        "Laser option 2" = t(specs_data()[2,]),
                                        "IPL" = t(specs_data()[3,])
                                        )  
                                    
                                    })
  output$od_plot_1 <- renderPlot({
    req(input$mfg)
    req(input$mod)
    ggplot(od_data_1(), aes(Wavelength, `Optical Density`, fill = Lens))+
      geom_col(color = "black")+
      scale_fill_manual(values = "black")+
      scale_y_continuous(limits = c(0, 10),
                         breaks = seq(0, 10, by = 1))+
      scale_x_continuous(limits = c(unique(selected_data()$MinimumWL), unique(selected_data()$MaximumWL)))+
      theme(legend.position = "top",
            legend.text = element_text(size = "5em"),
            panel.background = element_rect(colour = "#f0ffff"),
            axis.text = element_text(size = "2.5em", angle = 0),
            axis.title = element_text(size = "3em"),
            plot.title = element_text(size = 15, face = "bold")
      )+
      labs(
           x = "Wavelength (nm)"
      )
    })
  output$vlt_plot_1 <- renderPlot({
    req(input$mfg)
    req(input$mod)
    ggplot(od_data_1(), aes(Wavelength, `%T`, color = Color))+
      geom_smooth()+
      geom_point()+
      scale_color_identity()+
      scale_y_continuous(limits = c(0, 100),
                         breaks = seq(0, 100, by = 25))+
      scale_x_continuous(limits = c(400, 700),
                         breaks = seq(400, 700, by = 100))+
      theme(legend.position = "top",
            legend.text = element_text(size = "5em"),
            panel.background = element_rect(colour = "#f0ffff"),
            axis.text = element_text(size = "2.5em", angle = 0),
            axis.title = element_text(size = "3em"),
            plot.title = element_text(size = 15, face = "bold")
      )+
      labs(
           y = "% Transmission",
           x = "Wavelength (nm)"
      )
  })
  output$od_plot_2 <- renderPlot({
    req(input$mfg)
    req(input$mod)
    if(length(od_data_2()$Wavelength)>0){
      ggplot(od_data_2(), aes(Wavelength, `Optical Density`, fill = Lens))+
      geom_col(color = "black")+
      scale_fill_manual(values = "black")+
      scale_y_continuous(limits = c(0, 10),
                         breaks = seq(0, 10, by = 1))+
      scale_x_continuous(limits = c(unique(selected_data()$MinimumWL), 
                                    unique(selected_data()$MaximumWL)))+
      theme(legend.position = "top",
            legend.text = element_text(size = "5em"),
            panel.background = element_rect(colour = "#E6E6F1"),
            axis.text = element_text(size = "2.5em", angle = 0),
            axis.title = element_text(size = "3em"),
            plot.title = element_text(size = 15, face = "bold")
      )+
      labs(
           x = "Wavelength (nm)"
      )
      }
  })
  output$vlt_plot_2 <- renderPlot({
    req(input$mfg)
    req(input$mod)
    if(length(od_data_2()$Wavelength)>0){
    ggplot(od_data_2(), aes(Wavelength, `%T`, color = Color))+
      geom_smooth()+
      geom_point()+
      scale_color_identity()+
      scale_y_continuous(limits = c(0, 100),
                         breaks = seq(0, 100, by = 25))+
      scale_x_continuous(limits = c(400, 700),
                         breaks = seq(400, 700, by = 100))+
      theme(legend.position = "top",
            legend.text = element_text(size = "5em"),
            panel.background = element_rect(colour = "#E6E6F1"),
            axis.text = element_text(size = "2.5em", angle = 0),
            axis.title = element_text(size = "3em"),
            plot.title = element_text(size = 15, face = "bold")
      )+
      labs(
           y = "% Transmission",
           x = "Wavelength (nm)"
      )
      }
  })
})