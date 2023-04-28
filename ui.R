library(shiny)
library(tidyverse)
library(bslib)

our_data <- readxl::read_excel("Master_1.xlsx",
                               sheet = "Lens_details")

oem_data <- readxl::read_excel("Master_1.xlsx",
                               sheet = "Laser_types") %>% 
  drop_na(`Eyewear Requirement`, `Compatible Lens 1`)

od_data <- readxl::read_excel("OD_Measurements.xlsx")

shinyUI(page_fluid(
  theme = bs_theme(
      version = 5,
      base_font = font_google("Karla"),
      bg = "white",
      fg = "black",
      primary = "royalblue"
    ),
  h3(imageOutput("logo", height = "50px", width = "180px")),
    card_header(fluidRow(
                         column(12,align = 'center',h3(strong("Select a laser")))
                         ),
      fluidRow(
        column(
          6,
          align = 'center',
          selectInput(
            inputId = "mfg",
            label = "Manufacturer",
            choices = sort(unique(oem_data$`Mfg`)),
            selected = "Alma",
          )),column(6,align='center',
          selectInput(
            inputId = "mod",
            label = "Model",
            choices = NULL,
            selected = NULL
          )
        ))),
  # take user inputs
  # press run and the conditional panel shows up
    
             conditionalPanel(
    condition = "input.mod",
   #  fluidRow(column(12,align='center',
   #                  p(strong("Feel free to look around - "), "find laser safety eyewear that meets a laser's specifications, ",
   #  em( "quickly. "), "Stay as long as you like. If this tool is not useful to you or you have questions, then ", a("contact", href = "innovativeoptics.com/contact"),
   # " us. Our team have more than a century's worth of collective experience in laser safety and are happy to help. ")
   #  )),
    card(card_header(h4("Reference its specifications")),
    card_body(fluidRow(column(8,align='center',offset =2,
                              # print confirmation statement
                              tableOutput("laserInfo")
    )
    ))),
    navs_tab_card(
                         nav(strong('Lens options'),
                       card_header(
                          h4(textOutput("devName")))
                          ,
                       card(layout_column_wrap(width = 3,
                                               card_body(
                                                 h5(strong(fluidRow(column(4,
                                                                           
                                                                           htmlOutput("image_Lens1"),
                                                                           htmlOutput("link_Lens1"),
                                                                 align = 'center'),
                                                          column(4,
                                                                 
                                                                 htmlOutput("image_Lens2"),
                                                                 htmlOutput("link_Lens2"),
                                                                 align = 'center'),
                                                          column(4,
                                                                 
                                                                 htmlOutput("image_Lens3"),
                                                                 htmlOutput("link_Lens3"),
                                                                 align = 'center'))
                                               )))))
        ,
                       
        card_footer(
          p("\"According to ANSI Z136, the US laser safety standard, everyone within the ocular hazard zone of a laser is supposed to wear
                          eye protection. With the abundance of laser safety options available, searching for the right set can be a time-consuming task. This is why we created this
            tool; to save our customers time by immediately providing compatible options and also informing their selection with a deep dive section. This was 
            our attempt at balancing the opposites of laser safety. We hope you find it useful.\""),
          p(em(strong("- Innovative Optics,")),
            em("Departments of Customer Service, Marketing and Regulatory Compliance"))
          )
        ),
    # recommend two products, with specs and comparison between two and show both graphs between
   
  nav(strong("Deep dive comparison"),
          card_header(h6("Compare the attenuation (OD) and transmission (VLT) curves of the compatible eyewear.")),
          card(
                         layout_column_wrap(width = 4, height = "240px", height_mobile = "100%",
                             
                 plotOutput("od_plot_1"),
                 plotOutput("vlt_plot_1"),
                 plotOutput("od_plot_2"),
                 plotOutput("vlt_plot_2")
        ),
        fluidRow(column(
          12, align = 'center',
          tableOutput("specs_table")
        )),
        em("Note - the 3rd option is intended for IPL protection only (if applicable).")),
        
        card_footer(fluidRow(column(12,
                        p(h4("Guide"),
                        tags$li("Laser safety eyewear should transmit green light if that is the color
                                of the laser's aiming beam"),
                        tags$li("When treating red/violet vascular lesions one would like 
                           to maximize red/violet light transmission"),
                        tags$li("Patients may be more comfortable 
                           with shady, high optical density/low transmission eyewear"))))
      ))
      )
  )
)
)