
####                                  BirdMApp v1.0                                      ####
#                                 (c) 2023 Orson De Witt                                    #
####                             https://orsondewitt.com                                 ####

#bs_theme(version = 3)
ui <- fluidPage(
    tags$head(
      tags$title("BirdMApp by Orson De Witt")
    ),
    tags$script(
      HTML(
        'Shiny.addCustomMessageHandler("togglePanel", function(show) {
                                      if (show) {
                                        $(".conditional-panel").show();
                                      } else {
                                        $(".conditional-panel").hide();
                                      }
                                    });
        ',".shiny-notification {
                                       height: 100px;
                                       width: 800px;
                                       position:fixed;
                                       top: calc(50% - 50px);
                                       left: calc(50% - 400px);
                                       font-size: 250%;
                                       text-align: center;
                                       }
                                       ")),
    # bsPopover script modifications indented, original code lumped together
    tags$script('function bsPopover(popid,placement,title,content,bgcolor,titlecolor,contentcolor,titlesize,contentsize,trigger,titleweight,contentweight,opacity,html,clickInside){let pop=$(`[data-popoverid="${popid}"]`).popover({animation:!clickInside,placement:placement,title:title,content:content,trigger:trigger,html:html,container:"body"});
                let debounceTimer;
pop.on("inserted.bs.popover",function(){var pop=$(`#${$(this).attr("aria-describedby")}`);pop.css({opacity:opacity,borderColor:bgcolor,padding:0});pop.find(".popover-title").css({backgroundColor:bgcolor,opacity:0.8,fontSize:titlesize,color:titlecolor,fontWeight:titleweight});pop.find(".arrow").css(`border-${placement}-color`,bgcolor);pop.find(".popover-content").css({fontSize:contentsize,color:contentcolor,fontWeight:contentweight})});
                if (clickInside) {
                  pop.on("mouseenter", function() {
                    let _this = this;
                    clearTimeout(debounceTimer);
                    debounceTimer = setTimeout(function() { $(_this).popover("show"); }, 200);
                    $(".popover").on("mouseleave", function() { $(_this).popover("hide"); });
                  }).on("mouseleave", function() {
                    let _this = this;
                    clearTimeout(debounceTimer);
                    debounceTimer = setTimeout(function() {
                      if (!$(".popover:hover").length) { $(_this).popover("hide"); } }, 300);
                  });
                }
              }'),
    #selectize-input overflow to fix the bottom margin
    tags$style(HTML(" .noPhoto {
                                  font-size: 30px !important; text-align: center !important;
                                  border: 1px solid #ddd !important; border-radius: 4px !important;
                                  padding: 5px !important; width: auto !important; height: auto !important; margin: 120px;
                                  }
                      .imgPhoto {
                                  border: 1px solid #ddd;
                                  border-radius: 4px;
                                  padding: 5px;
                                  max-width: 35vw;
                                  width: auto;
                                  height: 40vh;
                                  }
                      .userPhoto {
                                  border: 1px solid #ddd;
                                  border-radius: 4px;
                                  padding: 5px;
                                  background: rgb(64,91,152);
                                  background: linear-gradient(315deg, rgba(64,91,152,1) 0%, rgba(229,184,11,1) 88%); 
                                  max-width: 35vw;
                                  width: auto;
                                  height: 40vh;
                                  }
                      .fa-bars    {color:#fff}
                      #table-title {
                                  position: absolute; top: 25px;
                                  font-size: 1.7rem; font-weight: 700;
                                  }
                      .table-chart-bar {
                                  background-color: #e8e8e8;
                                  display: block;
                                  position:relative;
                                  width: 100%;
                                  height: 20px;
                                  }
                      .table-bar {
                                  background-color: #C23531;
                                  float: left;
                                  height: 100%;
                      }
                      .dataTables_filter {
                                  margin-top: 10px;
                                  margin-bottom: 10px;
                                  margin-right: 5px;
                                  }
                      #tableContainer {
                                  background-color: white;
                                  position: relative;
                                  margin-top: 15px;
                                  }
                      #spinnerContainer::before {
                                  background-color: white;
                                  content: '';
                                  display: block;
                                  position: absolute;
                                  top: 20px;
                                  left: 0;
                                  right: 0;
                                  bottom: 0;
                                  z-index: 9999;
                                  }
                      .selectize-input {
                                  overflow: visible;
                                  color: #000;
                                  font-weight: bold;
                                  }
                      .leaflet_slider .irs--shiny .irs-bar {
                                  background: rgba(64,91,152,0.8);
                                  border-top: 1px solid rgba(64,91,152,0.8);
                                  border-bottom: 1px solid rgba(64,91,152,0.8);
                                  }
                      .leaflet_slider .irs--shiny .irs-to, .irs--shiny .irs-from {
                                  background-color: rgba(64,91,152,0.8);
                                  }
                      .leaflet_slider .irs--shiny .irs-handle {
                                  width: 12px;
                                  height: 24px;
                                  background-color: #333;
                                  border-radius: 6px;
                                  margin-left: -6px;
                                  }
                      .form-group { margin-bottom: 0px; }
                      .flexModal { width: 40vw; }
                      .modal-title { text-align: center; }
                      #pathBrowser { padding-top: 10px; }
                      .modal-dialog { width: fit-content !important; }
                      .leaflet-popup-photo a.leaflet-popup-close-button {
                                	top: -13px;
                                	right: -13px;
                                	background-color: #fff;
                                	padding: 5px;
                                	border-radius: 12px;
                                	width: 15px;
                                	height: 15px;
                                	box-shadow: 0 3px 14px rgba(0,0,0,0.4);
                                	color: #555;	
                                  }
                      .marker-cluster-small {
                                  background-color: rgba(0, 0, 0, 1);
                                  position: absolute;
                                  font-family: 'FontAwesome';
                                  top: 0;
                                  left: 10px;
                                  content: '\f03e';
                                  }
                      .marker-cluster-medium {
                                  background-color: rgba(0, 0, 0, 1);
                                  position: absolute;
                                  font-family: 'FontAwesome';
                                  top: 0;
                                  left: 10px;
                                  content: '\f03e';
                                  }
                      .marker-cluster-large {
                                  background-color: rgba(0, 0, 0, 1);
                                  position: absolute;
                                  font-family: 'FontAwesome';
                                  top: 0;
                                  left: 10px;
                                  content: '\f03e';
                                  }
                      .popover { max-width: 1000px; }
                      .popoverContainer { position: relative; }
                      .popoverRight {
                                  text-align: right !important;
                                  font-size: 1.75rem !important;
                                  }
                      .userColor { color: #405B98 !important; }
                      .popoverTitleLeft {
                                  top: 10px !important;
                                  text-align: left !important;
                                  position: absolute !important;
                                  left: 2.5% !important;
                                  font-size: 16px !important;
                                  font-weight: regular !important;
                                  color: #000000;
                                  }
                      .popoverTitleRight {
                                  top: 8px !important;
                                  text-align: right !important;
                                  position: absolute !important;
                                  right: 2.5% !important;
                                  font-size: 18px !important;
                                  }
                      .popoverCenter {
                                  font-size: 18px !important;
                                  text-align:center !important;
                                  color: #ffffff !important; 
                                  }
                      .redlistLC { color:rgba(0, 102, 102, 1) !important; }
                      .redlistNT { color:rgba(205, 154, 100, 1) !important; }
                      .redlistVU { color:rgba(205, 154, 0, 1) !important;  }
                      .redlistEN { color:rgba(205, 102, 48, 1) !important; }
                      .redlistCR { color:rgba(205, 48, 48, 1) !important; }
                      .redlistDD { color:rgba(195, 195, 195, 1) !important; }
                      .redlist   { color: #000000 !important; }
                      .popspan   { color: #ffffff !important; }
                      .popspan2  { color: #ffffff !important; text-align: right !important; }
                      .card-container {
                                  display: flex;
                                  flex-wrap: wrap;
                                  justify-content: space-between;
                                  }
                      .card {
                                  flex: 0 0 calc(50% - 20px);
                                  margin: 10px;
                                  padding: 10px;
                                  background-color: #f7f7f7;
                                  border: 1px solid #ddd;
                                  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                                  box-sizing: border-box;
                                  }
                      .full-width-card {
                                  flex-basis: calc(100vw - 20px);
                                  margin: 10px;
                                  padding: 10px;
                                  background-color: #f7f7f7;
                                  border: 1px solid #ddd;
                                  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                                  box-sizing: border-box;
                                  } 
                      ul.nav-tabs { display: flex !important; justify-content: center !important;}
                      .nav-tabs .active { font-weight: bold; }
                      .picker-label { font-size: 30px; }
                      .plot {
                                  margin: 10px -10px 10px;
                                  padding: 10px -10px 10px;
                                  background-color: #f7f7f7;
                                  border: 1px solid #ddd;
                                  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                                  box-sizing: border-box;
                      }
                      .dropdown-item .text-muted {
                                  position: absolute;
                                  right: 10%;
                                  font-size: 75%;
                      }
                      @media (max-width: 1820px) {
                                  .flexModal { width: 45vw; }
                      }
                      @media (max-width: 1620px) {
                                  .flexModal { width: 55vw; }
                      }
                      @media (max-width: 1330px) {
                                  .flexModal { width: 60vw; }
                      }
                      @media (max-width: 1220px) {
                                  .flexModal { width: 65vw; }
                      }
                      @media (max-width: 1120px) {
                                  .flexModal { width: 70vw; }
                      }
                      @media (max-width: 1030px) {
                                  .flexModal { width: 75vw; }
                      }
                      @media (max-width: 960px) {
                                  .flexModal { width: 90vw; }
                      }
                      @media (max-width: 768px) {
                                  .card {
                                    flex: 0 0 calc(100% - 20px);
                                  }
                                
                                  .full-width-card {
                                    flex-basis: calc(100% - 20px);
                                  }
                                  
                                  .flexModal { width: 95vw; }
                      }
                "), 
               type = 'text/css', 
                      # Fix the conflict between SelectizeInput and HighCharter, 
                      # where .loading style from Highcharter's 'motion.css' makes selectize form jump up and down
                      '#form .loading {
                                  margin-top: 0em !important;
                                  text-align: left !important;
                                  color: black !important;
                                  }',
                      '.menu-item { 
                                  display: block;
                                  padding: 10px;
                                  margin: 5px 0;
                                  color: #333;
                                  text-decoration: none;
                                  }', # menu item: popup menu
                      '.menu-item:hover { background-color: #eee; }',
                      '.marker-cluster-large div { 
                                  background-color:rgba(116, 173, 209, 40);
                                  color:#000000; 
                                  font-weight: bold;
                                  }', #.marker-cluster-small, medium div, leaflet
                      '.closeButton {
                                  class: fa-solid fa-2xl;
                                  background-color: #be0000;
                                  border: none;
                                  color: white;
                                  text-align: center;
                                  text-decoration: none;
                                  display: inline-block;
                                  top: 10px; 
                                  right: 10px;
                                  font-size: 12px;
                                  border-radius: 4px;}', # modal
                      '.leaflet-popup:has(+div) {margin: 0;}',
                      '.leaflet-popup-content-wrapper {text-align: center; display: inline-block;}' ,
                      '.leaflet-popup-content-wrapper:has(+div) {padding: 0px; border-radius: 1px;}' ,
                      '.leaflet-popup-photo img, .leaflet-popup-photo video { display: inline-block;}',
                      '.polaroid p {
                                  font-style: bold;
                                  bottom: 0px;
                                  font-size: 1.6rem;
                                  color: #888;}',
                      '.polaroid img {
                                  padding: 2px;
                                  margin-top: 6px;
                                  margin-left: -12px;
                                  padding-left: -12px;
                                  margin-bottom: -6px;
                                  width: 326px;
                                  height: 326px;
                                  }',
                ),
  # tags$script(HTML('
  #   $(document).on("click", "#openNestedModal", function() {
  #     $("#nestedModal").show();
  #   });
  # ')),
  spsDepend("pop-tip"), # Needed for pop-ups to work
  useShinyjs(),
  tags$div(style = "display: none; margin-top: 10px;", id = "conditional",
     fluidRow(
         column(width = 2,
                # actionButton("renderMap", "Render Map"), #for testing
                # actionButton("rerenderMap", "Re-Render Map"), #for testing
                radioGroupButtons(
                  inputId = "bodyTab",
                  label = NULL,
                  choices = c(`<i class="fa-solid fa-earth-africa"></i>` = "map",
                              `<i class='fa fa-bar-chart'></i>` = "bar")
                )),
         div(id = "search",
             column(width = 8,
                    div(id = "form", 
                        style="display:flex; flex-direction: column-reverse; align-items: center;",
                        div(style = "position: relative;",
                            selectizeInput("species", label = NULL, choices = NULL),
                            actionButton("clearspecies", label = NULL, icon = icon("remove"), 
                                         style = "position: absolute; top: 38%; 
                                         transform: translateY(-50%); right: -35px; height:35px;"),
             )))),
         tags$div(style = "position: absolute; top: 8px; right: 14px;",
                  actionBttn(inputId = "menuButton",
                             label = NULL,
                             style = "simple",
                             color = "primary",
                             icon = icon("bars")
                   ))),
         tags$div(id = "menuPopup",
                  style = "display: none; position: fixed; top: 50px; right: 14px; 
                  background-color: white; padding: 10px; width: 150px; z-index: 1000;",
                  class = "dropdown-menu dropdown-menu-right",
                  # For photos
                  actionLink("refreshData", "Sync Photos", 
                             icon = icon("camera-rotate", lib="font-awesome"), class = "menu-item") %>%
                    bsPopover("If you've added new photos and wish to include them (fastest option)", 
                              "Does not work for deleted photos. 
                              If you wish to remove deleted photos please re-index instead.", html = TRUE, "left"),
                  actionLink("reIndexData", "Re-index Folder", 
                             icon = icon("magnifying-glass-location", lib="font-awesome"), class = "menu-item") %>%
                    bsPopover("Re-index folder", #paste0('Current folder: '), 
                              "If you've updated tags in your photos, renamed photos or there are inconsistencies", 
                              html = TRUE, "left"),
                  actionLink("changeData", "Change Folder", 
                             icon = icon("folder-open", lib="font-awesome"), class = "menu-item") %>%
                    bsPopover("Change folder", "Index another folder", 
                              html = TRUE, "left"),
                  # For CSV File
                  actionLink("CSVreIndexData", "Sync", 
                             icon = icon("magnifying-glass-location", lib="font-awesome"), class = "menu-item"),
                  actionLink("CSVchangeData", "Switch Data", 
                             icon = icon("folder-open", lib="font-awesome"), class = "menu-item"),
                  # Both
                  tags$hr(), 
                  actionLink("reportIssue", "Report Issue", 
                             icon = icon("pen-to-square", lib="font-awesome"), class = "menu-item") %>%
                    bsPopover(HTML(paste0("<a href='https://discord.com/invite/c48wxW4rer'>Discord</a>")), HTML(paste0("<a href='https://github.com/OrsonDeWitt/BirdMApp/issues'>GitHub</a>")), 
                              click_inside = TRUE, html = TRUE, "left"),
                  actionLink("website", "Website", 
                             icon = icon("up-right-from-square", lib="font-awesome"), class = "menu-item")
         ),
     
    # Busy indicator
    tags$div(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 9999;",
      add_busy_spinner(spin = "fading-circle", timeout = 100)
    ),
    
    tags$div(id = "bodyMap",
             absolutePanel(id = "customDiv",
                           class = "custom-panel",
                           style = "z-index: 999 !important; position: absolute; 
                                    top: 60px; right: 25px; padding: 5px; width: 125px;",
                           draggable = TRUE,
                           div(
                             pickerInput(
                               inputId = "monthFilter",
                               label = NULL, 
                               selected = "Year-round",
                               choices = c("Year-round", "January", "February", "March", "April", "May", "June", 
                                           "July", "August", "September", "October", "November", "December"),
                               options = list(
                                 width = "120px",
                                 title = "Year-round",
                                 showTick = TRUE
                               ),
                               choicesOpt = list(
                                 style = c("font-weight: bold;"))
                 ))),
                 leafletOutput("map", height = "90vh")),
    tags$div(style = "display: none;", id = "bodyCharts",
             titlePanel(h1("Personal Statistics" , align = "center")),
             tabsetPanel(type = "tabs",
                         tabPanel("General", 
                                  fluidRow(style = "margin-left: 10px; margin-right: 10px;",
                                  column(6, div(id = "table-title", "Most Explored Regions (unique sp.)"), 
                                            div(class = "plot", DTOutput("regionsTable"))),
                                  column(6, div(class = "plot", 
                                            highchartOutput("chartLine", height = 497)))
                         )),
                         tabPanel("Detailed", 
                                  fluidRow(style = "margin-left: 10px; margin-right: 10px;",
                                  column(12, align = "center", tags$br(), 
                                         pickerInput(inputId = "realms",
                                                    width = 450,
                                                    label = "Biogeographic Realms", 
                                                    selected = "Worldwide",
                                                    choices = c("Worldwide", "Nearctic", "Panamanian", 
                                                                "Neotropical", "Palearctic", "Saharo-Arabian", 
                                                                "Afrotropical", "Madagascan", "Sino-Japanese", 
                                                                "Oriental",  "Oceanian", "Australasian", "Antarctic"),
                                                    # Subtext on the right side of the choice
                                                    choicesOpt = list(
                                                      subtext = paste("", c("", "North America", "Central America", 
                                                                            "South America", "Northern Eurasia", 
                                                                            "West Asia & Northern Africa", 
                                                                            "Sub-Saharan Africa", 
                                                                            "Madagascar & neighboring islands", 
                                                                            "Japan, Bhutan, Nepal & Northern China", 
                                                                            "South & Southeast Asia", "Oceania", 
                                                                            "Australia, New Zealand & Tokelau", 
                                                                            "Antarctica, FK, GS, HM, TF, BV"),
                                                                      sep = "")),
                                                    multiple = TRUE,
                                                    options = list(style = "btn-primary",
                                                                  `actions-box` = TRUE)) %>%
                                           bsPopover("Biogeographic regionalization © 
<a href='https://www.researchgate.net/publication/235237906_An_Update_of_Wallace%27s_Zoogeographic_Regions_of_the_World' 
                                           target='_blank'>Holt et. al (DOI: 10.1126/science.1228282)</a>", "",
                                           placement = "top", bgcolor = "#e8e8e8", click_inside = TRUE, html = TRUE)
                                         )),
                                  fluidRow(style = "margin-left: 10px; margin-right: 10px;",
                                  column(6, 
                                         div(class = "plot", plotlyOutput("histogram1"))),
                                  column(6, 
                                         div(class = "plot", plotlyOutput("histogram2")))),
                                  fluidRow(style = "margin-left: 10px; margin-right: 10px;",
                                  column(6, 
                                         div(class = "plot", plotlyOutput("histogram3"))),
                                  column(6, 
                                         div(class = "plot", plotlyOutput("histogram4"))))),
                         tabPanel("Most Active Days", 
                                  fluidRow(style = "margin-left: 10px; margin-right: 10px;",
                                  column(12, align="center",
                                         div(class = "full-width-card", plotlyOutput("calendar"))))),
                         tabPanel("Most Observed Species", 
                                  fluidRow(style = "margin-left: 10px; margin-right: 10px;",
                                  column(12,
                                         highchartOutput("treemapChart", height = 700)))),
                         tabPanel("Families", 
                                  fluidRow(style = "margin-left: 10px; margin-right: 10px;",
                                  column(6, 
                                         div(id = "table-title", "Breakdown by Family (unique sp.)"), 
                                         div(class = "plot", DTOutput("familiesTable"))),
                                  column(6, 
                                         div(class = "plot", highchartOutput("itemChart", height = 497)))
                         ))
             )),
    
  tags$div(style = "margin-top: 10px; padding-left: 2.5%; padding-right: 2.5%", 
           class = "leaflet_slider", id = "leafletSlider",
           uiOutput("leafletSlider"))
))
