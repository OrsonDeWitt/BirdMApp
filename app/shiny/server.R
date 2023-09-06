

####                                   BirdMApp v1.0.1                                        ####
#                                   (c) 2023 Orson De Witt                                      #
####                               https://orsondewitt.com                                   ####

server <- function(input, output, session) {
  
  print("BirdMApp v1.0.1")
  library(data.table)
  photoDir <- reactiveVal(NULL)
  observe({ photoDir <- "" })
  
  ####                                                                                        ####
  #                                   Data Import System                                         #
  ####                                                                                        ####
  
  varPath <- getwd()
  addResourcePath("wd", varPath)
  showPathBrowser <- FALSE
  path <- reactive({
                    if (length(input$drive) == 0 || input$drive == "C:/") {return("~")}
                    else { return(input$drive) } 
                  })
  filebrowser <- file_browser_server("files", path = path, root = reactive(input$drive),
                                     extensions = "",
                                     show_size = FALSE,
                                     include_hidden = FALSE,
                                     text_empty = "",
                                     include_empty = FALSE)

  importModal <- function(){
    showModal(
      modalDialog(
        id = "importMethod",
        
        # Header
        tags$h4(style = "color: #B96148; font-size: 28px; text-align: center; margin-top: 30px;", 
                "Please select import method"),
        tags$div(style = "text-align: center; font-size: 15px; ", 
                 #"...", 
                 #tags$br(), "...", 
                 tags$br()), 
        tags$div(style = "position: relative; height: 1px; margin-bottom: 20px; margin-top: 30px",
                 tags$div(style = "position: absolute; left: 0; top: 0; width: 45%; height: 100%; background-color: #B96148;"),
                 tags$span(
                   style = "position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); color: #B96148; font-size: 20px;",
                   class = "fas fa-feather"
                 ),
                 tags$div(style = "position: absolute; right: 0; top: 0; width: 45%; height: 100%; background-color: #B96148;")
        ),
        
      # Import options
      tags$div(
        style = "display: flex; justify-content: center;", class = "flexModal",
        tags$div(style = "display: flex; justify-content: space-between; width: 100%; height:130%;",
        actionButton("photoPath", HTML("Photo Folder<br><br><span style='font-size: 12px;'>
                                       I have a folder with tagged photos that I want analyzed and, if any of them have geotags, 
                                       I want to have them plotted on the map</span>"), 
                     style = "width: 80%; white-space: normal; font-size:4rem; line-height: 0.4; padding:30px; padding-top: 40px; margin:10px; "),
        actionButton("csvImport", HTML("CSV File<br><br><span style='font-size: 12px;'>
                                       I have a .csv file with either of the following columns: \"scientificName\", 
                                       \"scientific name\", \"commonName\", or \"common name\", as well \"latitude\", 
                                       \"longitude\", and \"date\"</span>"), 
                     style = "width: 80%; white-space: normal; font-size:4rem; line-height: 0.4; padding:30px; padding-top: 40px; margin:10px; ")
      )),
      #   class = "container-fluid",
      #   div(
      #     class = "row",
      #     div(
      #       class = "col-md-6 column column-left" ...
      footer = NULL
    ))
}
  
  if (file.exists(paste0(varPath, "/userData/Use.RData"))){
    load(paste0(varPath, "/userData/Use.RData"))
    print(photoList)
    varPath <- getwd()
    addResourcePath("wd", varPath)
    if (photoDir != ""){ addResourcePath("photos", photoDir) }
    shinyjs::show(id = "conditional")
    # Hide irrelevant buttons from the menu
    if (csvFile != ""){ 
      shinyjs::hide("refreshData")
      shinyjs::hide("reIndexData")
      shinyjs::hide("changeData")
    } else {
      shinyjs::hide("CSVreIndexData")
      shinyjs::hide("CSVchangeData")
    }
    setkeyv(Abundances, c("scientificName", "countrycode", "month", "Present"))
  } else {
    importModal()
  }
  
  observeEvent(input$photoPath, {
    pathModal()
  })

  # Path browser for photos
  pathModal <- function(){
    photoDir <- ""
    photoList <<- data.table()
    showModal(
      modalDialog(
        id = "pathBrowser",
        
        # Header
        tags$h4(style = "color: #B96148; font-size: 28px; text-align: center; margin-top: 30px;", 
                "Select the directory that contains all your tagged bird photos"),
        tags$div(style = "text-align: center; font-size: 15px; ", 
                 "Selecting a folder with other photos will not affect the analysis but might significantly increase the loading time.", 
                 tags$br(), "File names with non-Latin letters are not supported and may lead to unexpected results.", tags$br()), 
        tags$div(style = "position: relative; height: 1px; margin-bottom: 20px; margin-top: 30px",
                tags$div(style = "position: absolute; left: 0; top: 0; width: 45%; height: 100%; background-color: #B96148;"),
                tags$span(
                    style = "position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); color: #B96148; font-size: 20px;",
                    class = "fas fa-feather"
                  ),
                tags$div(style = "position: absolute; right: 0; top: 0; width: 45%; height: 100%; background-color: #B96148;")
        ),
        
        # Options
        tags$h2("Options", style = "color: #B96148; font-size: 16px; margin: 0;"),
        tags$div(
          class = "button-container",
          style = "display: flex; justify-content: space-between; margin-top: 16px;",
          prettyRadioButtons(
            inputId = "Id000",
            status = "success",
            label = NULL,
            shape = "curve",
            choiceNames = c("My tags use English names", "My tags use scientific names", "My tags omit adjectives"),
            choiceValues = c("tagsEnglish", "tagsScientific", "tagsAdj"),
            selected = "tagsEnglish",
            outline = TRUE),
          prettyCheckbox(
            inputId = "zoo",
            label = "Include photos with \"zoo\" tag", 
            value = FALSE,
            status = "success",
            shape = "curve",
            outline = TRUE),
          tags$div(style = "display: inline-block; justify-content: space-between; line-height: 155%;",
            prettyCheckbox(
              inputId = "replaceNames",
              label = "Use my names over accepted names", #Eurasian Eagle Owl vs. Eurasian Eagle-owl
              value = FALSE,
              status = "success",
              shape = "curve",
              outline = TRUE),
            prettyCheckbox(
              inputId = "usePhotos",
              label = "Use my photos in species lists", #photo choice is dictated by its rating
              value = FALSE,
              status = "success",
              shape = "curve",
              outline = TRUE) )),
        uiOutput("picker"),
        
        # Folder selection
        tags$div(
          style = "color: #B96148; font-size: 16px; font-weight: regular;",
          selectInput("drive", "Drive", get_drives())),
        file_browser_ui("files"),
        footer = actionButton("PathChosen", "OK")
      ))
    }
  
  output$picker <- renderUI({
    if (input$Id000 == "tagsAdj") {
      div(style = "display: inline-block; margin-top: 8px; width: 90%;", 
      HTML('Choose one adjective to apply to your tags.<br>
           For example, if you live in Europe and have tagged your Eurasian Blackbirds simply "Blackbird", 
           <br>your European Robins "Robin", or your House Sparrows "Sparrow"'),
      div(
        style = "display: flex; margin-top: 10px; justify-content:space-between; margin-right: 25%",
      pickerInput(
        inputId = "Regional",
        selected = "",
        label = NULL,
        choices = list(
          "Select one" = c(""),
          directions = c("Northern", "Western", "Eastern", "Southern"),
          regions = c("African/Somali", "Alpine", "Amazonian", "American", "Andaman", "Andean", 
                      "Arabian", "Asian/Indian", "Australian", "Bolivian", "Bornean", "Ecuadorian", 
                      "Eurasian/European", "Hawaiian", "Himalayan", "Hispaniolan", "Iraq", "Jamaican", 
                      "Japanese", "Javan", "Madagascar", "Malay", "Moluccan", "New Caledonian", 
                      "New Guinea", "New Zealand", "Pacific", "Papuan", "Peruvian", "Philippine/Mindanao", 
                      "Puerto Rican", "Santa Marta", "Sao Tome", "Seychelles", "Siberian", "Sri Lanka", 
                      "Sulawesi", "Sumatran", "Sunda", "Taiwan", "Tepui", "Tibetan", "Timor", "Yucatan"),
          # I have not tested adjectives below on whether they have conflicts with other adjectives/regions
          # So I am only allowing single choice for now (of adjectives that have at least 12 counts)
          adjectives = c("Banded", "Band-tailed", "Black-backed", "Black-bellied", "Black-breasted", 
                         "Black-chinned", "Black-crowned", "Black-headed", "Black-throated", "Black-winged", 
                         "Blue-headed", "Blue-throated", "Cape", "Chestnut-backed", "Chestnut-bellied", 
                         "Collared", "Crested", "Dusky", "Green-backed", "Grey-headed", "House", "Forest", 
                         "Long-tailed", "Mangrove", "Painted", "Plain", "Red-faced", "Red-tailed", 
                         "Red-throated", "Ruddy", "Rufous", "Rufous-breasted", "Slender-billed", "Speckled", 
                         "Spotted", "Streaked", "Striated", "Striped", "White-bellied", "White-breasted", 
                         "White-browed", "White-cheeked", "White-eared", "White-faced", "White-fronted", 
                         "White-naped", "White-rumped", "White-throated", "White-winged", "Yellow-breasted", 
                         "Yellow-browed") # Domestic?
        ),
        multiple = FALSE
      ),
      prettyCheckbox(
        inputId = "Common",
        label = 'If species not found, try to match by adding "Common"',
        value = FALSE,
        status = "success",
        shape = "curve",
        outline = TRUE
      )))
    }
  })
  
  observeEvent(input$csvImport, {
    csvModal()
  })
  
  csvModal <- function(){
    photoDir <- ""
    photoList <<- data.table()
    filebrowser <- file_browser_server("files", path = path, root = reactive(input$drive),
                                       extensions = "csv",
                                       show_size = TRUE,
                                       include_hidden = FALSE,
                                       text_empty = "",
                                       include_empty = FALSE)
    showModal(
      modalDialog(
        id = "fileBrowser",
        # Header
        # tags$h4(style = "color: #B96148; font-size: 28px; text-align: center; margin-top: 30px;", 
        #         ""),
        tags$div(style = "text-align: center; font-size: 15px; ",
                 "Any brackets that follow the name of the species will be removed.",
                 tags$br()), #"", tags$br()),
        tags$div(style = "position: relative; height: 1px; margin-bottom: 20px; margin-top: 30px",
                 tags$div(style = "position: absolute; left: 0; top: 0; width: 45%; height: 100%; background-color: #B96148;"),
                 tags$span(
                   style = "position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); color: #B96148; font-size: 20px;",
                   class = "fas fa-feather"
                 ),
                 tags$div(style = "position: absolute; right: 0; top: 0; width: 45%; height: 100%; background-color: #B96148;")
        ),
        
        # Options
        tags$h2("Options", style = "color: #B96148; font-size: 16px; margin: 0;"),
        tags$div(
          class = "button-container",
          style = "display: flex; justify-content: space-between; margin-top: 16px;",
          prettyRadioButtons(
            inputId = "Id000",
            status = "success",
            label = NULL,
            shape = "curve",
            choiceNames = c("English names", "Scientific names", "Match omitted adjectives"),
            choiceValues = c("tagsEnglish", "tagsScientific", "tagsAdj"),
            selected = "tagsEnglish",
            outline = TRUE),
          tags$div(style = "display: inline-block; justify-content: space-between; line-height: 155%;",
                   prettyCheckbox(
                     inputId = "replaceNames",
                     label = "Use my names over accepted names", #Eurasian Eagle Owl vs. Eurasian Eagle-owl
                     value = FALSE,
                     status = "success",
                     shape = "curve",
                     outline = TRUE))
          ),
        uiOutput("picker"),
        uiOutput("datepicker"),
        
        # Folder selection
        tags$div(
          style = "color: #B96148; font-size: 16px; font-weight: regular;",
          selectInput("drive", "Drive", get_drives())),
        file_browser_ui("files"),
        footer = actionButton("CSVChosen", "OK")
      ))
  }

    output$datepicker <- renderUI({
      div(style = "display: inline-block; margin-top: 8px; width: 90%;", 
          HTML('You have to choose the date format of your "date" column.'),
          div(
            style = "display: flex; margin-top: 10px; justify-content:space-between; margin-right: 25%",
            pickerInput(
              inputId = "dateformat",
              selected = "",
              label = NULL,
              choices = list(
                "Select one" = c(""),
                formats = c("%d.%m.%y", "%m.%d.%y", "%y.%m.%d", "%y.%d.%m", "%d.%y.%m", "%d-%m-%y", 
                            "%d-%y-%m", "%m-%d-%y", "%m-%y-%d", "%y-%m-%d", "%y-%d-%m", "%d/%m/%y", 
                            "%d/%y/%m", "%y/%m/%d", "%y/%d/%m", "%m-%d-%y")
              ),
              multiple = FALSE
      )))
  })

  observeEvent(input$PathChosen, {
    # Isolating the variable
    # So that it is static and can be saved and used outside observeEvent
    isolate({ photoDir <<- filebrowser$path() 
                             csvFile <<- "" })
    # Add resource path so it's usable in the future, too
    addResourcePath("photos", photoDir)
    dataInput()
  })
   
  observeEvent(input$CSVChosen, {
    isolate({ csvFile <<- filebrowser$selected() 
                               photoDir <<- "" })
    dataInput()
  })
  
  ####                                                                                        ####
  #                                        Data Import                                           #
  ####                                                                                        ####
  
  
  dataInput <- function(){
    #show_modal_spinner(spin = "fingerprint", color = "#AAFF00", text = "Counting feathers...")
    show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", text = "Counting feathers...")
    # Gif by pch.vector @ Freepik
    Warning_Tags <<- data.table()
    Warning_noTags <<- data.table()
    Warning_noMatch <<- data.table()
    Warning_GPS <<- data.table()
    Warning_AThrush <<- 0
    error <<- ""
    
    print(paste0("Beginning analysis with custom settings. Zoo:", 
                 input$zoo, "; Replace names:", input$replaceNames, 
                 "; Names:", input$Id000, "; Common:", input$Common, 
                 "; Photos:", input$usePhotos, "; Adjectives:", input$Regional))
    
    # If there is an error when choosing directory/file
    if (is_null(csvFile) & photoDir == ""){
      error <<- paste0("There was an error selecting the file.<br>
      Please note that when a file is selected it appears blue.<br>
      If the issue persists, please restart the app.")
      NoBirds_Modal()
    } else {
    
    # Check if CSV or photos are to be loaded
    if (csvFile == ""){
      
    # Extract EXIF, GPS, IPTC, XMP, FlashPix, AFCP tags
    library(exiftoolr) # © Joshua O'Brien https://github.com/JoshOBrien/exiftoolr
    
    # Tags from video files cannot be extracted as they don't have any
    # Even with GPS added in Adobe and Tags added in Windows, ExifTool sees 0 tags
    image_files <<- list.files(photoDir, pattern=".jpg$|.JPG$|.jpeg$|.JPEG$", full.names = TRUE, 
                               recursive = TRUE)

    # Busy indicator
    if (length(image_files) > 1000) {
      if (length(image_files) > 3000) {
        show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", 
                       text = paste0(length(image_files), 
                                     " pictures found. \nDon't close this window, it will take a while."))
      } else {
        show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", 
                       text = paste0(length(image_files), " feathers found. What a magnificent collection! 
                                     \nIt might take a while. Come back in a few minutes."))}
    } else {
      show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", 
                     text = paste0(length(image_files), " feathers found. Hang on, it will take a minute."))
    }
    # Alternatively, in one line, in the same order:
    # show_modal_spinner(spin = "fulfilling-square", color = "#FF5733", text = paste0(length(image_files), " text1"))
    #, color = "#FFC300", text = paste0(length(image_files), " text2"))}
    #, color = "#405b98", text = paste0(length(image_files), " text3"))
    
    photoData <- as.data.table(
      exif_read(image_files, tags = c("gpslatitude", "gpslongitude", "subject", "DateTimeOriginal", "Rating")))
    
    show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", text = "We're almost done.")
    
    
    ####                                                                                        ####
    #                                       Data Cleaning                                          #
    ####                                                                                        ####
    
    
    # Calculate amount of columns required to separate Subject (# of comma-separated values)
    nTags <- max(stringr::str_count(photoData$Subject, ", ")) + 1
    
    print("Creating Warning_noTags table for photos without tags")
    
    if (any(is.null(photoData$Subject) | photoData$Subject == "NULL" | is.na(photoData$Subject) | photoData$Subject == "")) {
      Warning_noTags <<- photoData[(is.null(Subject) | Subject == "NULL" | is.na(Subject) | Subject == "")]
      Warning_noTags <<- Warning_noTags %>%
        mutate(DateTime = ymd_hms(DateTimeOriginal, quiet = TRUE),
               DateTime = format(DateTime, format = "%d %b %Y, %H:%M"),
               SourceFile = str_remove(SourceFile, photoDir),
               Error = "No tags found") %>% 
        select(-DateTimeOriginal, -Subject)
    }

    # Delete untagged photos, split Subject to Tags
    photoData <- photoData[!(is.null(Subject) | Subject == "NULL" | is.na(Subject) | Subject == "")]
    photoData[, paste0("Tag", seq_len(nTags)) := tstrsplit(Subject, '", "')][, Subject := NULL]

    # Get rid of special characters in Tags
    photoData[, Tag1 := gsub('c\\("', '', Tag1)]
    photoData[, (grep("Tag", names(photoData), value = TRUE)) :=
                lapply(.SD, function(x) gsub("(?![-'`])\\p{P}", "", x, perl = TRUE)),
              .SDcols = patterns("Tag")]
    
    # Re-use nTags for "Tag" column names, assign tags to properly named columns
    nTags <- grep("Tag", names(photoData), value=TRUE)
    photoData[, c("Animal Class", "Zoo") := .(
      apply(.SD, 1, function(z) intersect(tolower(c("bird", "spider", "bug", 
                                                    "mammal", "reptile", "gastropod", "fish")), tolower(z))[1]),
      apply(.SD, 1, function(z) "zoo" %in% z)), .SDcols = nTags]
    photoData[, (grep("Tag", names(photoData), value = TRUE)) :=
                lapply(.SD, function(x) ifelse(x %in% c("bird", "bug", "spider", "gastropod", "reptile", 
                                                        "mammal", "fish", "zoo", "uncertain"), NA_character_, x)),
              .SDcols = nTags]
    
    # Remove pictures from zoos if requested, add the amount to a variable to show in the WarningModal
    if (input$zoo == FALSE){
      inputZoo <<- FALSE
      deletedZooNum <<- nrow(photoData[Zoo==TRUE])
      photoData <- photoData[Zoo==FALSE]
    } else { inputZoo <<- TRUE } 
    print("Finished removing zoo tags")
    
    # Clean up
    photoData <- photoData %>%
      mutate(DateTime = ymd_hms(DateTimeOriginal, quiet = TRUE),
             DateTime = format(DateTime, format = "%d %b %Y, %H:%M"),
             YearMonthDay = as.POSIXct(DateTimeOriginal, format = "%Y:%m:%d"),
             YearMonth = format(YearMonthDay, "%Y-%m"),
             YearMonthDay = format(YearMonthDay, "%Y-%m-%d"),
             `Animal Class` = tools::toTitleCase(photoData$`Animal Class`),
             SourceFile = str_remove(SourceFile, photoDir)) %>% # Same as sub() in Base R
      arrange(YearMonthDay)
    
    # Make GPS coords numeric, otherwise leaflet validateCoords has issues
    photoData$GPSLongitude[is.na(photoData$GPSLongitude)] <- NA_integer_
    photoData$GPSLongitude <- as.numeric(photoData$GPSLongitude)
    photoData$GPSLatitude[is.na(photoData$GPSLatitude)] <- NA_integer_
    photoData$GPSLatitude <- as.numeric(photoData$GPSLatitude)
    
    # Divide photos with 2 or more species into separate rows
    # Pivot all the columns that start with "Tag" into a column "Species"
    photoList <<- photoData %>%
      pivot_longer(cols = starts_with("Tag"), values_to = "Species", values_drop_na = TRUE) %>%
      select(-name, -DateTimeOriginal) %>% # Remove the column created from pivoting + old date
      filter(!(`Animal Class` != "Bird" & !(is.na(`Animal Class`)))) 
      # ^ Filter out non-birds & non-NAs (in case they are untagged birds)
    
    print("Creating Warning_GPS table for photos without location")
    if (any(is.na(photoList$GPSLongitude) | is.na(photoList$GPSLatitude))) {
      Warning_GPS <<- copy(photoList) %>% 
        filter((is.na(GPSLatitude)) | (is.na(GPSLongitude))) %>% 
        select(-YearMonth, -YearMonthDay, -Zoo, -`Animal Class`) %>% 
        select(SourceFile, Species, DateTime, GPSLatitude, GPSLongitude, everything())
    }
    
    # CSV loading and clean-up
    } else { # if CSV exists
      print(paste0("date format:", input$dateformat))
      inputUserPhotos <<- FALSE
      inputZoo <<- FALSE
      image_files <<- FALSE
      photoList <<- read_csv(csvFile)
      nrow_for_warnings <<- nrow(photoList)
      colnames(photoList) <<- tolower(colnames(photoList))
      colnames(photoList) <<- str_remove_all(colnames(photoList)," ")
      if (input$Id000 == "tagsScientific") {
        if (exists("scientificname", where = photoList)){
          if (exists("latitude", where = photoList)){
            if (exists("longitude", where = photoList)){
              if (exists("date", where = photoList)){
                photoList <<- photoList %>%
                  mutate(date = ymd(date, quiet = TRUE))
                if (exists("time", where = photoList)){
                  photoList <<- photoList %>% select(scientificname, latitude, longitude, date, time)
                  photoList <<- photoList %>%
                    #mutate(time = hms(time, quiet = TRUE)) %>% 
                    rename(scientificName = scientificname) %>% 
                    mutate(scientificName = gsub("\\s*\\([^)]+\\)", "", scientificName))
                } else {
                  photoList <<- photoList %>% select(scientificname, latitude, longitude, date) %>% 
                    rename(scientificName = scientificname) %>% 
                    mutate(scientificName = gsub("\\s*\\([^)]+\\)", "", scientificName))
                }
              } else { error <<- "Column \"date\" not found." }
            } else { error <<- "Column \"longitude\" not found." }
          } else { error <<- "Column \"latitude\" not found." }
        } else { error <<- "Column \"scientificName\" not found." }
      } else {
        if (exists("commonname", where = photoList)){
          if (exists("latitude", where = photoList)){
            if (exists("longitude", where = photoList)){
              if (exists("date", where = photoList)){
                photoList <<- photoList %>%
                  mutate(date = ymd(date, quiet = TRUE))
                if (exists("time", where = photoList)){
                  photoList <<- photoList %>% select(commonname, latitude, longitude, date, time)
                  photoList <<- photoList %>%
                    #mutate(time = hms(time, quiet = TRUE)) %>% 
                    rename(Species = commonname) %>% 
                    mutate(Species = gsub("\\s*\\([^)]+\\)", "", Species))
                } else {
                  photoList <<- photoList %>% select(commonname, latitude, longitude, date) %>% 
                    rename(Species = commonname) %>% 
                    mutate(Species = gsub("\\s*\\([^)]+\\)", "", Species))
                }
              } else { error <<- "Column \"date\" not found." }
            } else { error <<- "Column \"longitude\" not found." }
          } else { error <<- "Column \"latitude\" not found." }
        } else { error <<- "Column \"commonName\" not found." }
      }
      if (error == ""){
        photoList <<- photoList %>%
          rename(GPSLongitude = longitude, GPSLatitude = latitude) %>% 
          mutate(YearMonthDay = as.POSIXct(date, format = "%Y:%m:%d"),
                 YearMonth = format(YearMonthDay, "%Y-%m"),
                 YearMonthDay = format(YearMonthDay, "%Y-%m-%d")) %>%
          arrange(YearMonthDay)
        
        photoList$GPSLongitude[is.na(photoList$GPSLongitude)] <<- NA_integer_
        photoList$GPSLongitude <<- as.numeric(photoList$GPSLongitude)
        photoList$GPSLatitude[is.na(photoList$GPSLatitude)] <<- NA_integer_
        photoList$GPSLatitude <<- as.numeric(photoList$GPSLatitude)
        if (any(is.na(photoList$GPSLongitude) | is.na(photoList$GPSLatitude))) {
          Warning_GPS <<- copy(photoList) %>% 
            filter((is.na(GPSLatitude)) | (is.na(GPSLongitude))) %>% 
            select(-YearMonth, -YearMonthDay) %>% 
            select(Species, date, GPSLatitude, GPSLongitude, everything())
        }
      } else {
        NoBirds_Modal()
      }
    }
    
    ####                                                                                        ####
    #                                        Data Analysis                                         #
    ####                                                                                        ####
    
    
    # Load the database for analysis
    BirdList <- readRDS(file = paste0(varPath,"/bin/Bi.rds")) %>% 
      arrange(desc(Rarity))
    
    library(stringdist) # Package to calculate Jaro–Winkler similarity
    
    # Match users' tags with the database & fill photoList with scientific names
    ### Function to look up English synonyms in BirdList
    match_scientificName <- function(species) {
      # Evaluate each column with an English name
      match <- amatch(tolower(species), tolower(BirdList$name), method = "jw")
      match_en1 <- amatch(tolower(species), tolower(BirdList$synEn1), method = "jw")
      match_en2 <- amatch(tolower(species), tolower(BirdList$synEn2), method = "jw")
      match_en3 <- amatch(tolower(species), tolower(BirdList$synEn3), method = "jw")
      match_en4 <- amatch(tolower(species), tolower(BirdList$synEn4), method = "jw")
      # Coalesce all the strings with results into one (e.g. NA 84 14 & 22 74 NA -> 22 84 14)
      matches <- coalesce(match, match_en1, match_en2, match_en3, match_en4)
      
      BirdList$scientificName[fifelse(matches > 0, matches, NA_integer_)]
    } 
    print("Beginning to match species")
    
    ## In case user's tags are in Latin
    if (input$Id000 == "tagsScientific") {
      inputScientific <<- TRUE
      inputRegional <<- ""
      inputCommon <<- FALSE
      
      if (csvFile == ""){
      photoList <<- photoList %>% # Is not a data table
        mutate(scientificName = if_else(tolower(Species) %in% tolower(BirdList$scientificName), Species, 
                                if_else(tolower(Species) %in% tolower(BirdList$synsLatin), 
                                        BirdList$scientificName[match(tolower(Species), tolower(BirdList$synsLatin))], 
                                        NA_character_)))
      } else {
      photoList$Species <<- photoList$scientificName
      photoList <<- photoList %>% # Not the most elegant way of having them technically "matched"
        mutate(scientificName = if_else(tolower(scientificName) %in% tolower(BirdList$scientificName), scientificName, 
                                if_else(tolower(scientificName) %in% tolower(BirdList$synsLatin), 
                                        BirdList$scientificName[match(tolower(scientificName), tolower(BirdList$synsLatin))], 
                                        NA_character_)))
      }
      
    ## In case of English tags
    ## Case insensitive & allows for ~one typo to be present in tags
    } else if (!is.null(input$Regional) && length(input$Regional) > 0 && input$Regional != "Select one") {
      photoList$scientificName <<- NA
      setDT(photoList)
      inputScientific <<- FALSE
      inputRegional <<- input$Regional
      print(input$Regional)
      
      photoList[, scientificName := fifelse(is.na(scientificName), match_scientificName(Species), NA_character_)]
      ## If the user chose to match tags with regional adjectives
      if (input$Regional == "Eurasian/European") {
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("eurasian ", Species)), scientificName)]
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("european ", Species)), scientificName)]
      } else if (input$Regional == "African/Somali") {
        # There are African/West African Thrush and Somali Thrush in Africa. Warn the user that we're assuming it's the African.
        if ("Thrush" %in% photoList$Species){
          Warning_AThrush <- 1
        }
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("african ", Species)), scientificName)]
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("somali ", Species)), scientificName)]
      } else if (input$Regional == "Asian/Indian") {
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("asian ", Species)), scientificName)]
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("indian ", Species)), scientificName)]
      } else if (input$Regional == "Philippine/Mindanao") {
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("Philippine ", Species)), scientificName)]
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("Mindanao ", Species)), scientificName)]
      } else {
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste(input$Regional, " ", Species)), scientificName)]
      }
      ## If name is not found, attempt to match names by adding "common", e.g. Magpie -> "Common Magpie"
      if (isTRUE(input$Common)){
        inputCommon <<- TRUE
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("common ", Species)), scientificName)]
      } else { inputCommon <<- FALSE }
    } else {
      ## If the user didn't choose any regional adjectives
      photoList$scientificName <<- NA
      setDT(photoList)
      inputScientific <<- FALSE
      inputRegional <<- ""
      photoList[, scientificName := fifelse(is.na(scientificName), 
                                            match_scientificName(Species), NA_character_)]
      ## If name is not found, attempt to match names by adding "common", e.g. Magpie -> "Common Magpie"
      if (isTRUE(input$Common)){
        inputCommon <<- TRUE
        photoList[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(paste("common ", Species)), scientificName)]
      } else { inputCommon <<- FALSE }
    } 
    
    print("Creating a table for unmatched photos")
    if (csvFile == ""){
      if (any(is.na(photoList$scientificName))) {
        Warning_noMatch <<- photoList %>%
          filter((is.null(scientificName) | is.na(scientificName))) %>% 
          select(-YearMonth, -YearMonthDay, -scientificName, -Zoo, -`Animal Class`) %>% 
          select(SourceFile, Species, DateTime, GPSLatitude, GPSLongitude, everything()) %>% 
          mutate(Error = "Tags could not be matched with any species in the database")
        # Warn the user about the possible mismatch
        if (Warning_AThrush == 1){
          thrush <<- photoList %>% filter(Species == "Thrush") %>% 
            select(-YearMonth, -YearMonthDay, -scientificName, -Zoo, -`Animal Class`) %>% 
            select(SourceFile, Species, DateTime, GPSLatitude, GPSLongitude, everything()) %>% 
            mutate(Error = "Thrush was assumed to be Turdus pelios. If that's not the case, please change the tag.")
          Warning_noMatch <<- rbind(Warning_noMatch, thrush)
        }
      }
    } else {
      Warning_noMatch <<- photoList %>%
        filter((is.null(scientificName) | is.na(scientificName))) %>% 
        select(-YearMonth, -date, -scientificName) %>% 
        select(Species, YearMonthDay, GPSLatitude, GPSLongitude, everything()) %>% 
        mutate(Error = "Tags could not be matched with any species in the database")
      if (Warning_AThrush == 1){
        thrush <<- photoList %>% filter(Species == "Thrush") %>% 
          select(-YearMonth, -date, -scientificName) %>% 
          select(Species, YearMonthDay, GPSLatitude, GPSLongitude, everything()) %>% 
          mutate(Error = "Thrush was assumed to be Turdus pelios. If that's not the case, please change the tag.")
        Warning_noMatch <<- rbind(Warning_noMatch, thrush)
      }
      if (inputScientific == TRUE){ photoList <<- photoList %>% select(-Species) }
    }
    
    print("Preparing warning tables")
    if (nrow(Warning_noTags) > 0 && (nrow(Warning_noMatch) > 0)) {
      Warning_noTags$GPSLatitude <- as.integer(Warning_noTags$GPSLatitude)
      Warning_noTags$GPSLongitude <- as.integer(Warning_noTags$GPSLongitude)
      Warning_Tags <<- full_join(Warning_noMatch, Warning_noTags) 
    } else if (nrow(Warning_noTags) > 0) {
      Warning_Tags <<- Warning_noTags 
    } else if (nrow(Warning_noMatch) > 0) {
      Warning_Tags <<- Warning_noMatch}
    if (nrow(Warning_GPS) > 0 & csvFile == ""){
    Warning_GPS <<- Warning_GPS %>% 
      filter(!SourceFile %in% Warning_Tags$SourceFile)
    }
  
    print("Filtering out NAs from photoList, sorting by Rating")
    if (exists("Rating", where = photoList)){
      photoList <<- photoList %>%
        filter(!(is.na(scientificName))) %>% 
        arrange(desc(Rating), desc(YearMonthDay)) # For photo matching, so the higher rating is matched
    } else {
      photoList <<- photoList %>%
        filter(!(is.na(scientificName))) %>% 
        arrange(desc(YearMonthDay))
    }
    print(photoList)
    
    if (exists("BirdList")) {
      # Assign "Discovered" to each species that the user has seen
      BirdList_Final <<- BirdList %>%
        mutate(Discovered = tolower(scientificName) %in% tolower(photoList$scientificName))
      
      if (csvFile == ""){
        # Replace photos with user photos, at user's request
        if (input$usePhotos) {
          inputUserPhotos <<- TRUE
          
          BirdList_Final[, Photo := fifelse(scientificName %in% photoList$scientificName, 
                                            photoList$SourceFile[match(scientificName, photoList$scientificName)], Photo)]
          BirdList_Final[, source := fifelse(scientificName %in% photoList$scientificName, 
                                             photoList$SourceFile[match(scientificName, photoList$scientificName)], source)]
          BirdList_Final[, rightsHolder := fifelse(scientificName %in% photoList$scientificName, "you", rightsHolder)]
        } else { inputUserPhotos <<- FALSE }
      }
      
      # Replace database names with user's names, if chosen (e.g. Eurasian Eagle Owl instead of Eurasian Eagle-owl)
      if (input$replaceNames == TRUE && input$Id000 != "tagsScientific") {
        inputReplaceNames <<- TRUE
        BirdList_Final <<- BirdList_Final %>%
          mutate(name = if_else(scientificName %in% photoList$scientificName, 
                                photoList$Species[match(scientificName, photoList$scientificName)], 
                        if_else(synsLatin %in% photoList$scientificName, 
                                photoList$Species[match(synsLatin, photoList$scientificName)], name)))
      } else if (input$Id000 == "tagsScientific") {
        photoList <<- photoList %>%
            mutate(Species = if_else(scientificName %in% BirdList_Final$scientificName, 
                                     BirdList_Final$name[match(scientificName, BirdList_Final$scientificName)], 
                             if_else(scientificName %in% BirdList_Final$synsLatin, 
                                     BirdList_Final$name[match(scientificName, BirdList_Final$synsLatin)], scientificName)))
        inputReplaceNames <<- FALSE 
      } else if (input$replaceNames == FALSE) {
        photoList <<- photoList %>%
          mutate(Species = if_else(scientificName %in% BirdList_Final$scientificName, 
                                   BirdList_Final$name[match(scientificName, BirdList_Final$scientificName)], 
                                   if_else(scientificName %in% BirdList_Final$synsLatin, 
                                           BirdList_Final$name[match(scientificName, BirdList_Final$synsLatin)], Species)))
        inputReplaceNames <<- FALSE 
      }
    }
    
    print("Merging names for Search function")
    Searchlist <<- data.table(name = c(BirdList_Final$name, BirdList_Final$scientificName, 
                                       BirdList_Final$synsLatin, BirdList_Final$synEn1, BirdList_Final$synEn2, 
                                       BirdList_Final$synEn3, BirdList_Final$synEn4)) %>% filter(name != "", name != is.na(name))
    
    # Create a shorter list from database with only the seen species
    Discovered <<- BirdList_Final %>% filter(Discovered == TRUE)
    
    
    ####                                                                                        ####
    #                                           Map Prep                                           #
    ####                                                                                        ####

    
    library(sf)
    
    if (nrow(Discovered) > 0){
    # Load abundances & probabilities
    Abundances <<- readRDS(file = paste0(varPath,"/bin/Abundances.rds")) %>% 
      mutate(Sqrt = if_else(is.na(Sqrt), 1, Sqrt)) %>% rename(scientificName = species)

    print("Loading leaflet_sf")
    leaflet_sf <- readRDS(file = paste0(varPath,"/bin/leaflet.rds"))
    
    # Calculate total amount of species *seen* per country
    userStats <- Abundances %>% 
      filter(scientificName %in% Discovered$scientificName) %>% 
      filter(Present == TRUE) %>% 
      count(countrycode, month) %>% 
      pivot_wider(names_from = "month", values_from = "n", names_prefix = "Seen") %>% 
      rename(Seen = Seen0, iso_a2_eh = countrycode) 
    userStats[is.na(userStats)] <- 0
    print("Added userStats")

    # st_as_sf to convert the data into something leaflet can understand
    leaflet_sf <- st_as_sf(left_join(leaflet_sf, userStats, by = join_by(iso_a2_eh))) 
    leaflet_sf[is.na(leaflet_sf)] <- 0
    leaflet_sf <- leaflet_sf %>% mutate(Ratio = round(Seen/Total, 3))
    print("Adding monthly stats to leaflet_sf")
    for (mnth in 1:12){
      n <- which(colnames(leaflet_sf) == mnth)
      seenm <- paste0("Seen", mnth)
      ratiom <- paste0("Ratio", mnth)
      leaflet_sf[[ratiom]] <- round(leaflet_sf[[seenm]]/leaflet_sf[[n]], 3)
    }
    leaflet_sf[is.na(leaflet_sf)] <- 0
    leaflet_sf <<- copy(leaflet_sf)
    userStats$country_name <- leaflet_sf$country_name[match(userStats$iso_a2_eh, leaflet_sf$iso_a2_eh)]
    userStats$Total <- leaflet_sf$Total[match(userStats$iso_a2_eh, leaflet_sf$iso_a2_eh)]
    userStats <- userStats %>% select(iso_a2_eh, country_name, Seen, Total)
    tempLeafset <- st_drop_geometry(leaflet_sf) %>% select(iso_a2_eh, country_name, Total)
    tempStats <- userStats %>% select(iso_a2_eh, country_name, Total)
    tempLeafset <- anti_join(tempLeafset, userStats)
    tempLeafset$Seen <- 0
    tempLeafset <- tempLeafset %>% select(iso_a2_eh, country_name, Seen, Total)
    userStats <- rbind(userStats, tempLeafset) %>% arrange(country_name)
    userStats <<- copy(userStats)
    
    # Print the list for debugging purposes
    print(photoList)
    print("Compare above with the warning table.")
    removeModal(session)
    remove_modal_gif()
    
    # Create user folder and save all objects
    if (!dir.exists(dirname(paste0(varPath,"/userData/Use.RData")))) {
      dir.create(paste0(varPath,"/userData")) }
    save(file = file.path(varPath,"/userData/Use.RData"),
         photoDir, csvFile, photoList, Searchlist, userStats, leaflet_sf, image_files, 
         BirdList_Final, Discovered, inputCommon, inputRegional, inputUserPhotos, inputScientific, 
         inputZoo, inputReplaceNames, Abundances)
    
    # Show the page
    shinyjs::show(id = "conditional")
    # Hide irrelevant buttons from the menu
    if (csvFile != ""){ 
      shinyjs::hide("refreshData")
      shinyjs::hide("reIndexData")
      shinyjs::hide("changeData")
      shinyjs::show("CSVreIndexData")
      shinyjs::show("CSVchangeData")
    } else {
      shinyjs::show("refreshData")
      shinyjs::show("reIndexData")
      shinyjs::show("changeData")
      shinyjs::hide("CSVreIndexData")
      shinyjs::hide("CSVchangeData")
    }
    
    
    ####                                                                                        ####
    #                                          Map Render                                          #
    ####                                                                                        ####
    
    
    createLeaflet()
    createSlider()
    gc()
    
    # In case of warnings, show the warning modal
    if (nrow(Warning_Tags) > 0 | nrow(Warning_GPS) > 0 | exists("deletedZooNum") && deletedZooNum > 0 ){
      Warning_Modal()
    }
    } else {
      if (length(image_files) == 0){
        error <<- "No photos found. Please choose another directory or add tags to your photos."
      } else {
        error <<- paste0(length(image_files), " images found, but none of them had identifiable tags.")
      }
      NoBirds_Modal()
    }
    }
  }
  
  # Colors for countries
  countries_map <- leafletProxy("map")
  mapPalette <- colorNumeric(
    palette = "Reds",
    domain = c(0, 1))
  
  # Colors for species
  proxy_map <- leafletProxy("map")
  speciesPalette <- colorNumeric(
    palette = "Greens", 
    #colorRampPalette(colors = c("#51c2c0", "#176872"))(n = 20),
    domain = c(0, 1), n = 20)
  speciesPaletteLowConf <- colorNumeric(
    palette = "Greys", 
    #colorRampPalette(colors = c("#ddf9d1", "#90c97b", "#3a9a24"))(n = 20),
    domain = c(0, 1), n = 20)
  
  # Polygon layer for main leaflet
  polygonFunction <- function(map, polygon, m){
    
    # Monthly filter
    if (isTRUE(exists("m") & m != 0)){
      Ratio <- paste0("Ratio", m)
      Seen <- paste0("Seen", m)
      mSeen <- leaflet_sf[[Seen]]
      m <- which(colnames(leaflet_sf) == m)
      mTotal <- leaflet_sf[[m]]
    } else { 
      Ratio <- "Ratio"
      mSeen <- leaflet_sf$Seen
      mTotal <- leaflet_sf$Total 
    }
    mapRatio <- mapPalette(leaflet_sf[[Ratio]])
    
    addPolygons(
      options = pathOptions(lineJoin = round, lineCap = round),
      group = "Main Map",
      map = map,
      data = polygon,
      stroke = TRUE,
      fillColor = ~ mapRatio,
      fillOpacity = 0.5,
      # Has to have random number included in order for it to work more than once in R
      popup = ~ paste("<strong style:'font-size:2em;'>", country_name, "</strong><br>", 
                      "Discovered: <a href='#' onclick='Shiny.onInputChange(\"button_discovered\",  Math.random())'>", 
                      mSeen, "</a>/<a href='#' onclick='Shiny.onInputChange(\"button_total\",  Math.random())'>", mTotal, 
                      "</a>"),
      color = "white",
      weight = 1,
      layerId = ~ iso_a2_eh,
      label = NULL,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#F28500",
        fillColor = "#ffb38a",
        fillOpacity = 0.01
      )
    )
    
  }
  
  # First leaflet render
  createLeaflet <- function(){
    # Set key for quicker data.table processing
    setkeyv(Abundances, c("scientificName", "countrycode", "month", "Present"))
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 2, maxZoom = 16, worldCopyJump = FALSE)) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addMiniMap(
          tiles = providers$Esri.WorldGrayCanvas,
          toggleDisplay = TRUE) %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="World Map",
          onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        polygonFunction(polygon = leaflet_sf, m = 0)
    })
  }
    
  # Leaflet render when useRData exists
  observe({
    if (exists("leaflet_sf")) {
      output$map <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 2, maxZoom = 16, worldCopyJump = FALSE)) %>%
          addProviderTiles("Esri.WorldGrayCanvas") %>%
          addMiniMap(
            tiles = providers$Stadia.StamenTonerLabels,
            width = 200, height = 200,
            zoomLevelOffset = -2,
            toggleDisplay = TRUE) %>%
          addEasyButton(easyButton(
            icon="fa-globe", title="World Map",
            onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
          setView(lng = 0, lat = 0, zoom = 2) %>%
          polygonFunction(polygon = leaflet_sf, m = 0)
      })
      welcomeModal()
      }
  })
  
  # For testing
  # observeEvent(input$rerenderMap, {
  #   proxy_map_main <- leafletProxy("map")
  #   proxy_map_main %>%
  # clearGroup doesn't work because it deletes underlying map. 
  # clearShapes does too... 
  #     clearShapes() %>%
  #     clearMarkers() %>%
  #     clearMarkerClusters() %>%
  #     polygonFunction(leaflet_sf)
  # })
  
  
  
  ####                                                                                        ####
  #                                          Map Filters                                         #
  ####                                                                                        ####
  
  
  
  # Data frame has to be reactive to render based on country click
  # Use get(input$map_shape_click$id) for a column name, e.g. US for accessing Abundances$US
  # Example: Abundances[get(input$map_shape_click$id) == "US"]
  selected_total <- reactive({
    # tidyverse alternative:
    # Abundances %>%
    #   filter(!!sym(input$map_shape_click$id) == TRUE) %>%
    #   select(scientificName) %>% 
    #   left_join(BirdList_Final) %>% 
    #   select(Family,name,scientificName,Rarity,redlistCategory,Photo,rightsHolder)
    country_filter <- Abundances[countrycode == input$map_shape_click$id & month == m & Present == TRUE
                                 ][, .(scientificName)][BirdList_Final, 
                                       .(Family, name, scientificName, Rarity, redlistCategory, Photo, source, 
                                         rightsHolder, count, Discovered), on = "scientificName", nomatch = 0]
    if (isTRUE(input$family_switch) | is.null(input$family_switch)) { 
              setorder(country_filter, Family, -Discovered, -Rarity) 
             } else { setorder(country_filter, -Discovered, -Rarity) }
    country_filter
  })
  selected_discovered <- reactive({
    # ^ ... tidyverse:
    #   filter(scientificName %in% Discovered$scientificName) %>%
    #   left_join(Discovered) %>%
    #   select(Family,name,scientificName,Rarity,redlistCategory,Photo,rightsHolder)
    country_filter <- Abundances[countrycode == input$map_shape_click$id & month == m & Present == TRUE
                                 ][, .(scientificName)][scientificName %in% Discovered$scientificName][Discovered, 
                                       .(Family, name, scientificName, Rarity, redlistCategory, Photo, source, 
                                                     rightsHolder, count), on = "scientificName", nomatch = 0]
    if (isTRUE(input$family_switch) | is.null(input$family_switch)) { 
                           setorder(country_filter, Family, -Rarity)
                          } else { setorder(country_filter, -Rarity) }
    country_filter
  })
  
  # "Show all" (and "discovered"/"total" on leaflet) trigger reaction
  total_filter <- reactive({
    # Define table filters based on the toggle switch value
    # Hide popover that might get stuck on toggle
    if (input$toggle_switch) {
      shinyjs::runjs("if ($('.popover').length) $('.popover').popover('hide');")
      selected_total()
    } else {
      shinyjs::runjs("if ($('.popover').length) $('.popover').popover('hide');")
      selected_discovered()
    }
  })
  
  # Click on Discovered # on the map
  observeEvent(input$button_discovered, {
    total_filter <- selected_discovered()
    updateSwitchInput(session, "toggle_switch", value = FALSE)
    countryModal(total_filter)
  })
  
  # Click on Total # on the map
  observeEvent(input$button_total, {
    total_filter <- selected_total()
    updateSwitchInput(session, "toggle_switch", value = TRUE)
    countryModal(total_filter)
  })
  
  
  
  ####                                                                                        ####
  #                                       Country Modal Prep                                     #
  ####                                                                                        ####
  
  
  
  # Transform each species name into a link with popover
  convertToLink <- function(species, scientificName, Rarity, redlistCategory, Photo, source, rightsHolder, count, index) {
    
    if (redlistCategory == "CR (PE)"){
      redlistCategory <- "CR.PE"
    }
    
    redlistColor <- data.frame(
      LC = c("Least Concern", "#405B98"),
      NT = c("Near Threatened", "#CD9A64"),
      VU = c("Vulnerable", "#CD9A00"),
      EN = c("Endangered", "#CD4700"),
      CR = c("Critically Endangered", "#CD3030"),
      `CR.PE` = c("Critically Endangered (Possibly Extinct)", "#CD3030"),
      DD = c("Data Deficient", "#C3C3C3")
    )
    redCat <- paste0("<a title='", redlistColor[redlistCategory][1, ], 
                     " (2022)'><span class='redlist'>", redlistCategory, "</span></a>")

    # Cut() refuses to assign my labels so we'll do it manually... and colors don't work as style, only as class...
    if (Rarity <= 0.3){ scarcity <- paste0('<div class="popoverTitleLeft scExSc">Extremely Scarce</div>')
    } else if (Rarity > 0.3 & Rarity <= 0.4){ scarcity <- paste0('<div class="popoverTitleLeft scHEl">Highly Elusive</div>')
    } else if (Rarity > 0.4 & Rarity <= 0.5){ scarcity <- paste0('<div class="popoverTitleLeft scEl">Elusive</div>')
    } else if (Rarity > 0.5 & Rarity <= 0.6){ scarcity <- paste0('<div class="popoverTitleLeft scVR">Very Rare</div>')
    } else if (Rarity > 0.6 & Rarity <= 0.7){ scarcity <- paste0('<div class="popoverTitleLeft scR">Rare</div>')
    } else if (Rarity > 0.7 & Rarity <= 0.8){ scarcity <- paste0('<div class="popoverTitleLeft scUnC">Uncommon</div>')
    } else if (Rarity > 0.8 & Rarity <= 0.9){ scarcity <- paste0('<div class="popoverTitleLeft scC">Common</div>')
    } else if (Rarity > 0.9 & Rarity <= 1){ scarcity <- paste0('<div class="popoverTitleLeft scUb">Ubiquitous</div>') }
    # Apply color to rarity text based on rarity
    color <- colorNumeric(palette = c("#9d1642", "#d53e4f", "#4393c3", "#2166ac"), domain = c(0, 1))(Rarity)
    
    # Apply boldness to scientific name
    if (scientificName %in% Discovered$scientificName){ 
      bold <- "font-weight: 600;"
    } else { bold <- "" }

    # Show photo
    if (is.na(rightsHolder)){
      spPhoto <- HTML(paste0("<img src='no-image.png' alt='Image not available' class='noPhoto'/>"))
    } else if (rightsHolder == "you"){
      spPhoto <- HTML(paste0("<a target='_blank' href='photos/", Photo, "'><img src='photos/", Photo, 
                             "' class='userPhoto'></a><br><div class='popoverRight userColor'>© you</div>"))
    } else {
      spPhoto <- HTML(paste0("<a target='_blank' href='", Photo, "'><img src='", Photo, "' class='imgPhoto'></a><br>
                             <div class='popoverRight'>© <a href='", source, "' target='_blank'>", rightsHolder, "</a></div>"))
    }
    
    # Combine all of the above into a link to return to renderTable
    link <- tags$a(href="#", style=paste0("font-size: 16px; color:", color, ";", bold), 
                   onclick=paste0("Shiny.setInputValue('species', '", scientificName, "');",
                                  "Shiny.setInputValue('species_English', '", species, "');",
                                  "Shiny.setInputValue('closeModal', true);",
                                  "if ($('.popover').length) { $('.popover').popover('hide'); }",
                                  ""), species) %>% 
            bsPopover(tag = ., HTML('<a title="Based on records of', count, 'individuals from 2011 to 2021">', scarcity, 
                                    '</a><div class="popoverCenter">', scientificName, '</div><div class="popoverTitleRight">', 
                                    redCat, '</div>'), spPhoto, click_inside = TRUE, html = TRUE, 
                                    bgcolor = redlistColor[redlistCategory][2, ], "right") # #405B98
    link
  }

  # Data table render for country modal
  output$filtered_table <- renderTable({
    
    totaldata <- total_filter()
    
    if (isTRUE(input$family_switch) | is.null(input$family_switch)){

      if (length(totaldata$name) < 1){
        
        mat <- "No observations"
        # Convert the matrix back to a data frame
        table_df <- as.data.table(c(sprintf("<b>%s</b>", mat)))
        #as.data.frame(c(sprintf("<b>%s</b>", family_name), "No observations", ""))
        table_df
        
      } else {
        
        # Create a list to hold separate tables for each Family
        table_list <- list()
        # Split the data by Family
        split_data <- split(totaldata, totaldata$Family)
        # Create a sub-table for each family
        create_family_table <- function(family_data) {
          num_rows <- ceiling(nrow(family_data) / 3)
          # Create an empty matrix with 3 columns
          mat <- matrix("", nrow = num_rows, ncol = 3)
          # Assign the links to the matrix with 3 columns
          for (i in 1:nrow(family_data)) {
            row <- ceiling(i / 3)
            col <- (i - 1) %% 3 + 1
            # Pass all the arguments to convertToLink function in order to process them and return links with popovers
            mat[row, col] <- convertToLink(family_data$name[i], family_data$scientificName[i], family_data$Rarity[i], 
                                           family_data$redlistCategory[i], family_data$Photo[i], family_data$source[i], 
                                           family_data$rightsHolder[i], family_data$count[i], i) %>% 
                                                                                   as.character() #required for pop-ups
          }
          mat
        }
        
        table_list <- lapply(split_data, create_family_table)
        
        # Add sub-headers to Family sub-tables with Family name & Totals
        table_list <- lapply(names(table_list), function(family_name) {
          family_table <- table_list[[family_name]]
          if (nrow(family_table) > 0) {
            if ( exists("Discovered", where = split_data[[family_name]]) ) {
                                                        # Add family totals to the right side of the table
                                family_discovered <- sum(as.logical(split_data[[family_name]]$Discovered))
                          family_totals <- paste0(family_discovered, "/", nrow(split_data[[family_name]]))
                                                                                                  } else { 
                                                        family_totals <- nrow(split_data[[family_name]]) }
            
            family_table <- rbind(c(sprintf("<div style='font-size: 15px;'><b>%s</b></div>", family_name), "", 
                                    sprintf("<div style='font-size: 12px; padding-top: 9px; text-align:right;'>%s</div>", 
                                            family_totals)), family_table)
          }
          for (i in 1:nrow(family_table)) { # Add some styling to the totals
            for (u in 1:ncol(family_table)) { # &nbsp;
              if (family_table[i, u] != sprintf("<div style='font-size: 15px;'><b>%s</b></div>", family_name)) {
                family_table[i, u] <- sprintf("<div style='padding-left: 10px;'>%s</div>", family_table[i, u])  
              }
            }
          }
          family_table
        })
        
        combined_table <- do.call(rbind, table_list)
        combined_table
      }
      
    } else {
      
      num_rows <- ceiling(length(totaldata$name) / 3)
      mat <- matrix("", nrow = num_rows, ncol = 3)
      if (length(totaldata$name) < 1){
        mat <- "No observations"
        table_df <- as.data.table(c(sprintf("<b>%s</b>", mat)))
      } else {
        for (i in 1:length(totaldata$name)) {
          row <- ceiling(i / 3)
          col <- (i - 1) %% 3 + 1
          mat[row, col] <- convertToLink(totaldata$name[i], totaldata$scientificName[i], totaldata$Rarity[i], 
                                         totaldata$redlistCategory[i], totaldata$Photo[i], totaldata$source[i], 
                                         totaldata$rightsHolder[i], totaldata$count[i], i) %>% as.character()
        }
        table_df <- as.data.table(mat)
        }
      table_df
    }
  }, sanitize.text.function = function(x) x, include.rownames = FALSE,include.colnames = FALSE)
  
  
  
  ####                                                                                        ####
  #                                         Country Modal                                        #
  ####                                                                                        ####
  
  

  # Modal to show on country click
  countryModal <- function(total_filter) {
    # Hide any popover that might have gotten stuck from previous modals
    gc()
    shinyjs::runjs("if ($('.popover').length) $('.popover').popover('hide');")
    
    # Get flags
    if ((grepl("US-", input$map_shape_click$id))) {
      flag_url <- paste0("https://flagcdn.com/h20/", tolower(input$map_shape_click$id), ".png")
    } else {
      input_country_code <- substr(input$map_shape_click$id, 1, 2)
      flag_url <- paste0("https://flagcdn.com/h20/", tolower(input_country_code), ".png")
    }
    # Get country name
    countryName <- leaflet_sf %>%
      filter(iso_a2_eh == input$map_shape_click$id) %>% select(country_name)

    showModal(modalDialog(
      tags$div(style = "position: relative;",
               tags$h2(style = "text-align: center; margin: 0;",
                       HTML(paste0('<img src="', flag_url, '" height="24"> ', countryName$country_name[1])),
               ),
               tags$div(style = "position: absolute; top: 0; right: 0;",
                        actionButton(label = "", class = "closeButton", inputId = "closeModal", 
                                     icon = icon(name = "xmark", lib = "font-awesome")))),
      tags$br(),
      tags$br(),
      tags$div(style="width:100%; margin:0 auto;
               display:inline-block; padding-left: 5px;",
               tags$div(style = "float: left; width: 33%; padding-top: 4px;",
                        materialSwitch(
                          inputId = "family_switch",
                          label = "Group by Family", 
                          value = isTRUE(input$family_switch),
                          status = "primary"
                        )),
               tags$div(style = "float: left; width: 33%; text-align:center;",
                        switchInput(
                          inputId = "toggle_switch",
                          label = "Show All",
                          onLabel = "<i class=\"fa fa-check\"></i>",
                          offLabel = "<i class=\"fa fa-xmark\"></i>",
                          onStatus = "primary", 
                          size = "small",
                          labelWidth = "60px",
                          value = isTRUE(input$toggle_switch) # Initial value
                        ))
      ),
      tags$p(),
      tableOutput("filtered_table")
    ))
  }
  
  # Nested modal 
  # tags$div(
  #   id = "nestedModal",
  #   style = "display: none;",
  #   tags$h4("Nested Modal"),
  #   p("This is a nested modal dialog.")
  # ),
  # actionButton("openNestedModal", "Open Nested Modal")
  
  
  
  ####                                                                                        ####
  #                                          Month Filter                                        #
  ####                                                                                        ####
  
  

  observeEvent(input$monthFilter, {
    # Use startUp variable to prevent firing of the filter at launch
    if (isFALSE(startUp)) {
      
      if (input$monthFilter == "Year-round"){
        m <<- 0
      } else {
        m <<- which(months_n == input$monthFilter)
      }
      
      if (inpSpecies != ""){ # If filtered by species,  re-draw the distribution map
        filteredAbundances <- Abundances %>% filter(month == m, scientificName == inpSpecies, Sqrt >= 0.033)
        sf_species_distribution <- leaflet_sf %>% filter(iso_a2_eh %in% filteredAbundances$countrycode) %>% 
          mutate(Sqrt = if_else(iso_a2_eh %in% filteredAbundances$countrycode, 
                                filteredAbundances$Sqrt[match(iso_a2_eh, filteredAbundances$countrycode)], 0)) %>% 
          mutate(Probability = if_else(iso_a2_eh %in% filteredAbundances$countrycode, 
                                       filteredAbundances$Probability[match(iso_a2_eh, filteredAbundances$countrycode)], NA)) %>% 
          mutate(nCountry = if_else(iso_a2_eh %in% filteredAbundances$countrycode, 
                                    filteredAbundances$n[match(iso_a2_eh, filteredAbundances$countrycode)], NA))
        print(sf_species_distribution$Sqrt)
        SpeciesMaps(filtered_data, sf_species_distribution)
        
      } else { # Otherwise, redraw the country map
        proxy_map_main <- leafletProxy("map")
        proxy_map_main %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearMarkerClusters() %>%
          polygonFunction(polygon = leaflet_sf, m = m)
      }
      
    }
    startUp <<- FALSE
  })

  
  
  ####                                                                                        ####
  #                                            Timeline                                          #
  ####                                                                                        ####
  
  
  
  # Render function for timeline (won't work in UI since it loads before photoList)
  createSlider <- function() {
    output$leafletSlider <- renderUI({
      photoList <<- photoList %>% arrange(YearMonth)
      sliderTextInput(
        inputId = "TimeLine",
        label = NULL,
        grid = TRUE,
        force_edges = FALSE,
        width = "100%",
        choices = unique(photoList$YearMonth),
        selected = photoList$YearMonth[nrow(photoList)]
    )}
  )}
  
  # After PhotoList is loaded, render timeline
  observe({
    if (exists("photoList") && nrow(photoList) > 1){
      createSlider()
  }})
  
  # 1+1s delay so the slider doesn't trigger as you're scrolling
  inputTimeLine <- reactive({
    input$TimeLine
  }) %>% throttle(1000) %>% debounce(1000)
  
  observeEvent(inputTimeLine(), {
    
    if (exists("photoList") && "YearMonth" %in% names(photoList) && input$TimeLine != "December" && 
        input$TimeLine != photoList$YearMonth[length(photoList$YearMonth)]) {
      
      # Temporary objects to store original data
      if (exists("photoList_unf") & exists("Discovered_unf")) {
        photoList <<- photoList_unf
        Discovered <<- Discovered_unf
      } else {
        photoList_unf <<- photoList
        Discovered_unf <<- Discovered}
      photoList <<- photoList %>%
        filter(YearMonth <= input$TimeLine)
      Discovered <<- Discovered %>% filter(scientificName %in% photoList$scientificName)
      
      # Jump to map if on charts panel (to force update on the charts)
      updateRadioGroupButtons(session, "bodyTab", selected = "map")

      # Calculate total amount of species *seen* per country
      userStats <- Abundances %>% 
        filter(scientificName %in% Discovered$scientificName) %>% 
        filter(Present == TRUE) %>% 
        count(countrycode, month) %>% 
        pivot_wider(names_from = "month", values_from = "n", names_prefix = "Seen") %>% 
        rename(Seen = Seen0, iso_a2_eh = countrycode) 
      userStats[is.na(userStats)] <- 0
      print("Added userStats")
      
      # st_as_sf to convert the data into something leaflet can understand
      leaflet_sf <- readRDS(file = paste0(varPath,"/bin/leaflet.rds"))
      leaflet_sf <- st_as_sf(left_join(leaflet_sf, userStats, by = join_by(iso_a2_eh))) 
      leaflet_sf[is.na(leaflet_sf)] <- 0
      leaflet_sf <- leaflet_sf %>% mutate(Ratio = round(Seen/Total, 3))
      print("Adding monthly stats to leaflet_sf")
      for (mnth in 1:12){
        n <- which(colnames(leaflet_sf) == mnth)
        seenm <- paste0("Seen", mnth)
        ratiom <- paste0("Ratio", mnth)
        leaflet_sf[[ratiom]] <- round(leaflet_sf[[seenm]]/leaflet_sf[[n]], 3)
      }
      leaflet_sf[is.na(leaflet_sf)] <- 0
      leaflet_sf <<- copy(leaflet_sf)
      userStats$country_name <- leaflet_sf$country_name[match(userStats$iso_a2_eh, leaflet_sf$iso_a2_eh)]
      userStats$Total <- leaflet_sf$Total[match(userStats$iso_a2_eh, leaflet_sf$iso_a2_eh)]
      userStats <- userStats %>% select(iso_a2_eh, country_name, Seen, Total)
      tempLeafset <- st_drop_geometry(leaflet_sf) %>% select(iso_a2_eh, country_name, Total)
      tempStats <- userStats %>% select(iso_a2_eh, country_name, Total)
      tempLeafset <- anti_join(tempLeafset, userStats)
      tempLeafset$Seen <- 0
      tempLeafset <- tempLeafset %>% select(iso_a2_eh, country_name, Seen, Total)
      userStats <- rbind(userStats, tempLeafset) %>% arrange(country_name)
      userStats <<- copy(userStats)
      
      print("Updating the map")
      proxy_map_main <- leafletProxy("map")
      proxy_map_main %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        polygonFunction(leaflet_sf, m = 0)
    }
  })
  
  
  
  ####                                                                                        ####
  #                                        Distribution Map                                      #
  ####                                                                                        ####
  
  
  
  observe({
    if (exists("Searchlist")) {
    updateSelectizeInput(session = session, inputId = "species", 
                         choices = Searchlist$name, server = TRUE, selected = character(0))}
  })
  
  speciesEnglishVar <- ""
  
  observeEvent(c(input$species, input$species_English), {
    if (!is.null(input$species) && input$species != "") {
      # To ensure that condition does not get executed when using pickerInput, since input$species_English stays unchanged after execution
      if (!is.null(input$species_English) && speciesEnglishVar != input$species_English){
        updateSelectizeInput(session = session, inputId = "species", server = TRUE, 
                             options = list(placeholder = input$species_English), 
                             choices = Searchlist$name, selected = character(0))
        speciesEnglishVar <<- input$species_English # Has to be global var
      } else {
      updateSelectizeInput(session = session, inputId = "species", server = TRUE, 
                           options = list(placeholder = input$species), 
                           choices = Searchlist$name, selected = character(0))
      }

      # Allow filtering regardless of the language it's typed in
      if (input$species %in% BirdList_Final$name){ # Check if English
        inpSpecies <<- BirdList_Final$scientificName[match(input$species, BirdList_Final$name)]
      } else if (input$species %in% BirdList_Final$scientificName){
        inpSpecies <<- input$species
      } else if (input$species %in% BirdList_Final$synsLatin){
        inpSpecies <<- BirdList_Final$scientificName[match(input$species, BirdList_Final$synsLatin)]
      } else if (input$species %in% c(BirdList_Final$synEn1, BirdList_Final$synEn2, BirdList_Final$synEn3, BirdList_Final$synEn4)) {
        if (input$species %in% BirdList_Final$synEn1){
          inpSpecies <<- BirdList_Final$scientificName[match(input$species, BirdList_Final$synEn1)]
        } else if (input$species %in% BirdList_Final$synEn2){
          inpSpecies <<- BirdList_Final$scientificName[match(input$species, BirdList_Final$synEn2)]
        } else if (input$species %in% BirdList_Final$synEn3){
          inpSpecies <<- BirdList_Final$scientificName[match(input$species, BirdList_Final$synEn3)]
        } else if (input$species %in% BirdList_Final$synEn4){
          inpSpecies <<- BirdList_Final$scientificName[match(input$species, BirdList_Final$synEn4)]
        }
      }
      
      # Make it global so the month filter has access to it
      filtered_data <<- photoList %>%
        filter(scientificName == inpSpecies)
      # filter(if (input$species %in% BirdList_Final$scientificName) scientificName == input$species else name == input$species)
      
      filteredAbundances <- Abundances %>% filter(month == m, scientificName == inpSpecies, Sqrt >= 0.033)
      sf_species_distribution <<- leaflet_sf %>% filter(iso_a2_eh %in% filteredAbundances$countrycode) %>% 
        mutate(Sqrt = if_else(iso_a2_eh %in% filteredAbundances$countrycode, 
                              filteredAbundances$Sqrt[match(iso_a2_eh, filteredAbundances$countrycode)], 0)) %>% 
        mutate(Probability = if_else(iso_a2_eh %in% filteredAbundances$countrycode, 
                                     filteredAbundances$Probability[match(iso_a2_eh, filteredAbundances$countrycode)], NA)) %>% 
        mutate(nCountry = if_else(iso_a2_eh %in% filteredAbundances$countrycode, 
                                  filteredAbundances$n[match(iso_a2_eh, filteredAbundances$countrycode)], NA))
      SpeciesMaps(filtered_data, sf_species_distribution)
    }
  })
  
  # Leaflet for distribution map
  SpeciesMaps <- function(filtered_data, sf_species_distribution) {
    proxy_map <- leafletProxy("map")
    
    if (nrow(sf_species_distribution) > 0) {
      # Hide all custom buttons except the "zoom out" on leaflet
      shinyjs::runjs('$(".easy-button-container:not(:first)").hide();')
      
      sf_species_distribution <- sf_species_distribution %>%
        mutate(confidence = if_else(nCountry < 1000, 
                                    "<font color='red'><b>Low confidence!<br>< 1 000 total observations here (for all species)</b><br>", 
                            if_else(nCountry < 5000, 
                                    "<font color='DarkRed'>Low confidence!<br>< 5 000 total observations here (for all species)<br>", 
                            if_else(nCountry < 10000, 
                                    "<font color='DarkRed'>Low confidence!<br>< 10 000 total observations here (for all species)<br>",
                                    "")))) # Alternatively, use confidence interval for all species obs (824147555 in 2011-2021)
      
      # Find the place with the biggest abundance
      biggestSqrt <- sf_species_distribution[sf_species_distribution$Sqrt == max(sf_species_distribution$Sqrt), ][1, ]
      # Get bbox for the region with biggest abundance
      bbox <- st_bbox(st_union(biggestSqrt))
      
      # In case of photo folder import, show photo on pop-up. Otherwise, only show the date of observation
      if (csvFile == ""){
        markerPopup <- paste0("<div class='polaroid'><a href='photos/", filtered_data$SourceFile, "' target='_blank'>
                              <img src='photos/", filtered_data$SourceFile, "'/></a><p>", filtered_data$DateTime, "</p></div>")
      } else if (exists("time", where = filtered_data)){
        markerPopup <- paste0("<div class='polaroid'>", filtered_data$YearMonthDay, ", ", filtered_data$time, "</div>")
      } else {
        markerPopup <- paste0("<div class='polaroid'>", filtered_data$YearMonthDay, "</div>")
      }
      
      proxy_map %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        addEasyButton(easyButton(id = "btnAbundant",
          icon="fa-crosshairs", title="Go To Most Abundant",
          onClick = JS(paste0("function(btn, map) { ",
                              "var bounds = L.latLngBounds([", bbox[2], ", ", bbox[1], "], [", bbox[4], ", ", bbox[3], "]);",
                              "map.fitBounds(bounds); }")))) %>% 
        addMarkers(
          options = markerOptions(riseOnHover = TRUE),
          icon = makeIcon(
            iconUrl = paste0(varPath,"/photo_marker.png"),
            iconWidth = 36, iconHeight = 55,
            iconAnchorX = 18, iconAnchorY = 46,
          ),
          lng = filtered_data$GPSLongitude,
          lat = filtered_data$GPSLatitude,
          popup = markerPopup,
          clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE, 
                                                # Marker clustering
                                                iconCreateFunction=JS("function (cluster) {
                                                  var childCount = cluster.getChildCount();
                                                  var c = ' marker-cluster-';
                                                  if (childCount < 100) {
                                                    c += 'large';
                                                  } else if (childCount < 1000) {
                                                    c += 'medium';
                                                  } else {
                                                    c += 'small';
                                                  }
                                                  return new L.DivIcon({ html: '<div><span>' 
                                                  + childCount + '</span></div>', className: 
                                                  'marker-cluster' + c, iconSize: new L.Point(40, 40) });
                                                                      }"))) %>%
        addPolygons(
          options = pathOptions(lineJoin = round, lineCap = round),
          group = "Proxy Map",
          data = sf_species_distribution,
          popup = ~ paste("<strong style:'font-size:2em;'>", country_name, 
                          "</strong><br>", confidence, "Every ", Probability, " reported bird</font>"), #bird encounters
          stroke = TRUE,
          fillColor = ~ ifelse(confidence != "" & Sqrt > 0.5, speciesPaletteLowConf(Sqrt), speciesPalette(Sqrt)), #"#B96148", #d0e7e8
          fillOpacity = 0.6,
          layerId = ~ iso_a2_eh, # Required for pop-ups to work independently of one another
          color = "#023020",     # Outline color of the polygons
          weight = 0.7,          # Outline weight
          label = NULL,
          # colors: D0CFD4 ; B96148 ; 69385c
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#023020",
            fillColor = "#e64500",
            fillOpacity = 0.01
          )
        )
    } else {
      m <<- 0
      updatePickerInput(session = session, inputId = "monthFilter", selected = "Year-round")
      updateSelectizeInput(session = session, inputId = "species", server = TRUE, 
                           options = list(placeholder = inpSpecies), choices = Searchlist$name, selected = inpSpecies)
      sendSweetAlert(
        session = session,
        title = "No records",
        text = "There are no records of this bird this month. Switching to year-round.",
        type = "error"
      )
    }
  }
  
  # Clear species filter and go back to the main map
  observeEvent(input$clearspecies, {
    inpSpecies <<- ""
    updateSelectizeInput(session = session, inputId = "species", options = list(placeholder = ""), 
                         choices = Searchlist$name, server = TRUE, selected = character(0))
    proxy_map_main <- leafletProxy("map")
    proxy_map_main %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      polygonFunction(polygon = leaflet_sf, m = m)
    # Hide "Most Abundant" button. If using just shiny::hide, then CSS styling remains 
    shinyjs::runjs('$(".easy-button-container:not(:first)").hide();') 
    gc()
  })
  
  
  
  ####                                                                                        ####
  #                                            Charts                                            #
  ####                                                                                        ####
  
  
  
  # Histogram function
  histogram <- function(h_data, h_title) {
    # h_data <- h_data %>% mutate(scarcity = if_else(Rarity <= 0.3, "Extremely Scarce", 
    # if_else(Rarity > 0.3 & Rarity <= 0.4, "Highly Elusive", if_else(Rarity > 0.4 & Rarity <= 0.5, "Elusive", 
    # if_else(Rarity > 0.5 & Rarity <= 0.6, "Very Rare", if_else(Rarity > 0.6 & Rarity <= 0.7, "Rare", 
    # if_else(Rarity > 0.7 & Rarity <= 0.8, "Uncommon", if_else(Rarity > 0.8 & Rarity <= 0.9, "Common", 
    # if_else(Rarity > 0.9 & Rarity <= 1, "Ubiquitous", "Extremely Scarce")))))))))
    
    hgram <- ggplot(h_data, aes(x = Rarity, fill = Discovered)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Arial"),
            legend.position = c(0.05, 0.95), 
            legend.justification = c(0, 1), 
            legend.margin = margin(t = -10, r = 0, b = 0, l = 0)) +
      # #facet_grid(rows = vars(fam), cols = vars(), scales = "free") +
      # facet_wrap(. ~ fam) +
      geom_histogram(binwidth = 0.05, color = "#000000", size = 0.2) +
      scale_x_continuous(
        limits = c(-0.05, 1.05),
        breaks = c(0, 0.2, seq(0.4, 1, by = 0.2)), 
        labels = c("Extremely Scarce", "Highly Elusive", "Elusive", "Rare", "Common", "Ubiquitous")) +
      scale_fill_manual(values = c("#2F4554", "#C23531"),
                        labels = c("UNDISCOVERED", "DISCOVERED")) + # Only works in ggplot; gets overwritten by plotly
      labs(x = "Ubiquity",
           y = "Unique Species",
           fill = NULL,
           title = h_title)
    
    ggplotly <- ggplotly(hgram) %>% 
      layout(dragmode=FALSE, hovermode = "x", 
             xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE), 
             legend = list(x = 0, y = 1, xanchor = "left", yanchor = "top")) %>% 
      config(editable = FALSE, scrollZoom = FALSE,
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "lasso2d", "zoomIn2d", "zoomOut2d", 
                                        "hoverClosestCartesian", "autoScale2d", "resetScale2d", 
                                        "hoverClosestCartesian", "hoverCompareCartesian")) #"select2d", 
             
    # Fix plotly's assholery that I spent hours coming up with
    ggplotly$x$data[[1]]$name <- "UNDISCOVERED"
    ggplotly$x$data[[2]]$name <- "SEEN"
    ggplotly$x$data[[1]]$meta <- "Undiscovered"
    ggplotly$x$data[[2]]$meta <- "Seen"
    ggplotly %>% 
      style(hovertemplate = paste0( "%{meta}: <b>%{y:,.0f}</b><br>",
                                    "<extra></extra>" )) # Get rid of category name in hover
  }
  
  observeEvent(input$bodyTab, {
    if (input$bodyTab == "bar"){
      shinyjs::hide(id = "bodyMap")
      shinyjs::hide(id = "search")
      shinyjs::show(id = "bodyCharts")
      
      library(highcharter)
      theme_set(theme_bw()) # For ggplot
      # Calendar and treeMap prep
      
      tempPhotoList <- separate(photoList, YearMonthDay, sep = "-", into = c("year", "month", "day"))
      
      ####                                                                                    ####
      #                                          Histogram                                       #
      ####                                                                                    ####
      
      BirdList_Nocturnal <<- BirdList_Final %>% filter(nocturnal == TRUE)
      BirdList_Terrestrial <<- BirdList_Final %>% filter(Terrestrial == TRUE)
      BirdList_Marine <<- BirdList_Final %>% filter(Marine == TRUE)
      BirdList_Freshwater <<- BirdList_Final %>% filter(Freshwater == TRUE)
      BirdListNocturnal <<- copy(BirdList_Nocturnal)
      BirdListTerrestrial <<- copy(BirdList_Terrestrial)
      BirdListMarine <<- copy(BirdList_Marine)
      BirdListFreshwater <<- copy(BirdList_Freshwater)
      
      output$histogram1 <- renderPlotly({
        histogram(BirdListNocturnal, "Nocturnal Species")
      })
      output$histogram2 <- renderPlotly({
        histogram(BirdListTerrestrial, "Terrestrial Species")
      })
      output$histogram3 <- renderPlotly({
        histogram(BirdListMarine, "Marine Species")
      })
      output$histogram4 <- renderPlotly({
        histogram(BirdListFreshwater, "Freshwater Species")
      })
      
      # Filter and update if realms are chosen
      observeEvent(input$realms, {
        if (input$bodyTab == "bar"){
          
          selected_realms <- input$realms
          
          if ("Worldwide" %in% selected_realms & length(selected_realms) == 1 || length(selected_realms) == 0) {
            BirdListNocturnal <<- BirdList_Nocturnal
            BirdListTerrestrial <<- BirdList_Terrestrial
            BirdListMarine <<- BirdList_Marine
            BirdListFreshwater <<- BirdList_Freshwater
          } else if (length(selected_realms > 0)) {
            if ("Worldwide" %in% selected_realms){
              selected_realms <- selected_realms[-which(selected_realms == "Worldwide")]
            }
            BirdListNocturnal <<- BirdList_Nocturnal[, .SD[rowSums(.SD[
                                                      , selected_realms, with = FALSE] == TRUE) > 0, ]]
            BirdListTerrestrial <<- BirdList_Terrestrial[, .SD[rowSums(.SD[
                                                      , selected_realms, with = FALSE] == TRUE) > 0, ]]
            BirdListMarine <<- BirdList_Marine[, .SD[rowSums(.SD[
                                                      , selected_realms, with = FALSE] == TRUE) > 0, ]]
            BirdListFreshwater <<- BirdList_Freshwater[, .SD[rowSums(.SD[
                                                      , selected_realms, with = FALSE] == TRUE) > 0, ]]
          }
          
          output$histogram1 <- renderPlotly({
            histogram(BirdListNocturnal, "Nocturnal Species")
          })
          output$histogram2 <- renderPlotly({
            histogram(BirdListTerrestrial, "Terrestrial Species")
          })
          output$histogram3 <- renderPlotly({
            histogram(BirdListMarine, "Marine Species")
          })
          output$histogram4 <- renderPlotly({
            histogram(BirdListFreshwater, "Freshwater Species")
          })
          
        }
      })
      
      
      ####                                                                                    ####
      #                                       General Table                                      #
      ####                                                                                    ####
      
      
      userStats <- userStats %>% 
        mutate(Ratio = round(Seen / Total * 100, 2)) %>% arrange(desc(Ratio))
      userStats <- userStats %>% 
        select(country_name, Seen, Ratio, Total, iso_a2_eh) %>% 
        rename(Region = country_name, `%` = Ratio)
      
      # DT
      output$regionsTable <- renderDT({
        
        dt <- datatable(userStats,
                        tags$style(HTML("#x1 td {
                                          padding-left: 0;
                                          padding-right: 0;
                                         }")),
                        rownames = FALSE,
                        options = list(
                          dom = 'frtp', # Remove information panel
                          columnDefs = list(
                            list(width = "40%", targets = 0),                     # name
                            list(width = "10%", targets = 1, searchable = FALSE), # Seen
                            list(targets = 2, searchable = FALSE,                    # %
                                 render = JS(tablebars), className = "dt-center"),
                            list(width = "10%", targets = 3, searchable = FALSE, # Total
                                 className = "dt-left"),
                            list(targets = 4, visible = FALSE, searchable = TRUE) # iso2
                          )
                        )
                        )
        # Apply semi-bold font face to the first column
        dt %>% formatStyle(fontWeight = 500, columns = 1)
        
      })
      
      
      ####                                                                                    ####
      #                                         Line Chart                                       #
      ####                                                                                    ####
      
      
      MonthlyStats <- photoList %>% arrange(YearMonth)
      tempStats <- MonthlyStats %>% count(YearMonth) %>% 
        rename(`Total observations` = n) %>% arrange(YearMonth)
      MonthlyStats <- MonthlyStats[!duplicated(MonthlyStats[,c('scientificName')]),]
      MonthlyStats <- MonthlyStats %>% count(YearMonth) %>% rename(`Unique Species` = n)
      
      MonthlyStats <- full_join(MonthlyStats, tempStats) %>% arrange(YearMonth)
      MonthlyStats[is.na(MonthlyStats)] <- 0
      for (i in 2:nrow(MonthlyStats)){
        MonthlyStats$`Unique Species`[i] <- MonthlyStats$`Unique Species`[i] + 
                                            MonthlyStats$`Unique Species`[i-1]
        MonthlyStats$`Total observations`[i] <- MonthlyStats$`Total observations`[i] + 
                                                MonthlyStats$`Total observations`[i-1]
      }
      
      MonthlyStats <- pivot_longer(MonthlyStats, cols = 2:3, names_to = "type", values_to = "n")
      
      visibleChart <- MonthlyStats %>% filter(type == "Unique Species")
      hiddenChart <- MonthlyStats %>% filter(type == "Total observations")
      
      output$chartLine <- renderHighchart({
        hc <- hchart(visibleChart, "line", hcaes(x = YearMonth, y = n, group = type), 
                     color = "#C23531", showEmpty = TRUE) %>%
          # Hide number of total observations by default
          hc_add_series(hiddenChart, "line", hcaes(x = YearMonth, y = n, group = type), 
                        color = "#2F4554", visible = FALSE) %>% 
          hc_plotOptions(
                        line = list(
                          lineWidth = 4,  # Adjust the width of the lines
                          marker = list(
                            radius = 5, 
                            symbol = "circle",
                            fillColor = "#fff",  # Change the color of the markers
                            lineWidth = 2,        # Outline width
                            lineColor = "#000000"  # Outline color
                          )
                        )) %>%
          # hc_add_theme(hc_theme(
          #   chart = list(
          #     backgroundColor = hex_to_rgba("white", 0.975),
          #     divBackgroundImage = "birdmapp.png"
          #   ))) %>%
  #         hc_chart(events = list(load = JS("function() {
  #         var chart = this;
  #         var imgWidth = chart.plotWidth * 1;  
  #         var imgHeight = chart.plotHeight * 1;  
  #         var imgX = chart.plotWidth / 2;  // Center 
  #         var imgY = chart.plotHeight / 2;  // Center 
  # 
        #   this.renderer.image('birdmapp.png', imgX, imgY, imgHeight)
        #         .css({
        #       'opacity': 0.05  // Set opacity to 0.1 (10%)
        #         })
        #       .add();
#           }"))) %>%
          hc_xAxis(title = list(text = "")) %>%  # Remove x title
          hc_yAxis(title = list(text = ""),
            # Make sure the numbers are written out instead of being "1k", "2k"...
                   labels = list(
                     formatter = JS("function() { return this.value; }")
                   )) %>% 
          # Align the legend on top
          hc_legend(layout = "horizontal", align = "center", verticalAlign = "top") 
        hc
      })
      
      # In plotly
      # chartLine <- pivot_longer(MonthlyStats, cols = c("Unique", "Total"), names_to = "photos", values_to = "n")
      # 
      # line <- plotly::plot_ly(data = chartLine,x = ~YearMonth,
      #                         y = ~Unique,name = "Unique Species",
      #                         type = "scatter",mode = "lines+markers",
      #                         line=list(width=7,color="#C23531")) %>%
      #   add_trace(y = ~Total, name = "Total Observations",mode = "lines+markers",
      #             line = list(width = 7, color="#2F4554")) %>%
      #   config(modeBarButtonsToRemove = c("zoom2d", "select2d", "zoomIn2d", 
      # "zoomOut2d", "pan2d", "lasso2d", "hoverClosestCartesian", "autoScale2d", 
      # "resetScale2d", "hoverCompareCartesian"), editable = FALSE, scrollZoom = FALSE) %>% 
      #   layout(title="Customized Multiline Plot using Plotly",hovermode = "x unified", 
      # legend = list(orientation = "h",   # show entries horizontally
      #               xanchor = "center",  # use center of legend as anchor
      #               x = 0.5))
      
      
      ####                                                                                    ####
      #                                         Calendar                                         #
      ####                                                                                    ####
      
      
      calendarList <- tempPhotoList %>% count(month, day)
      
      output$calendar <- renderPlotly({
        calendar <- readRDS("bin/calenda.rds") # Read the file with formatted table of a year
        calendar$n <- NA_integer_
        calendarList$day <- as.integer(calendarList$day)
        calendarList$month <- month.name[as.numeric(calendarList$month)]
        
        calendar <- full_join(calendar, calendarList, by = c("day", "month")) %>% 
          select(-n.x) %>% rename(n = n.y) 
        calendar[is.na(calendar)] <- 0
        calendar <- calendar %>% 
          mutate(n = if_else((month == "April" & day == 31) | (month == "June" & day == 31), NA, n)) %>% 
          mutate(n = if_else((month == "February" & day == 31) | (month == "February" & day == 30), NA, n)) %>% 
          mutate(n = if_else((month == "September" & day == 31) | (month == "November" & day == 31), NA, n))
        calendar <- calendar %>% rename(observations = n)

        heatMap <- ggplot(data = calendar) + 
          geom_tile(
            aes(x = day,
                y = month,
                fill = observations),      
            color = "white") +   # white gridlines
          geom_text(
            aes(
              x = day,
              y = month,
              label = observations),  
            color = "black",                          
            size = 4) +          
          scale_fill_gradient(
            low = "#f6efa6",
            high = "#bf444c",
            na.value = "white") +
          scale_x_discrete(
            expand = c(0, 0),
            limits = as.factor(1:31),
            labels = sprintf("%02d", seq(1, 31))) +
          scale_y_discrete(
            expand = c(0, 0),
            limits = rev(month.name)) +
          theme_minimal() +
          theme(
            plot.title.position = "plot", plot.title = element_text(hjust = 0.5, face = "bold", family = "Arial"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.title = element_text(size=12, face = "bold", family = "Arial"),
            legend.position = "top"
          ) +
          labs(
            x = "Day of Month",
            y = NULL,
            fill = "Photos\nper day",
            #title = "Most active days",
            subtitle = "With numbers of photos made each day, regardless of the year",
            caption = "Months and days.") 
        # guides(color = guide_colorbar(title.position = "top", title.hjust = .5, barwidth = unit(20, "lines"), 
        # barheight = unit(.5, "lines"))) #move legend to top and make horizontal
        ggplotly(heatMap) %>% 
          # https://github.com/plotly/plotly.js/blob/master/src/components/modebar/buttons.js
          config(modeBarButtonsToRemove = c("zoom2d", "select2d", "zoomIn2d", "zoomOut2d", "pan2d", "lasso2d", 
                                            "hoverClosestCartesian", "autoScale2d", "resetScale2d", "hoverCompareCartesian"), 
                 editable = FALSE, scrollZoom = FALSE) %>% 
          layout(yaxis = list( ticktext = c("December ", "November ", "October ", "September ", "August ", "July ", 
                                            "June ", "May ", "April ", "March ", "February ", "January "), mirror = TRUE,
                               legend = list(orientation = "h",   # show entries horizontally
                                             xanchor = "center",  # use center of legend as anchor
                                             x = -0.5)))  %>% 
          style(hovertemplate = paste0( "<span style='text-align: center'>%{y}%{x:,.0f}</span><br>Observations: %{text}",
                                        "<extra></extra>"))
      })
      
      
      ####                                                                                    ####
      #                                          Tree Map                                        #
      ####                                                                                    ####
      
      
      tempPhotoList <- tempPhotoList %>% mutate(Species = str_to_title(Species))
      tempPhotoList <- tempPhotoList %>% count(scientificName, Species) %>% arrange(-n)
      
      nTreeItems <- 29
      if (nrow(tempPhotoList) < nTreeItems){ nTreeItems <- nrow(tempPhotoList) }
      treeMapList <- tempPhotoList[1:nTreeItems, ] %>% 
        mutate(Photo = if_else(scientificName %in% BirdList_Final$scientificName, 
                               BirdList_Final$Photo[match(scientificName, 
                                                          BirdList_Final$scientificName)], NA))
      
      treeMap <- data_to_hierarchical(treeMapList, Species, n)
      
      if (inputUserPhotos == TRUE){
        for (i in 1:nTreeItems){
          photo <- treeMapList$Photo[match(treeMap[[i]]$name, treeMapList$Species)]
          treeMap[[i]]$color <- list(pattern = list(image = paste0("photos/", photo),
                                                    aspectRatio = 1/1))
        }} else {
          for (i in 1:nTreeItems){
            photo <- treeMapList$Photo[match(treeMap[[i]]$name, treeMapList$Species)]
            treeMap[[i]]$color <- list(pattern = list(image = photo,
                                                      aspectRatio = 1/1))
          }}
      
      output$treemapChart <- renderHighchart({
        hchart(treeMap, type = "treemap", dataLabels = list(
          backgroundColor = "black", borderWidth = 2, verticalAlign = "bottom",
          style = list(color = "white"))) %>%
          #hc_size(height = 1000)  %>% 
          #hc_chart(height = '100%') %>% 
          #hc_title(text = "Most Observed Species", style = list(fontWeight = 700)) %>% 
          hc_add_dependency("modules/pattern-fill.js") # necessary plugin to add images
      })
      

      ####                                                                                    ####
      #                                        Families Tab                                      #
      ####                                                                                    ####
      
      
      familiesList <- BirdList_Final %>% count(Family)
      discFamiliesList <- Discovered %>% count(Family)
      
      familiesList <- familiesList %>% 
        mutate(Discovered = if_else(Family %in% discFamiliesList$Family, 
                                    discFamiliesList$n[match(Family, discFamiliesList$Family)], 
                                    0)) %>% 
        mutate(percDiscovered = round(Discovered/n * 100, 1)) %>% arrange(-n) %>% 
        mutate(percUndiscovered = round(100 - percDiscovered, 1))
      
      itemChartList <- familiesList %>% arrange(desc(Discovered)) %>% filter(Discovered > 3)
      
      if (nrow(itemChartList) > 1){
        # Do not display more than X species so as not to cram it too much
        sumF <- 0
        stopF <- FALSE
        rows_to_filter <- c()
        
        # Loop through each row in the data frame
        for (i in 1:nrow(itemChartList)) {
          if (!stopF) {
            sumF <- sumF + itemChartList$Discovered[i]
            if (sumF >= 450) {
              stopF <- TRUE
            }
          }
          if (!stopF) {
            rows_to_filter <- c(rows_to_filter, i)
          }
        }
        
        # Filter the data frame based on the stored indices
        itemChartList <- itemChartList[rows_to_filter, ]
        
        output$itemChart <- renderHighchart({
          hchart(
            itemChartList,
            "item",
            hcaes(name = Family,
                  y = Discovered,
                  label = Family),
            name = "Unique Species Seen",
            showInLegend = FALSE,
            size = "80%",
            startAngle = -180,
            endAngle  = 180,
            innerSize = "20%",
            showEmpty = TRUE
          ) %>%
            hc_title(text = "Most Observed Families", 
                     style = list(fontWeight = 700))
        })
      }
      
      familiesList <- familiesList %>% 
        rename(`%` = "percDiscovered", Total = "n", Seen = "Discovered") %>% 
        select(Family, Seen, `%`, Total)
      
      output$familiesTable <- renderDT({
        
        dt <- datatable(familiesList,
                        tags$style(HTML("#x1 td {
                                          padding-left: 0;
                                          padding-right: 0;
                                         }")),
                        rownames = FALSE,
                        options = list(
                          dom = 'frtp', # Remove information panel
                          columnDefs = list(
                            list(width = "40%", targets = 0),                     # name
                            list(width = "10%", targets = 1, searchable = FALSE), # Seen
                            list(targets = 2, searchable = FALSE,                    # %
                                 render = JS(tablebars), className = "dt-center"),
                            list(width = "10%", targets = 3, searchable = FALSE, # Total
                                 className = "dt-left")
                          )
                        )
                      )
        dt %>% formatStyle(fontWeight = 500, columns = 1)
        
      })
      
    } else {
      shinyjs::show(id = "bodyMap")
      shinyjs::show(id = "search")
      shinyjs::hide(id = "bodyCharts")}
  })
  
  # JS code to show % as bar in DT tables
  tablebars <- 'function(data, type, row, meta) {
                        return $("<div></div>", {
                          class: "table-chart-bar"
                        })
                .append(
                  $("<div></div>", {
                    class: "table-bar"
                  }).css({
                    width: (data * 1) + "%"
                  })
                )
                .prop("outerHTML");}'

  
  
  ####                                                                                        ####
  #                                       UI: Menu Observers                                     #
  ####                                                                                        ####

  
  
  observeEvent(input$menuButton, {
    shinyjs::toggle(id = "menuPopup")})
  
  observeEvent(input$refreshData, {
    Warning_Tags <<- data.table()
    Warning_noTags <<- data.table()
    Warning_noMatch <<- data.table()
    Warning_GPS <<- data.table()
    Warning_AThrush <<- 0
    # Extract EXIF, GPS, IPTC, XMP, FlashPix, AFCP tags
    library(exiftoolr) # © Joshua O'Brien https://github.com/JoshOBrien/exiftoolr
    
    # Get the list of files and check if they're the same 
    image_files_new <- list.files(photoDir, pattern=".jpg$|.JPG$|.jpeg$|.JPEG$", 
                                  full.names = TRUE, recursive = TRUE)
    image_files_diff <<- setdiff(image_files_new, image_files)
    
    if (length(image_files_diff) > 0) {
      
      # Busy indicator
      if (length(image_files_diff) > 1000) {
        if (length(image_files_diff) > 3000) {
          show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", 
                         text = paste0(length(image_files_diff), 
                                       " new pictures found. \nDon't close this window, it will take a while."))
        } else {
          show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", 
                         text = paste0(length(image_files_diff), 
                                       " new pictures found.\nIt might take a while. Come back in a few minutes."))}
      } else {
        show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", 
                       text = paste0(length(image_files_diff), 
                                     " new pictures found. Hang on, it will take a minute."))
      }
      
      photoData <- as.data.table(exif_read(image_files_diff, 
                                           tags = c("gpslatitude", "gpslongitude", "subject", 
                                                    "DateTimeOriginal", "Rating")))
      
      show_modal_gif(src = "wd/loading_bird.gif", modal_size = "s", text = "We're almost done.")
      
      
      ####                                                                                    ####
      #                                     Data Cleaning                                        #
      ####                                                                                    ####
      
      
      ### 1:1 copy of initial data import
          # Calculate amount of columns required to separate Subject (# of comma-separated values)
          nTags <- max(stringr::str_count(photoData$Subject, ", ")) + 1
          
          print("Creating Warning_noTags table for photos without tags")
          if (any(is.null(photoData$Subject) | photoData$Subject == "NULL" | is.na(photoData$Subject) | photoData$Subject == "")) {
            Warning_noTags <<- photoData[(is.null(Subject) | Subject == "NULL" | is.na(Subject) | Subject == "")]
            Warning_noTags <<- Warning_noTags %>%
              mutate(DateTime = ymd_hms(DateTimeOriginal, quiet = TRUE),
                     DateTime = format(DateTime, format = "%d %b %Y, %H:%M"),
                     SourceFile = str_remove(SourceFile, photoDir),
                     Error = "No tags found") %>% 
              select(-DateTimeOriginal, -Subject)
          }
          
          # Delete untagged photos, split Subject to Tags
          photoData <- photoData[!(is.null(Subject) | Subject == "NULL" | is.na(Subject) | Subject == "")]
          photoData[, paste0("Tag", seq_len(nTags)) := tstrsplit(Subject, '", "')][, Subject := NULL]
          
          # Get rid of special characters in Tags
          photoData[, Tag1 := gsub('c\\("', '', Tag1)]
          photoData[, (grep("Tag", names(photoData), value = TRUE)) :=
                      lapply(.SD, function(x) gsub("(?![-'`])\\p{P}", "", x, perl = TRUE)),
                    .SDcols = patterns("Tag")]
          
          # Re-use nTags for "Tag" column names, assign tags to properly named columns
          nTags <- grep("Tag", names(photoData), value=TRUE)
          photoData[, c("Animal Class", "Zoo") := .(
            apply(.SD, 1, function(z) intersect(tolower(c("bird", "spider", "bug", 
                                                          "mammal", "reptile", "gastropod", "fish")), tolower(z))[1]),
            apply(.SD, 1, function(z) "zoo" %in% z)), .SDcols = nTags]
          photoData[, (grep("Tag", names(photoData), value = TRUE)) :=
                      lapply(.SD, function(x) ifelse(x %in% c("bird", "bug", "spider", "gastropod", "reptile", 
                                                              "mammal", "fish", "zoo", "uncertain"), NA_character_, x)),
                    .SDcols = nTags]
      ### 1:1 copy of initial data import
      
      # Remove pictures from zoos if requested, add the amount to a variable to show in the WarningModal
      if (inputZoo == FALSE){
        deletedZooNum <<- nrow(photoData[Zoo==TRUE])
        photoData <- photoData[Zoo==FALSE]
      }
      print("Finished removing zoo tags")
      
      # Clean up
      photoData <- photoData %>%
        mutate(DateTime = ymd_hms(DateTimeOriginal, quiet = TRUE),
               DateTime = format(DateTime, format = "%d %b %Y, %H:%M"),
               YearMonthDay = as.POSIXct(DateTimeOriginal, format = "%Y:%m:%d"),
               YearMonth = format(YearMonthDay, "%Y-%m"),
               YearMonthDay = format(YearMonthDay, "%Y-%m-%d"),
               `Animal Class` = tools::toTitleCase(photoData$`Animal Class`),
               SourceFile = str_remove(SourceFile, photoDir)) %>% # Same as sub() in Base R
        arrange(YearMonthDay)
      
      # Make GPS coords numeric, otherwise leaflet validateCoords has issues
      photoData$GPSLongitude[is.na(photoData$GPSLongitude)] <- NA_integer_
      photoData$GPSLongitude <- as.numeric(photoData$GPSLongitude)
      photoData$GPSLatitude[is.na(photoData$GPSLatitude)] <- NA_integer_
      photoData$GPSLatitude <- as.numeric(photoData$GPSLatitude)
      
      # Divide photos with 2 or more species into separate rows
      # Pivot all the columns that start with "Tag" into a column "Species"
      photoList_new <- photoData %>%
        pivot_longer(cols = starts_with("Tag"), values_to = "Species", values_drop_na = TRUE) %>%
        select(-name, -DateTimeOriginal) %>% # Remove the column created from pivoting + old date
        filter(!(`Animal Class` != "Bird" & !(is.na(`Animal Class`))))
      
      ####                                                                                        ####
      #                                        Data Analysis                                         #
      ####                                                                                        ####
      
      # Load the database for analysis
      BirdList <- readRDS(file = paste0(varPath,"/bin/Bi.rds"))
      
      library(stringdist) # Library for species name matching using Jaro–Winkler similarity
      
      print("Beginning to match species")
      
      match_scientificName <- function(species) {
        # Evaluate each column with an English name
        match <- amatch(tolower(species), tolower(BirdList$name), method = "jw")
        match_en1 <- amatch(tolower(species), tolower(BirdList$synEn1), method = "jw")
        match_en2 <- amatch(tolower(species), tolower(BirdList$synEn2), method = "jw")
        match_en3 <- amatch(tolower(species), tolower(BirdList$synEn3), method = "jw")
        match_en4 <- amatch(tolower(species), tolower(BirdList$synEn4), method = "jw")
        # Coalesce all the strings with results into one (e.g. NA 84 14 & 22 74 NA -> 22 84 14)
        matches <- coalesce(match, match_en1, match_en2, match_en3, match_en4)
        
        BirdList$scientificName[fifelse(matches > 0, matches, NA_integer_)]
      } 
      
      ## In case user's tags are in Latin
      if (inputScientific == TRUE) {
          photoList_new$Species <- photoList_new$scientificName
          photoList_new <- photoList_new %>% # Not the most elegant way of having them technically "matched"
            mutate(scientificName = if_else(tolower(scientificName) %in% tolower(BirdList$scientificName), scientificName, 
                                            if_else(tolower(scientificName) %in% tolower(BirdList$synsLatin), 
                                                    BirdList$scientificName[match(tolower(scientificName), 
                                                                                  tolower(BirdList$synsLatin))], 
                                                                                                NA_character_)))
      }
        
        ## In case of English tags
        ## Case insensitive & allows for ~one typo to be present in tags
      else if (!is.null(inputRegional) && length(inputRegional) > 0 && inputRegional != "Select one") {
        photoList_new$scientificName <- NA
        Warning_AThrush <- 0
        setDT(photoList_new)
        
        photoList_new[, scientificName := fifelse(is.na(scientificName), match_scientificName(Species), NA_character_)]
        if (inputRegional == "Eurasian/European") {
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("eurasian ", Species)), 
                                                scientificName)]
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("european ", Species)), 
                                                scientificName)]
        } else if (inputRegional == "African/Somali") {
          if ("Thrush" %in% photoList_new$Species){
            Warning_AThrush <- 1
          }
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("african ", Species)), 
                                                scientificName)]
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("somali ", Species)), 
                                                scientificName)]
        } else if (inputRegional == "Asian/Indian") {
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("asian ", Species)), 
                                                scientificName)]
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("indian ", Species)), 
                                                scientificName)]
        } else if (inputRegional == "Philippine/Mindanao") {
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("Philippine ", Species)), 
                                                scientificName)]
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("Mindanao ", Species)), 
                                                scientificName)]
                                                } else {
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste(inputRegional, " ", Species)), 
                                                scientificName)]
        }
        if (isTRUE(inputCommon)){
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("common ", Species)), 
                                                scientificName)]
        }
      } else {
        ## If the user didn't choose any regional adjectives
        photoList_new$scientificName <- NA
        setDT(photoList_new)
        photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                              match_scientificName(Species), 
                                              NA_character_)]
        if (isTRUE(inputCommon)){
          photoList_new[, scientificName := fifelse(is.na(scientificName), 
                                                match_scientificName(paste("common ", Species)), 
                                                scientificName)]
        }
      } 

      print("Creating a table for unmatched photos")
      if (any(is.na(photoList_new$scientificName))) {
        Warning_noMatch <<- photoList_new %>%
          filter((is.null(scientificName) | is.na(scientificName))) %>% 
          select(-YearMonth, -YearMonthDay, -scientificName, -Zoo, -`Animal Class`) %>% 
          select(SourceFile, Species, DateTime, GPSLatitude, GPSLongitude, everything()) %>% 
          mutate(Error = "Tags could not be matched with any species in the database")
        # Warn the user about the possible mismatch
        if (Warning_AThrush == 1){
          thrush <<- photoList_new %>% filter(Species == "Thrush") %>% 
            select(-YearMonth, -YearMonthDay, -scientificName, -Zoo, -`Animal Class`) %>% 
            select(SourceFile, Species, DateTime, GPSLatitude, GPSLongitude, everything()) %>% 
            mutate(Error = "Thrush was assumed to be Turdus pelios. If that's not the case, please change the tag.")
          Warning_noMatch <<- rbind(Warning_noMatch, thrush)
        }
      }
      
      print("Preparing warning tables")
      if (nrow(Warning_noTags) > 0 && (nrow(Warning_noMatch) > 0)) {
        Warning_noTags$GPSLatitude <- as.integer(Warning_noTags$GPSLatitude)
        Warning_noTags$GPSLongitude <- as.integer(Warning_noTags$GPSLongitude)
        Warning_Tags <<- full_join(Warning_noMatch, Warning_noTags) 
      } else if (nrow(Warning_noTags) > 0) {
        Warning_Tags <<- Warning_noTags 
      } else if (nrow(Warning_noMatch) > 0) {
        Warning_Tags <<- Warning_noMatch}
      if (nrow(Warning_GPS) > 0 & csvFile == ""){
        Warning_GPS <<- Warning_GPS %>% 
          filter(!SourceFile %in% Warning_Tags$SourceFile)
      }
      
      print("Filtering out NAs from photoList, sorting by Rating")
      photoList_new <- photoList_new %>%
        filter(!(is.na(scientificName)))
      nrow_for_warnings <<- nrow(photoList_new)

      photoList <<- full_join(photoList, photoList_new)
      if (exists("Rating", where = photoList)){
        photoList <<- photoList %>% arrange(desc(Rating), desc(YearMonthDay))
      } else {
        photoList <<- photoList %>% arrange(desc(YearMonthDay))
      }
      
      if (exists("BirdList_Final")) {
        BirdList_Final <<- BirdList_Final %>%
          mutate(Discovered = scientificName %in% photoList$scientificName)
                                                
      if (csvFile == ""){
        # Replace photos with user photos, at user's request
        if (inputUserPhotos) {
          
          BirdList_Final[, Photo := fifelse(scientificName %in% photoList$scientificName, 
                                            photoList$SourceFile[match(scientificName, photoList$scientificName)], Photo)]
          BirdList_Final[, source := fifelse(scientificName %in% photoList$scientificName, 
                                             photoList$SourceFile[match(scientificName, photoList$scientificName)], source)]
          BirdList_Final[, rightsHolder := fifelse(scientificName %in% photoList$scientificName, "you", rightsHolder)]
        }
      }
      
      # Replace database names with user's names, if chosen (e.g. Eurasian Eagle Owl instead of Eurasian Eagle-owl)
      if (inputReplaceNames == TRUE && inputScientific == TRUE){
        BirdList_Final <<- BirdList_Final %>%
          mutate(name = if_else(scientificName %in% photoList$scientificName, 
                                photoList$Species[match(scientificName, photoList$scientificName)], 
                                if_else(synsLatin %in% photoList$scientificName, 
                                        photoList$Species[match(synsLatin, photoList$scientificName)], name)))
      } else if (inputReplaceNames == FALSE && inputScientific == FALSE){
        photoList <<- photoList %>%
          mutate(Species = if_else(scientificName %in% BirdList_Final$scientificName, 
                                   BirdList_Final$name[match(scientificName, BirdList_Final$scientificName)], 
                                   if_else(scientificName %in% BirdList_Final$synsLatin, 
                                           BirdList_Final$name[match(scientificName, BirdList_Final$synsLatin)], Species)))
        }
      }
      
      print("Create a new Searchlist with (possibly) new names")
      Searchlist <<- data.table(c())
      Searchlist <<- data.table(name = c(BirdList_Final$name, BirdList_Final$scientificName, BirdList_Final$synsLatin, BirdList_Final$synEn1, BirdList_Final$synEn2, BirdList_Final$synEn3, BirdList_Final$synEn4)) %>% filter(name != "", name != is.na(name))
      
      # Create a shorter list from database with only the seen species
      Discovered <<- BirdList_Final %>% filter(Discovered == TRUE)
  
      ####                                                                                        ####
      #                                         Leaflet Prep                                         #
      ####                                                                                        ####
      
      library(sf)
    
      print("Loading leaflet_sf")
      leaflet_sf <- readRDS(file = paste0(varPath,"/bin/leaflet.rds"))
      
      # Calculate total amount of species *seen* per country
      userStats <- Abundances %>% 
        filter(scientificName %in% Discovered$scientificName) %>% 
        filter(Present == TRUE) %>% 
        count(countrycode, month) %>% 
        pivot_wider(names_from = "month", values_from = "n", names_prefix = "Seen") %>% 
        rename(Seen = Seen0, iso_a2_eh = countrycode) 
      userStats[is.na(userStats)] <- 0
      print("Added userStats")
      
      # st_as_sf to convert the data into something leaflet can understand
      leaflet_sf <- st_as_sf(left_join(leaflet_sf, userStats, by = join_by(iso_a2_eh))) 
      leaflet_sf[is.na(leaflet_sf)] <- 0
      leaflet_sf <- leaflet_sf %>% mutate(Ratio = round(Seen/Total, 3))
      print("Adding monthly stats to leaflet_sf")
      for (mnth in 1:12){
        n <- which(colnames(leaflet_sf) == mnth)
        seenm <- paste0("Seen", mnth)
        ratiom <- paste0("Ratio", mnth)
        leaflet_sf[[ratiom]] <- round(leaflet_sf[[seenm]]/leaflet_sf[[n]], 3)
      }
      leaflet_sf[is.na(leaflet_sf)] <- 0
      leaflet_sf <<- copy(leaflet_sf)
      userStats$country_name <- leaflet_sf$country_name[match(userStats$iso_a2_eh, leaflet_sf$iso_a2_eh)]
      userStats$Total <- leaflet_sf$Total[match(userStats$iso_a2_eh, leaflet_sf$iso_a2_eh)]
      userStats <- userStats %>% select(iso_a2_eh, country_name, Seen, Total)
      tempLeafset <- st_drop_geometry(leaflet_sf) %>% select(iso_a2_eh, country_name, Total)
      tempStats <- userStats %>% select(iso_a2_eh, country_name, Total)
      tempLeafset <- anti_join(tempLeafset, userStats)
      tempLeafset$Seen <- 0
      tempLeafset <- tempLeafset %>% select(iso_a2_eh, country_name, Seen, Total)
      userStats <- rbind(userStats, tempLeafset) %>% arrange(country_name)
      userStats <<- copy(userStats)
      
      removeModal(session)
      remove_modal_gif()
      
      image_files <<- image_files_new
        
      save(file = file.path(varPath,"/userData/Use.RData"),
           photoDir, csvFile, photoList, Searchlist, userStats, leaflet_sf, image_files, 
           BirdList_Final, Discovered, inputCommon, inputRegional, inputUserPhotos, inputScientific, 
           inputZoo, inputReplaceNames, Abundances)
        
      # Show the page
      shinyjs::show(id = "bodyMap")
      shinyjs::show(id = "search")
      shinyjs::hide(id = "bodyCharts")
      createLeaflet()
      createSlider()
      gc()
      
      # In case of warnings, show the warning modal
      if ( nrow(Warning_Tags) > 0 | nrow(Warning_GPS) > 0 | exists("deletedZooNum") && deletedZooNum > 0 ){
        Warning_Modal()
      }
      
    } else {
      sendSweetAlert(
        session = session,
        title = "No new photos found",
        text = "Perhaps you wanted to run re-indexation? Re-indexation takes a little while, but it checks for tag updates and removes photos that you deleted from your disk.",
        type = "info"
      )
    }
  })
  
  observeEvent(input$reIndexData, {
    shinyjs::hide(id = "conditional")
    dataInput()
  })
  
  observeEvent(input$CSVreIndexData, {
    shinyjs::hide(id = "conditional")
    importModal()
  })
  
  observeEvent(input$changeData, {
    shinyjs::hide(id = "conditional")
    importModal()
  })
  
  observeEvent(input$CSVchangeData, {
    shinyjs::hide(id = "conditional")
    importModal()
  })
  
  # observeEvent(input$reportIssue, {
  #   # Open the website link in a new tab
  #   browseURL("https://github.com/OrsonDeWitt/BirdMApp/issues")
  # })
  
  observeEvent(input$website, {
    # Open the website link in a new tab
    browseURL("https://orsondewitt.com")
  })
  
  
  ####                                                                                        ####
  #                                    Import Warning Modals                                     #
  ####                                                                                        ####
  
  
  
  # Modal for errors when map cannot be created
  NoBirds_Modal <- function() {
    showModal(modalDialog(
      tags$div(style = "position: center;",
               tags$h2(style = "text-align: center; margin: 0;",
                       HTML(paste0(error))
               )),
      tags$p(),tags$br(),
      tags$div(style = "position: absolute; bottom: 3%; left: 45%;",
               actionButton(label = "Try again", class = "closeButton", 
                            inputId = "tryAgain")),
      footer = NULL
    ))
  }
  
  observeEvent(input$tryAgain, {
    importModal()
  })
  
  # Modal for analysis errors/warnings
  Warning_Modal <- function() {
    showModal(modalDialog(
      tags$div(style = "position: absolute; top: 0; right: 0;",
           actionButton(label = "", class = "closeButton", 
                        inputId = "closeModal", icon = icon(name = "xmark", lib = "font-awesome"))),
      tags$div(style = "position: center;",
           tags$h2(style = "text-align: center; margin: 0;",
                   
               if (exists("image_files_diff") && image_files_diff != 0){
                 HTML(paste0(nrow_for_warnings - nrow(Warning_Tags), "/", nrow_for_warnings, " photos successfully processed"))
               } else if (csvFile == ""){
                 HTML(paste0(length(unique(photoList$SourceFile)), "/", length(image_files), " photos successfully processed"))
               } else {
                 HTML(paste0(nrow(photoList), "/", nrow_for_warnings, " observations successfully processed"))
               }
                   
           ),
           # Aesthetics - lines and feather
           tags$div(style = "position: relative; height: 1px; margin-bottom: 40px; margin-top: 40px",
                tags$div(style = "position: absolute; left: 0; top: 0; width: 45%; height: 140%; background-color: #B96148;"),
                tags$span(
                  style = "position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); color: #B96148; font-size: 24px;",
                  class = "fas fa-feather"
                ),
                tags$div(style = "position: absolute; right: 0; top: 0; width: 45%; height: 140%; background-color: #B96148;"
                ))),
      
      if (nrow(Warning_GPS) > 0){
          tags$div(
               tags$h4(style = "color: #B96148; font-size: 16px; margin: 0;", 
                       HTML(paste0(icon("circle-exclamation"), 
                                  " The following photos <b>have been processed without location tags</b>. 
                                  These photos will not have markers on the map."))),
               
              # Since the spinner of ShinyWidgets does not disappear on its own and has to be masked, use tableContainer CSS
               if (nrow(Warning_Tags) == 0){
                 addSpinner(
                   div(id = "tableContainer", 
                       tableOutput("Warning_GPS")),
                   spin = "folding-cube",
                   color = "#405b98"
                 )} else { div(id = "tableContainer", 
                              tableOutput("Warning_GPS")) })},
      
      if (nrow(Warning_Tags) > 0){
          tags$div(
               tags$h4(style = "color: #B96148; font-size: 16px; margin: 0;", 
                       HTML(paste0(icon("circle-exclamation"), 
                                    " The following photos <b>have not been processed</b>. 
                                   Please re-index the folder if you fix the issues listed below."))),
               addSpinner(
                  div(id = "tableContainer", 
                      tableOutput("Warning_Tags")),
                  spin = "folding-cube",
                  color = "#405b98" ) )},
      
      if (input$zoo == FALSE && exists("deletedZooNum") && deletedZooNum > 0 ) {
        tags$div(
             tags$h4(style = "color: #B96148; font-size: 16px; margin: 0; margin-bottom: 10px;", 
                     HTML(paste0(icon("circle-exclamation"), " ", deletedZooNum, 
                                " photos have been excluded as they had the \"zoo\" tag."))))}
    ))
    image_files_diff <<- 0 # Reset: so that it doesn't show wrong number if the user re-indexes after syncing
    }
  
  output$Warning_GPS <- renderTable({
    Warning_GPS
  })
  output$Warning_Tags <- renderTable({
    Warning_Tags
  })
  
  observeEvent(input$closeModal, {
    if (input$closeModal) {
      removeModal()
      # Remove photo popovers
      shinyjs::runjs("if ($('.popover').length) $('.popover').popover('hide');")
    }
  })
  
  # If changelog doesn't have welcome text anymore, show modal
  welcomeModal <- function(){
    changelog <- 'https://orsondewitt.com/birdmapp/changelog'
    library(rvest)
    library(xml2)
    changelog <- read_html(changelog)
    # If the first header under "Changelog" equals 1.0
    if (strsplit(xml_attrs(xml_child(xml_child(changelog, 1), 2))[["id"]], "--")[[1]][[1]] == "10"){
      showModal(modalDialog(
        tags$div(style = "position: center;",
                 tags$h2(style = "text-align: center; margin: 0;", "Hey there!"),
                 tags$p(),
                 tags$div(style = "text-align: center;", HTML(paste0(
                         "This pop-up will be used as a way to notify you if there's a new release 
                          (unless I find a better way...). Hope it's not too much of a bother!<br>
                          For now, though, since this has just been released, there's nothing new. 
                          Enjoy the app, and let me know how you find it!<br>
                          No, really. I put my heart and soul into this and would 
                          <a href='mailto:dev@orsondewitt.com' target='_blank'>love to hear it</a> 
                           if you genuinely find it useful 😊")))
        ))
      )
    } else {
      showModal(modalDialog(
        tags$div(style = "position: center; text-align: center; margin: 0;",
                 tags$h2("New release!"),
                 tags$a(href= "https://orsondewitt.com/birdmapp/", target = "_blank", "Download here"),
                 tags$p(),
                 tags$iframe(src = "https://orsondewitt.com/birdmapp/changelog", frameBorder="0", width = "600px")
        )))
    }
  }
  
  ####                                                                                        ####
  #                                            Misc                                              #
  ####                                                                                        ####
  
  # Set GB locale so everything is consistent
  Sys.setlocale (category = "LC_TIME", locale = "en_GB.UTF-8")
  months_n <- c("January", "February", "March", "April", "May", "June", 
                "July", "August", "September", "October", "November", "December")
  startUp <- TRUE
  inpSpecies <- ""
  m <- 0

  # Terminate the process
  session$onSessionEnded(function() {
    stopApp()
  })

}

