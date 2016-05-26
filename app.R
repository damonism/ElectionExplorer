library(shiny)
library(leaflet)
library(htmltools)
library(sp)
library(rgdal)

##
## TODO:
## - Slider to only show polling places of a certain size
## - Fix up the UI.
## - Pre-poll and super-booth toggle
## - Collapse polling places at the same location together.
## - Mark abolished polling places and gather their votes into the nearest remaining [hard!]


# ##
# ## Download the 2013 results by polling place
# ##
# 
# # 2013 First HoR First Preference by PP
# hor_2013 <- read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-NSW.csv", header=TRUE, skip=1, sep=",")
# hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-VIC.csv", header=TRUE, skip=1, sep=","))
# hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-QLD.csv", header=TRUE, skip=1, sep=","))
# hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-WA.csv", header=TRUE, skip=1, sep=","))
# hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-SA.csv", header=TRUE, skip=1, sep=","))
# hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-TAS.csv", header=TRUE, skip=1, sep=","))
# hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-ACT.csv", header=TRUE, skip=1, sep=","))
# hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-NT.csv", header=TRUE, skip=1, sep=","))
# hor_2013$Election <- "2013"
# 
# # It's probably easier to use PartyAb than PartyNm, but put "Informal" in the otherwise
# # black PartyAb for informal votes.
# 
# hor_2013$PartyAb <- as.character(hor_2013$PartyAb)
# hor_2013$PartyAb[hor_2013$PartyNm == "Informal"] <- "Informal"
# hor_2013$PartyAb <- as.factor(hor_2013$PartyAb)
# 
# library(reshape2)
# result_by_pp_2013 <- dcast(hor_2013, StateAb + DivisionNm + PollingPlace + PollingPlaceID ~ PartyAb, sum, value.var = "OrdinaryVotes")
# result_by_pp_2013$TotalVotes <- rowSums(result_by_pp_2013[,!colnames(result_by_pp_2013) %in% c("StateAb", "DivisionNm", "PollingPlace", "PollingPlaceID")])
# 
# # Toby's fixed 2016 boundaries
# boundaries_new_2016 <- readOGR("2016 Commonwealth Boundaries/Fixed", "COM_ELB_20160509")
# 
# # Reduce the complexity of the district boundary map (default is 46mb)
# library(rgeos)
# boundaries_big_2016 <- boundaries_new_2016
# tmp_data <- data.frame(boundaries_big_2016)
# tmp_shape <- gSimplify(boundaries_big_2016, tol = .01, topologyPreserve = TRUE)
# boundaries_new_2016 <- SpatialPolygonsDataFrame(tmp_shape, tmp_data)
# rm(tmp_shape)
# rm(tmp_data)
# 
# # The variable names are different in the redistribution maps, so we need to rename them
# names(boundaries_new_2016)[names(boundaries_new_2016) == "Elect_div"] <- "ELECT_DIV"
# names(boundaries_new_2016)[names(boundaries_new_2016) == "Actual"] <- "ACTUAL"
# names(boundaries_new_2016)[names(boundaries_new_2016) == "Numccds"] <- "NUMCCDS"
# names(boundaries_new_2016)[names(boundaries_new_2016) == "Area_SqKm"] <- "AREA_SQKM"
# names(boundaries_new_2016)[names(boundaries_new_2016) == "Projected"] <- "PROJECTED"
# names(boundaries_new_2016)[names(boundaries_new_2016) == "Sortname"] <- "SORTNAME"
# names(boundaries_new_2016)[names(boundaries_new_2016) == "Total_Popu"] <- "POPULATION"
# 
# # Some of the division names are not capitalised properly, and this messes up the merge with the notional status
# levels(boundaries_new_2016$ELECT_DIV) <- gsub("Mcpherson", "McPherson", levels(boundaries_new_2016$ELECT_DIV))
# levels(boundaries_new_2016$ELECT_DIV) <- gsub("Mcmillan", "McMillan", levels(boundaries_new_2016$ELECT_DIV))
# 
# # Add the notional status
# notional_status <- read.csv("Notional.csv")
# boundaries_new_2016 <- merge(boundaries_new_2016, notional_status)
# 
# # Okay, worked out what I did wrong last time is I need to make two sets of polygons, one for ALP and one for Coalition
# 
# boundaries_2016_coalition <- boundaries_new_2016[boundaries_new_2016$Notional == "Coalition",]
# boundaries_2016_alp <- boundaries_new_2016[boundaries_new_2016$Notional == "ALP",]
# boundaries_2016_nonclassic <- boundaries_new_2016[boundaries_new_2016$Notional == "Non-Classic",]
# 
# # Invert the ALP margins (turn them into minus margins)
# boundaries_new_2016$RelMargin <- boundaries_new_2016$Margin
# boundaries_new_2016$RelMargin[boundaries_new_2016$Notional == "ALP"] <- 0 - boundaries_new_2016$Margin[boundaries_new_2016$Notional == "ALP"]
# 
# ##
# ## Polling places
# ##
# 
# pp_geo <- read.csv("http://results.aec.gov.au/17496/Website/Downloads/GeneralPollingPlacesDownload-17496.csv", header = TRUE, skip = 1)
# tcp_by_pp <- read.csv("http://results.aec.gov.au/17496/Website/Downloads/HouseTcpByCandidateByPollingPlaceDownload-17496.csv", header = TRUE, skip = 1)
# 
# library(reshape2)
# tcp_party_pp <- dcast(tcp_by_pp, PollingPlaceID + PollingPlace ~ PartyAb, value.var = "OrdinaryVotes")
# tcp_party_pp$COA <- rowSums(cbind(tcp_party_pp$LP, tcp_party_pp$CLP, tcp_party_pp$LNP, tcp_party_pp$NP), na.rm = TRUE)
# tcp_party_pp$TotalVotes <- tcp_party_pp$ALP + tcp_party_pp$COA
# tcp_party_pp$PercentALP <- tcp_party_pp$ALP / tcp_party_pp$TotalVotes
# tcp_party_pp$PercentCOA <- tcp_party_pp$COA / tcp_party_pp$TotalVotes
# 
# # Because we're going to merge this with the full polling place results by party, we need to identify these as TPP results by party
# names(tcp_party_pp) <- gsub("^", "tpp", names(tcp_party_pp))
# names(tcp_party_pp)[1] <- "PollingPlaceID"
# 
# # Merge TPP and Geocodes
# tcp_party_pp <- merge(tcp_party_pp, pp_geo, by = "PollingPlaceID", all.x = TRUE)
# 
# # Merge this with the results
# tcp_party_pp <- merge(tcp_party_pp, result_by_pp_2013, by = "PollingPlaceID", all.x = TRUE)
# 
# # We don't want polling places with no geocodes
# tcp_party_pp <- tcp_party_pp[!is.na(tcp_party_pp$Longitude),]
# 
# # Some polling places have a dodgy geocode
# tcp_party_pp <- tcp_party_pp[tcp_party_pp$Longitude > 0,]
# # Dandenong West is on Christmas Island, for some reason
# tcp_party_pp <- tcp_party_pp[tcp_party_pp$PollingPlaceID != 3965,]
# 
# # And the mapping is easier if we delete the Norfolk Island polling places (sorry Norfolk Island)
# tcp_party_pp <- tcp_party_pp[tcp_party_pp$PremisesPostCode != 7140,]
# # And Lord Howe Island
# tcp_party_pp <- tcp_party_pp[tcp_party_pp$PremisesPostCode != 2898,]
# # Home Island
# tcp_party_pp <- tcp_party_pp[tcp_party_pp$PremisesPostCode != 6799,]
# # Christmas Island
# tcp_party_pp <- tcp_party_pp[tcp_party_pp$PremisesPostCode != 6798,]
# 
# # Also get rid of the polling places which don't have any votes
# tcp_party_pp <- tcp_party_pp[tcp_party_pp$TotalVotes > 0,]
# 
# # Tell R what the Latitudes and Longitudes are
# coordinates(tcp_party_pp) <- c("Longitude", "Latitude")
# proj4string(tcp_party_pp) <- proj4string(boundaries_new_2016)
# 
# tcp_party_pp <- tcp_party_pp[!is.na(tcp_party_pp$PollingPlaceID),]
# 
# # Percantage results for ALP, Coalition, Greens
# tcp_party_pp$PercentALP <- tcp_party_pp$ALP / tcp_party_pp$TotalVotes * 100
# tcp_party_pp$PercentCOA <- (tcp_party_pp$LP + tcp_party_pp$NP + tcp_party_pp$CLP + tcp_party_pp$LNP) / tcp_party_pp$TotalVotes * 100
# tcp_party_pp$PercentGRN <- tcp_party_pp$GRN / tcp_party_pp$TotalVotes * 100
# 
# # Break down polling places into TPP groups
# tcp_pp_alp <- tcp_party_pp[!is.na(tcp_party_pp$tppPercentALP),]
# tcp_pp_alp <- tcp_pp_alp[tcp_pp_alp$tppPercentALP >= 0.5,]
# 
# # Determine relative margin (ALP with negative margins)
# tcp_party_pp$tppRelative <- tcp_party_pp$tppPercentCOA - 0.5
# 
# tcp_pp_coa <- tcp_party_pp[!is.na(tcp_party_pp$tppPercentCOA),]
# tcp_pp_coa <- tcp_pp_coa[tcp_pp_coa$tppPercentCOA >= 0.5,]


ui <- fluidPage(
  titlePanel("2013 Election Results Explorer"),
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map"),
  
  absolutePanel(
    top = 70, right = 20, width = 300,
    draggable = TRUE,
    style = "opacity: 0.92",
    radioButtons("polling_place_markers", "Polling Place Markers",
                 choices = list("Two Party Preferred" = "tpp",
                                "Coalition Primary" = "coa",
                                "ALP Primary" = "alp",
                                "Greens Primary" = "grn"),
                 selected = "tpp"),
    # checkboxGroupInput("boundaries", "Boundaries",
    #                    choices = list("2016 Notional Margin" = "margin",
    #                                   "2016 Boundary" = "outline"),
    #                    selected = "margin"),
    checkboxInput("no_ppvc", "Exclude pre-polls"),
    selectInput("division", "Zoom to Division", c("Select a division" = "", levels(boundaries_new_2016$ELECT_DIV)))
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(boundaries_new_2016) %>% 
      addTiles() %>% 
     addProviderTiles("CartoDB.Positron") %>%
      fitBounds(~min(tcp_party_pp$Longitude), 
                ~min(tcp_party_pp$Latitude), 
                ~max(tcp_party_pp$Longitude), 
                ~max(tcp_party_pp$Latitude))

  })
  
  
  ##
  ## Draw the boundaries as filled polygons shaded by margin
  ##
  
  observe({
    
    content_alp <- paste(
      "<b>", htmlEscape(boundaries_2016_alp$ELECT_DIV), "</b><br />",
      "Notional Party:", htmlEscape(boundaries_2016_alp$Notional), "<br />",
      "Notional Margin:", htmlEscape(round(boundaries_2016_alp$Margin, digits = 1)), "<br />",
      "Held By:", htmlEscape(boundaries_2016_alp$HeldBy), "<br />"
    )
    
    content_coalition <- paste(
      "<b>", boundaries_2016_coalition$ELECT_DIV, "</b><br />",
      "Notional Party:", boundaries_2016_coalition$Notional, "<br />",
      "Notional Margin:", round(boundaries_2016_coalition$Margin, digits = 1), "<br />",
      "Held By:", boundaries_2016_coalition$HeldBy, "<br />"
    )
    
    content_nonclassic <- paste(
      "<b>", boundaries_2016_nonclassic$ELECT_DIV, "</b><br />",
      "Notional Party:", boundaries_2016_nonclassic$Notional, "<br />",
      "Notional Margin:", round(boundaries_2016_nonclassic$Margin, digits = 1), "<br />",
      "Held By:", boundaries_2016_nonclassic$HeldBy, "<br />"
    )
    
    
    pal_coalition <- colorNumeric(
      palette = "Blues", 
      domain = boundaries_new_2016$Margin
    )
    
    pal_alp <- colorNumeric(
      palette = "Reds", 
      domain = boundaries_new_2016$Margin
    )
    
    pal_nonclassic <- colorNumeric(
      palette = "Greens",
      domain = boundaries_new_2016$Margin
    )  
    
    leafletProxy("map", data = boundaries_2016_alp) %>% 
      addPolygons(color = ~pal_alp(boundaries_2016_alp$Margin), popup = content_alp, 
                  stroke = TRUE, weight = 1, fillOpacity = 0.5, group = "Notional Margin")
    leafletProxy("map", data = boundaries_2016_coalition) %>% 
      addPolygons(color = ~pal_coalition(boundaries_2016_coalition$Margin), popup = content_coalition, 
                  stroke = TRUE, weight = 1, fillOpacity = 0.5, group = "Notional Margin")
    leafletProxy("map", data = boundaries_2016_nonclassic) %>% 
      addPolygons(color = ~pal_nonclassic(boundaries_2016_nonclassic$Margin), popup = content_nonclassic, 
                  stroke = TRUE, weight = 1, fillOpacity = 0.5, group = "Notional Margin")
    leafletProxy("map", data = boundaries_new_2016) %>% 
      addPolylines(weight = 1, stroke = TRUE, opacity = 1, color = "Black", group = "2016 Boundary")

    # For some reason this is *really* slow, at least on my computer.     
    if("margin" %in% input$boundaries){
      leafletProxy("map") %>% showGroup("Notional Margin")
    } else {
      leafletProxy("map") %>% hideGroup("Notional Margin")
    }
    
    if("outline" %in% input$boundaries){
      leafletProxy("map") %>% showGroup("2016 Boundary")
    } else {
      leafletProxy("map") %>% hideGroup("2016 Boundary")
    }
    
    leafletProxy("map") %>% addLayersControl(
      position = "bottomright",
      options = layersControlOptions(collapsed = FALSE),
      overlayGroups = c("Notional Margin", "2016 Boundary")
    )
    
    
  })
  
  ##
  ## Draw the two-party preferred polling place markers
  ##
  
  # observe({
  #   
  #   pal_pp_alp <- colorNumeric(
  #     palette = "Reds",
  #     domain = tcp_pp_alp$tppPercentALP
  #   )
  #   
  #   pal_pp_coa <- colorNumeric(
  #     palette = "Blues",
  #     domain = tcp_pp_coa$tppPercentCOA
  #   )
  #   
  #   pal_grn_percent <- colorNumeric(
  #     palette = "Greens",
  #     domain = tcp_party_pp$PercentGRN
  #   )
  #   
  #   pal_alp_percent <- colorNumeric(
  #     palette = "Reds",
  #     domain = tcp_party_pp$PercentALP
  #   )
  #   
  #   pal_coa_percent <- colorNumeric(
  #     palette = "Blues",
  #     domain = tcp_party_pp$PercentCOA
  #   )
  #   
  #   content_tcp_alp <- paste(
  #     "<b>", tcp_pp_alp$PollingPlace, " (", tcp_pp_alp$PremisesSuburb, ", ", tcp_pp_alp$PremisesStateAb, ")</b><br />",
  #     "Division of ", tcp_pp_alp$DivisionNm.x, "<br>",
  #     "ALP primary vote: ", tcp_pp_alp$ALP, " (", round(tcp_pp_alp$PercentALP, digits = 1), "%)<br />",
  #     "ALP Two Party Preferred: ", round(tcp_pp_alp$tppPercentALP * 100, digits = 1), "%<br />",
  #     "Coalition primary vote: ", tcp_pp_alp$COA, " (", round(tcp_pp_alp$PercentCOA, digits = 1), "%)<br />",
  #     "Coalition Two Party Preferred: ", round(tcp_pp_alp$tppPercentCOA * 100, digits = 1), "%<br />",
  #     "Greens primary vote: ", tcp_pp_alp$GRN, " (", round(tcp_pp_alp$PercentGRN, digits = 1), "%)<br />",
  #     "Total votes: ", tcp_pp_alp$TotalVotes, "<br />",
  #     sep = ""
  #   )
  #   
  #   content_tcp_coa <- paste(
  #     "<b>", tcp_pp_coa$PollingPlace, " (", tcp_pp_coa$PremisesSuburb, ", ", tcp_pp_coa$PremisesStateAb, ")</b><br />",
  #     "Division of ", tcp_pp_coa$DivisionNm.x, "<br>",
  #     "ALP primary vote: ", tcp_pp_coa$ALP, " (", round(tcp_pp_coa$PercentALP, digits = 1), "%)<br />",
  #     "ALP Two Party Preferred: ", round(tcp_pp_coa$tppPercentALP * 100, digits = 1), "%<br />",
  #     "Coalition primary vote: ", tcp_pp_coa$COA, " (", round(tcp_pp_coa$PercentCOA, digits = 1), "%)<br />",
  #     "Coalition Two Party Preferred: ", round(tcp_pp_coa$tppPercentCOA * 100, digits = 1), "%<br />",
  #     "Greens primary vote: ", tcp_pp_coa$GRN, " (", round(tcp_pp_coa$PercentGRN, digits = 1), "%)<br />",
  #     "Total votes: ", tcp_pp_coa$TotalVotes, "<br />",
  #     sep = ""
  #   )
  #   
  #   # Content bubble for COA/ALP/GRN polling places
  #   content_pp <- paste(
  #     "<b>", htmlEscape(tcp_party_pp$PollingPlace), 
  #     " (", htmlEscape(tcp_party_pp$PremisesSuburb), ", ", htmlEscape(tcp_party_pp$PremisesStateAb), ")</b><br />",
  #     "Division of ", tcp_party_pp$DivisionNm.x, "<br>",
  #     "ALP primary vote: ", tcp_party_pp$ALP, " (", round(tcp_party_pp$PercentALP, digits = 1), "%)<br />",
  #     "ALP Two Party Preferred: ", round(tcp_party_pp$tppPercentALP * 100, digits = 1), "%)<br />",
  #     "Coalition primary vote: ", tcp_party_pp$COA, " (", round(tcp_party_pp$PercentCOA, digits = 1), "%)<br />",
  #     "Coalition Two Party Preferred: ", round(tcp_party_pp$tppPercentCOA * 100, digits = 1), "%)<br />",
  #     "Greens primary vote: ", tcp_party_pp$GRN, " (", round(tcp_party_pp$PercentGRN, digits = 1), "%)<br />",
  #     "Total votes: ", tcp_party_pp$TotalVotes, "<br />",
  #     sep = ""
  #   )
  #   
  #   #
  #   # Select which layer to show
  #   #
  #   
  #   # Show the Two Party Preferred
  #   if(input$polling_place_markers == "tpp"){
  #     leafletProxy("map") %>% clearMarkers()
  #     leafletProxy("map", data = tcp_pp_alp ) %>% 
  #       addCircleMarkers(popup = content_tcp_alp, color = ~pal_pp_alp(tcp_pp_alp$tppPercentALP), stroke = FALSE, 
  #                        radius = 3, fillOpacity = 0.8, group = "Two Party Preferred")
  #     
  #     leafletProxy("map", data = tcp_pp_coa ) %>% 
  #       addCircleMarkers(popup = content_tcp_coa, color = ~pal_pp_coa(tcp_pp_coa$tppPercentCOA), stroke = FALSE, 
  #                        radius = 3, fillOpacity = 0.8, group = "Two Party Preferred")
  #     
  #     #            leafletProxy("map") %>% showGroup("Two Party Preferred") %>% 
  #     #        hideGroup("Coalition Primary") %>% hideGroup("ALP Primary") %>% hideGroup("Greens Primary")
  #     
  #   }
  #   
  #   # Show the Coalition
  #   if(input$polling_place_markers == "coa"){
  #     leafletProxy("map") %>% clearMarkers()
  #     
  #     leafletProxy("map", data = tcp_party_pp ) %>% 
  #       addCircleMarkers(popup = content_pp, color = ~pal_coa_percent(tcp_party_pp$PercentCOA), stroke = FALSE, 
  #                        radius = 3, fillOpacity = 0.8, group = "Coalition Primary")
  #     
  #     
  #     #      leafletProxy("map") %>% showGroup("Coalition Primary") %>% 
  #     #        hideGroup("Two Party Preferred") %>% hideGroup("ALP Primary") %>% hideGroup("Greens Primary")
  #   }
  #   
  #   # Show the ALP
  #   if(input$polling_place_markers == "alp"){
  #     
  #     leafletProxy("map") %>% clearMarkers()
  #     leafletProxy("map", data = tcp_party_pp ) %>% 
  #       addCircleMarkers(popup = content_pp, color = ~pal_alp_percent(tcp_party_pp$PercentALP), stroke = FALSE, 
  #                        radius = 3, fillOpacity = 0.8, group = "ALP Primary")
  #     #      leafletProxy("map") %>% showGroup("ALP Primary") %>% 
  #     #        hideGroup("Two Party Preferred") %>% hideGroup("Coalition Primary") %>% hideGroup("Greens Primary")
  #   }
  #   
  #   # Show the Greens
  #   if(input$polling_place_markers == "grn"){
  #     
  #     leafletProxy("map") %>% clearMarkers()
  #     leafletProxy("map", data = tcp_party_pp ) %>% 
  #       addCircleMarkers(popup = content_pp, color = ~pal_grn_percent(tcp_party_pp$PercentGRN), stroke = FALSE, 
  #                        radius = 3, fillOpacity = 0.8, group = "Greens Primary")
  #     
  #     # leafletProxy("map") %>% 
  #     #   addLegend(position = "bottomleft")
  #     
  #     #      leafletProxy("map") %>% showGroup("Greens Primary") %>% 
  #     #        hideGroup("Two Party Preferred") %>% hideGroup("ALP Primary") %>% hideGroup("Coalition Primary")
  #   }
  #})
  
  #
  # Zoom to a particular division
  #
  observe({
    if(input$division != ""){
      div_shape <- bbox(boundaries_new_2016[boundaries_new_2016$ELECT_DIV == input$division,])
      leafletProxy("map") %>% 
        fitBounds(div_shape[1], div_shape[2], div_shape[3], div_shape[4])
    }
  })
  
  observe({
    filter_pps <- reactive({
      if(input$no_ppvc) {
        tcp_party_pp[!grepl("PPVC|Divisional Office (PREPOLL)", tcp_party_pp$PollingPlaceNm),]
      } else {
        tcp_party_pp
      }
    })

    data_pp <- filter_pps()

    pp_value <- reactive({
      if(input$polling_place_markers == "tpp") {
        return(data_pp$tppRelative)
      }
      if(input$polling_place_markers == "coa") {
        return(data_pp$PercentCOA)
      }
      if(input$polling_place_markers == "alp") {
        return(data_pp$PercentALP)
      }
      if(input$polling_place_markers == "grn") {
        return(data_pp$PercentGRN)
      }
      
    })
    
    colour_pp <- reactive({
      if(input$polling_place_markers == "tpp") {
        colour_pp_colours = c("Red", "White", "Blue")
      }
      if(input$polling_place_markers == "coa") {
        colour_pp_colours = "Blues"
      }
      if(input$polling_place_markers == "alp") {
        colour_pp_colours = "Reds"
      }
      if(input$polling_place_markers == "grn") {
        colour_pp_colours = "Greens"
      }
      return(colour_pp_colours)
    })
    
    pal <- colorNumeric(
      palette = colour_pp(),
      domain = pp_value()
    )
    
    content_pp <- paste(
      "<b>", htmlEscape(data_pp$PollingPlace),
      " (", htmlEscape(data_pp$PremisesSuburb), ", ", htmlEscape(data_pp$PremisesStateAb), ")</b><br />",
      "Division of ", data_pp$DivisionNm.x, "<br>",
      "ALP primary vote: ", data_pp$ALP, " (", round(data_pp$PercentALP, digits = 1), "%)<br />",
      "ALP Two Party Preferred: ", round(data_pp$tppPercentALP * 100, digits = 1), "%)<br />",
      "Coalition primary vote: ", data_pp$COA, " (", round(data_pp$PercentCOA, digits = 1), "%)<br />",
      "Coalition Two Party Preferred: ", round(data_pp$tppPercentCOA * 100, digits = 1), "%)<br />",
      "Greens primary vote: ", data_pp$GRN, " (", round(data_pp$PercentGRN, digits = 1), "%)<br />",
      "Total votes: ", data_pp$TotalVotes, "<br />",
      sep = ""
    )
    
    
    #leafletProxy("map") %>% clearMarkers()
    leafletProxy("map", data = data_pp) %>% 
      addCircleMarkers(popup = content_pp, color = ~pal(pp_value()), stroke = FALSE, 
                       radius = 3, fillOpacity = 0.8)
    
  })
}



shinyApp(ui, server)