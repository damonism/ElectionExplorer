library(shiny)
library(leaflet)
library(htmltools)

##
## TODO:
## - Slider to only show polling places of a certain size
## - Fix up the UI.
## - Pre-poll and super-booth toggle

ui <- fluidPage(
  # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # 
  # leafletOutput("map", width = "100%", height = "100%"),
  # absolutePanel(
  #   radioButtons("polling_place_markers", "Polling Place Markers",
  #                choices = list("Two Party Preferred" = "tpp", 
  #                               "Coalition Primary" = "coa", 
  #                               "ALP Primary" = "alp", 
  #                               "Greens Primary" = "grn"),
  #                selected = "tpp"),
  #   checkboxGroupInput("boundaries", "Boundaries",
  #                      choices = list("2016 Notional Margin" = "margin", 
  #                                     "2016 Boundary" = "outline"),
  #                      selected = "margin"),
  #   #checkboxInput("no_ppvc", "Exclude pre-polls"),
  #   selectInput("division", "Zoom to Division", c("Select a division" = "", levels(boundaries_new_2016$ELECT_DIV)))
  # )
    
  sidebarLayout(
    sidebarPanel(
          radioButtons("polling_place_markers", "Polling Place Markers",
                       choices = list("Two Party Preferred" = "tpp",
                                      "Coalition Primary" = "coa",
                                      "ALP Primary" = "alp",
                                      "Greens Primary" = "grn"),
                       selected = "tpp"),
          checkboxGroupInput("boundaries", "Boundaries",
                             choices = list("2016 Notional Margin" = "margin",
                                            "2016 Boundary" = "outline"),
                             selected = "margin"),
          #checkboxInput("no_ppvc", "Exclude pre-polls"),
          selectInput("division", "Zoom to Division", c("Select a division" = "", levels(boundaries_new_2016$ELECT_DIV)))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(boundaries_new_2016) %>% 
      addTiles() %>% 
#      addProviderTiles("CartoDB.Positron") %>% 
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
    
    
  })
  
  ##
  ## Draw the two-party preferred polling place markers
  ##
  
  observe({
    
    # Filter out pre-polls (if selected)
    # tcp_party_pp <- reactive({
    #   if(input$no_ppvc) {
    #     tcp_party_pp[!grepl("PPVC|Divisional Office (PREPOLL)", tcp_party_pp$PollingPlaceNm),]
    #   } else {
    #     tcp_party_pp
    #   }
    # })

    pal_pp_alp <- colorNumeric(
      palette = "Reds",
      domain = tcp_pp_alp$tppPercentALP
    )
    
    pal_pp_coa <- colorNumeric(
      palette = "Blues",
      domain = tcp_pp_coa$tppPercentCOA
    )
    
    pal_grn_percent <- colorNumeric(
      palette = "Greens",
      domain = tcp_party_pp$PercentGRN
    )
    
    pal_alp_percent <- colorNumeric(
      palette = "Reds",
      domain = tcp_party_pp$PercentALP
    )
    
    pal_coa_percent <- colorNumeric(
      palette = "Blues",
      domain = tcp_party_pp$PercentCOA
    )
    
    content_tcp_alp <- paste(
      "<b>", tcp_pp_alp$PollingPlace, " (", tcp_pp_alp$PremisesSuburb, ", ", tcp_pp_alp$PremisesStateAb, ")</b><br />",
      "Division of ", tcp_pp_alp$DivisionNm.x, "<br>",
      "ALP primary vote: ", tcp_pp_alp$ALP, " (", round(tcp_pp_alp$PercentALP, digits = 1), "%)<br />",
      "ALP Two Party Preferred: ", round(tcp_pp_alp$tppPercentALP * 100, digits = 1), "%<br />",
      "Coalition primary vote: ", tcp_pp_alp$COA, " (", round(tcp_pp_alp$PercentCOA, digits = 1), "%)<br />",
      "Coalition Two Party Preferred: ", round(tcp_pp_alp$tppPercentCOA * 100, digits = 1), "%<br />",
      "Greens primary vote: ", tcp_pp_alp$GRN, " (", round(tcp_pp_alp$PercentGRN, digits = 1), "%)<br />",
      "Total votes: ", tcp_pp_alp$TotalVotes, "<br />",
      sep = ""
    )
    
    content_tcp_coa <- paste(
      "<b>", tcp_pp_coa$PollingPlace, " (", tcp_pp_coa$PremisesSuburb, ", ", tcp_pp_coa$PremisesStateAb, ")</b><br />",
      "Division of ", tcp_pp_coa$DivisionNm.x, "<br>",
      "ALP primary vote: ", tcp_pp_coa$ALP, " (", round(tcp_pp_coa$PercentALP, digits = 1), "%)<br />",
      "ALP Two Party Preferred: ", round(tcp_pp_coa$tppPercentALP * 100, digits = 1), "%<br />",
      "Coalition primary vote: ", tcp_pp_coa$COA, " (", round(tcp_pp_coa$PercentCOA, digits = 1), "%)<br />",
      "Coalition Two Party Preferred: ", round(tcp_pp_coa$tppPercentCOA * 100, digits = 1), "%<br />",
      "Greens primary vote: ", tcp_pp_coa$GRN, " (", round(tcp_pp_coa$PercentGRN, digits = 1), "%)<br />",
      "Total votes: ", tcp_pp_coa$TotalVotes, "<br />",
      sep = ""
    )
    
    # Content bubble for COA/ALP/GRN polling places
    content_pp <- paste(
      "<b>", htmlEscape(tcp_party_pp$PollingPlace), 
      " (", htmlEscape(tcp_party_pp$PremisesSuburb), ", ", htmlEscape(tcp_party_pp$PremisesStateAb), ")</b><br />",
      "Division of ", tcp_party_pp$DivisionNm.x, "<br>",
      "ALP primary vote: ", tcp_party_pp$ALP, " (", round(tcp_party_pp$PercentALP, digits = 1), "%)<br />",
      "ALP Two Party Preferred: ", round(tcp_party_pp$tppPercentALP * 100, digits = 1), "%)<br />",
      "Coalition primary vote: ", tcp_party_pp$COA, " (", round(tcp_party_pp$PercentCOA, digits = 1), "%)<br />",
      "Coalition Two Party Preferred: ", round(tcp_party_pp$tppPercentCOA * 100, digits = 1), "%)<br />",
      "Greens primary vote: ", tcp_party_pp$GRN, " (", round(tcp_party_pp$PercentGRN, digits = 1), "%)<br />",
      "Total votes: ", tcp_party_pp$TotalVotes, "<br />",
      sep = ""
    )
    
    #
    # Select which layer to show
    #
    
    # Show the Two Party Preferred
    if(input$polling_place_markers == "tpp"){
      leafletProxy("map") %>% clearMarkers()
      leafletProxy("map", data = tcp_pp_alp ) %>% 
        addCircleMarkers(popup = content_tcp_alp, color = ~pal_pp_alp(tcp_pp_alp$tppPercentALP), stroke = FALSE, 
                         radius = 3, fillOpacity = 0.8, group = "Two Party Preferred")
      
      leafletProxy("map", data = tcp_pp_coa ) %>% 
        addCircleMarkers(popup = content_tcp_coa, color = ~pal_pp_coa(tcp_pp_coa$tppPercentCOA), stroke = FALSE, 
                         radius = 3, fillOpacity = 0.8, group = "Two Party Preferred")
      
      #            leafletProxy("map") %>% showGroup("Two Party Preferred") %>% 
      #        hideGroup("Coalition Primary") %>% hideGroup("ALP Primary") %>% hideGroup("Greens Primary")
      
    }
    
    # Show the Coalition
    if(input$polling_place_markers == "coa"){
      leafletProxy("map") %>% clearMarkers()
      
      leafletProxy("map", data = tcp_party_pp ) %>% 
        addCircleMarkers(popup = content_pp, color = ~pal_coa_percent(tcp_party_pp$PercentCOA), stroke = FALSE, 
                         radius = 3, fillOpacity = 0.8, group = "Coalition Primary")
      
      
      #      leafletProxy("map") %>% showGroup("Coalition Primary") %>% 
      #        hideGroup("Two Party Preferred") %>% hideGroup("ALP Primary") %>% hideGroup("Greens Primary")
    }
    
    # Show the ALP
    if(input$polling_place_markers == "alp"){
      
      leafletProxy("map") %>% clearMarkers()
      leafletProxy("map", data = tcp_party_pp ) %>% 
        addCircleMarkers(popup = content_pp, color = ~pal_alp_percent(tcp_party_pp$PercentALP), stroke = FALSE, 
                         radius = 3, fillOpacity = 0.8, group = "ALP Primary")
      #      leafletProxy("map") %>% showGroup("ALP Primary") %>% 
      #        hideGroup("Two Party Preferred") %>% hideGroup("Coalition Primary") %>% hideGroup("Greens Primary")
    }
    
    # Show the Greens
    if(input$polling_place_markers == "grn"){
      
      leafletProxy("map") %>% clearMarkers()
      leafletProxy("map", data = tcp_party_pp ) %>% 
        addCircleMarkers(popup = content_pp, color = ~pal_grn_percent(tcp_party_pp$PercentGRN), stroke = FALSE, 
                         radius = 3, fillOpacity = 0.8, group = "Greens Primary")
      
      #      leafletProxy("map") %>% showGroup("Greens Primary") %>% 
      #        hideGroup("Two Party Preferred") %>% hideGroup("ALP Primary") %>% hideGroup("Coalition Primary")
    }
    
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
  })
}



shinyApp(ui, server)