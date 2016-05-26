library(sp)
library(rgdal)

##
## Download the 2013 results by polling place
##

# 2013 First HoR First Preference by PP
hor_2013 <- read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-NSW.csv", header=TRUE, skip=1, sep=",")
hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-VIC.csv", header=TRUE, skip=1, sep=","))
hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-QLD.csv", header=TRUE, skip=1, sep=","))
hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-WA.csv", header=TRUE, skip=1, sep=","))
hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-SA.csv", header=TRUE, skip=1, sep=","))
hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-TAS.csv", header=TRUE, skip=1, sep=","))
hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-ACT.csv", header=TRUE, skip=1, sep=","))
hor_2013 <- rbind(hor_2013, read.csv2("http://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-NT.csv", header=TRUE, skip=1, sep=","))
hor_2013$Election <- "2013"

# It's probably easier to use PartyAb than PartyNm, but put "Informal" in the otherwise
# black PartyAb for informal votes.

hor_2013$PartyAb <- as.character(hor_2013$PartyAb)
hor_2013$PartyAb[hor_2013$PartyNm == "Informal"] <- "Informal"
hor_2013$PartyAb <- as.factor(hor_2013$PartyAb)

library(reshape2)
result_by_pp_2013 <- dcast(hor_2013, StateAb + DivisionNm + PollingPlace + PollingPlaceID ~ PartyAb, sum, value.var = "OrdinaryVotes") 
result_by_pp_2013$TotalVotes <- rowSums(result_by_pp_2013[,!colnames(result_by_pp_2013) %in% c("StateAb", "DivisionNm", "PollingPlace", "PollingPlaceID")])

# Don't need any of this any more because I have the fixed 2016 map from Toby
#
# boundaries_aust_2013 <- readOGR("//home4/mullerd$/My Documents/Data/2013 Commonwealth Boundaries", "COM20111216_ELB_region")
# boundaries_nsw_2016 <- readOGR("//home4/mullerd$/My Documents/Data/2016 Commonwealth Boundaries", "NSW_Electoral_Boundaries_25-02-2016")
# boundaries_wa_2016 <- readOGR("//home4/mullerd$/My Documents/Data/2016 Commonwealth Boundaries/WA Final", "WA_Electoral_Boundaries_19-01-2016")
# boundaries_act_2016 <- readOGR("//home4/mullerd$/My Documents/Data/2016 Commonwealth Boundaries/ACT Final", "ACT_Electoral_Boundaries_28-01-2016")
# 
# boundaries_old_2016 <- boundaries_aust_2013[!boundaries_aust_2013$STATE == "NSW",]
# boundaries_old_2016 <- boundaries_old_2016[!boundaries_old_2016$STATE == "ACT",]
# boundaries_old_2016 <- boundaries_old_2016[!boundaries_old_2016$STATE == "WA",]
# 
# boundaries_nsw_2016$STATE <- factor("NSW")
# boundaries_wa_2016$STATE <- factor("WA")
# boundaries_act_2016$STATE <- factor("ACT")
# 
# # Make sure all of the shapefiles are on the same page
# proj4string(boundaries_wa_2016) <- proj4string(boundaries_aust_2013)
# proj4string(boundaries_act_2016) <- proj4string(boundaries_aust_2013)
# proj4string(boundaries_nsw_2016) <- proj4string(boundaries_aust_2013)
# 
# # Combine all of the new division maps.
# boundaries_new_2016 <- rbind(boundaries_wa_2016, boundaries_act_2016, boundaries_nsw_2016, makeUniqueIDs = TRUE)

# Toby's fixed 2016 boundaries
boundaries_new_2016 <- readOGR("../2016 Commonwealth Boundaries/Fixed", "COM_ELB_20160509")

# Reduce the complexity of the district boundary map (default is 46mb)
library(rgeos)
boundaries_big_2016 <- boundaries_new_2016
tmp_data <- data.frame(boundaries_big_2016)
tmp_shape <- gSimplify(boundaries_big_2016, tol = .01, topologyPreserve = TRUE)
boundaries_new_2016 <- SpatialPolygonsDataFrame(tmp_shape, tmp_data)
rm(tmp_shape)
rm(tmp_data)

# # How to very quickly check a polygon in leaflet (via @tobybellwood)
# library(leaflet)
# leaflet() %>% addTiles() %>% addPolygons(data = boundaries_new_2016)

# The variable names are different in the redistribution maps, so we need to rename them
names(boundaries_new_2016)[names(boundaries_new_2016) == "Elect_div"] <- "ELECT_DIV"
names(boundaries_new_2016)[names(boundaries_new_2016) == "Actual"] <- "ACTUAL"
names(boundaries_new_2016)[names(boundaries_new_2016) == "Numccds"] <- "NUMCCDS"
names(boundaries_new_2016)[names(boundaries_new_2016) == "Area_SqKm"] <- "AREA_SQKM"
names(boundaries_new_2016)[names(boundaries_new_2016) == "Projected"] <- "PROJECTED"
names(boundaries_new_2016)[names(boundaries_new_2016) == "Sortname"] <- "SORTNAME"
names(boundaries_new_2016)[names(boundaries_new_2016) == "Total_Popu"] <- "POPULATION"

# # Add the new boundaries to the old boundaries, skipping some variables that aren't in both.
# boundaries_2016 <- rbind(boundaries_new_2016[, c(-1, -7)], boundaries_old_2016[,-7], makeUniqueIDs = TRUE)

# Some of the division names are not capitalised properly, and this messes up the merge with the notional status
levels(boundaries_new_2016$ELECT_DIV) <- gsub("Mcpherson", "McPherson", levels(boundaries_new_2016$ELECT_DIV))
levels(boundaries_new_2016$ELECT_DIV) <- gsub("Mcmillan", "McMillan", levels(boundaries_new_2016$ELECT_DIV))

# Add the notional status
notional_status <- read.csv("Notional.csv")
boundaries_new_2016 <- merge(boundaries_new_2016, notional_status)

# Okay, worked out what I did wrong last time is I need to make two sets of polygons, one for ALP and one for Coalition

boundaries_2016_coalition <- boundaries_new_2016[boundaries_new_2016$Notional == "Coalition",]
boundaries_2016_alp <- boundaries_new_2016[boundaries_new_2016$Notional == "ALP",]
boundaries_2016_nonclassic <- boundaries_new_2016[boundaries_new_2016$Notional == "Non-Classic",]

# Invert the ALP margins (turn them into minus margins)
boundaries_new_2016$RelMargin <- boundaries_new_2016$Margin
boundaries_new_2016$RelMargin[boundaries_new_2016$Notional == "ALP"] <- 0 - boundaries_new_2016$Margin[boundaries_new_2016$Notional == "ALP"]

##
## Polling places
##

## TODO: 
## - Collapse polling places at the same location together.
## - Mark abolished polling places and gather their votes into the nearest remaining [hard!]

pp_geo <- read.csv("http://results.aec.gov.au/17496/Website/Downloads/GeneralPollingPlacesDownload-17496.csv", header = TRUE, skip = 1)
tcp_by_pp <- read.csv("http://results.aec.gov.au/17496/Website/Downloads/HouseTcpByCandidateByPollingPlaceDownload-17496.csv", header = TRUE, skip = 1)

library(reshape2)
tcp_party_pp <- dcast(tcp_by_pp, PollingPlaceID + PollingPlace ~ PartyAb, value.var = "OrdinaryVotes")
tcp_party_pp$COA <- rowSums(cbind(tcp_party_pp$LP, tcp_party_pp$CLP, tcp_party_pp$LNP, tcp_party_pp$NP), na.rm = TRUE)
tcp_party_pp$TotalVotes <- tcp_party_pp$ALP + tcp_party_pp$COA
tcp_party_pp$PercentALP <- tcp_party_pp$ALP / tcp_party_pp$TotalVotes
tcp_party_pp$PercentCOA <- tcp_party_pp$COA / tcp_party_pp$TotalVotes

# Because we're going to merge this with the full polling place results by party, we need to identify these as TPP results by party
names(tcp_party_pp) <- gsub("^", "tpp", names(tcp_party_pp))
names(tcp_party_pp)[1] <- "PollingPlaceID"

# Merge TPP and Geocodes
tcp_party_pp <- merge(tcp_party_pp, pp_geo, by = "PollingPlaceID", all.x = TRUE)

# Merge this with the results
tcp_party_pp <- merge(tcp_party_pp, result_by_pp_2013, by = "PollingPlaceID", all.x = TRUE)

# We don't want polling places with no geocodes
tcp_party_pp <- tcp_party_pp[!is.na(tcp_party_pp$Longitude),]

# Some polling places have a dodgy geocode
tcp_party_pp <- tcp_party_pp[tcp_party_pp$Longitude > 0,]
# Dandenong West is on Christmas Island, for some reason
tcp_party_pp <- tcp_party_pp[tcp_party_pp$PollingPlaceID != 3965,]

# And the mapping is easier if we delete the Norfolk Island polling places (sorry Norfolk Island)
tcp_party_pp <- tcp_party_pp[tcp_party_pp$PremisesPostCode != 7140,]
# And Lord Howe Island
tcp_party_pp <- tcp_party_pp[tcp_party_pp$PremisesPostCode != 2898,]
# Home Island
tcp_party_pp <- tcp_party_pp[tcp_party_pp$PremisesPostCode != 6799,]
# Christmas Island
tcp_party_pp <- tcp_party_pp[tcp_party_pp$PremisesPostCode != 6798,]

# Also get rid of the polling places which don't have any votes
tcp_party_pp <- tcp_party_pp[tcp_party_pp$TotalVotes > 0,]

# Tell R what the Latitudes and Longitudes are
coordinates(tcp_party_pp) <- c("Longitude", "Latitude")
proj4string(tcp_party_pp) <- proj4string(boundaries_new_2016)

tcp_party_pp <- tcp_party_pp[!is.na(tcp_party_pp$PollingPlaceID),]

# Percantage results for ALP, Coalition, Greens
tcp_party_pp$PercentALP <- tcp_party_pp$ALP / tcp_party_pp$TotalVotes * 100
tcp_party_pp$PercentCOA <- (tcp_party_pp$LP + tcp_party_pp$NP + tcp_party_pp$CLP + tcp_party_pp$LNP) / tcp_party_pp$TotalVotes * 100
tcp_party_pp$PercentGRN <- tcp_party_pp$GRN / tcp_party_pp$TotalVotes * 100

# Determine relative margin (ALP with negative margins)
tcp_party_pp$tppRelative <- tcp_party_pp$tppPercentCOA - 0.5

# Break down polling places into TPP groups
tcp_pp_alp <- tcp_party_pp[!is.na(tcp_party_pp$tppPercentALP),]
tcp_pp_alp <- tcp_pp_alp[tcp_pp_alp$tppPercentALP >= 0.5,]

tcp_pp_coa <- tcp_party_pp[!is.na(tcp_party_pp$tppPercentCOA),]
tcp_pp_coa <- tcp_pp_coa[tcp_pp_coa$tppPercentCOA >= 0.5,]

##
## Draw the map
##

library(leaflet)

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

pal_pp_alp <- colorNumeric(
  palette = "Reds",
  domain = tcp_pp_alp$tppPercentALP
)

pal_pp_coa <- colorNumeric(
  palette = "Blues",
  domain = tcp_pp_coa$tppPercentCOA
)

content_alp <- paste(
  "<b>", boundaries_2016_alp$ELECT_DIV, "</b><br />",
  "Notional Party:", boundaries_2016_alp$Notional, "<br />",
  "Notional Margin:", round(boundaries_2016_alp$Margin, digits = 1), "<br />",
  "Held By:", boundaries_2016_alp$HeldBy, "<br />"
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

content_pp <- paste(
  "<b>", tcp_party_pp$PollingPlace, "(", tcp_party_pp$PremisesSuburb, ", ", tcp_party_pp$PremisesStateAb, ")</b><br />",
  "ALP primary vote: ", tcp_party_pp$ALP, " (", round(tcp_party_pp$PercentALP, digits = 1), "%)<br />",
  "ALP Two Party Preferred: ", round(tcp_party_pp$tppPercentALP * 100, digits = 1), "%)<br />",
  "Coalition primary vote: ", tcp_party_pp$COA, " (", round(tcp_party_pp$PercentCOA, digits = 1), "%)<br />",
  "Coalition Two Party Preferred: ", round(tcp_party_pp$tppPercentCOA * 100, digits = 1), "%)<br />",
  "Greens primary vote: ", tcp_party_pp$GRN, " (", round(tcp_party_pp$PercentGRN, digits = 1), "%)<br />",
  "Total votes: ", tcp_party_pp$TotalVotes, "<br />",
  sep = ""
)

content_tcp_coa <- paste(
  "<b>", tcp_pp_coa$PollingPlace, " (", tcp_pp_coa$PremisesSuburb, ", ", tcp_pp_coa$PremisesStateAb, ")</b><br />",
  "ALP primary vote: ", tcp_pp_coa$ALP, " (", round(tcp_pp_coa$PercentALP, digits = 1), "%)<br />",
  "ALP Two Party Preferred: ", round(tcp_pp_coa$tppPercentALP * 100, digits = 1), "%<br />",
  "Coalition primary vote: ", tcp_pp_coa$COA, " (", round(tcp_pp_coa$PercentCOA, digits = 1), "%)<br />",
  "Coalition Two Party Preferred: ", round(tcp_pp_coa$tppPercentCOA * 100, digits = 1), "%<br />",
  "Greens primary vote: ", tcp_pp_coa$GRN, " (", round(tcp_pp_coa$PercentGRN, digits = 1), "%)<br />",
  "Total votes: ", tcp_pp_coa$TotalVotes, "<br />",
  sep = ""
)

content_tcp_alp <- paste(
  "<b>", tcp_pp_alp$PollingPlace, " (", tcp_pp_alp$PremisesSuburb, ", ", tcp_pp_alp$PremisesStateAb, ")</b><br />",
  "ALP primary vote: ", tcp_pp_alp$ALP, " (", round(tcp_pp_alp$PercentALP, digits = 1), "%)<br />",
  "ALP Two Party Preferred: ", round(tcp_pp_alp$tppPercentALP * 100, digits = 1), "%<br />",
  "Coalition primary vote: ", tcp_pp_alp$COA, " (", round(tcp_pp_alp$PercentCOA, digits = 1), "%)<br />",
  "Coalition Two Party Preferred: ", round(tcp_pp_alp$tppPercentCOA * 100, digits = 1), "%<br />",
  "Greens primary vote: ", tcp_pp_alp$GRN, " (", round(tcp_pp_alp$PercentGRN, digits = 1), "%)<br />",
  "Total votes: ", tcp_pp_alp$TotalVotes, "<br />",
  sep = ""
)

# For some reason it doesn't render properly when I have the popups added.
# The reason seems to be that it's too complex when combined with the boundary maps.

m <- leaflet()

m %>%  
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = boundaries_2016_alp, color = ~pal_alp(boundaries_2016_alp$Margin), popup = content_alp, stroke = FALSE, fillOpacity = 0.7, group = "Notional Margin") %>%
  addPolygons(data = boundaries_2016_coalition, color = ~pal_coalition(boundaries_2016_coalition$Margin), popup = content_coalition, stroke = FALSE, fillOpacity = 0.7, group = "Notional Margin") %>%
  addPolygons(data = boundaries_2016_nonclassic, color = ~pal_nonclassic(boundaries_2016_nonclassic$Margin), popup = content_nonclassic, stroke = FALSE, fillOpacity = 0.7, group = "Notional Margin") %>%
  addPolylines(data = boundaries_new_2016, weight = 1, stroke = TRUE, opacity = 1, color = "Black", group = "2016 Boundary") %>%
  addCircleMarkers(data = tcp_pp_alp, popup = content_tcp_alp, color = ~pal_pp_alp(tcp_pp_alp$tppPercentALP), stroke = FALSE, radius = 4, fillOpacity = 0.6, group = "Two Party Preferred") %>%
  addCircleMarkers(data = tcp_pp_coa, popup = content_tcp_coa, color = ~pal_pp_coa(tcp_pp_coa$tppPercentCOA), stroke = FALSE, radius = 4, fillOpacity = 0.6, group = "Two Party Preferred") %>%
  addCircleMarkers(data = tcp_party_pp, popup = content_pp, color = ~pal_coa_percent(tcp_party_pp$PercentCOA), stroke = FALSE, radius = 4, fillOpacity = 0.6, group = "Coalition Primary") %>%
  addCircleMarkers(data = tcp_party_pp, popup = content_pp, color = ~pal_alp_percent(tcp_party_pp$PercentALP), stroke = FALSE, radius = 4, fillOpacity = 0.6, group = "ALP Primary") %>%
  addCircleMarkers(data = tcp_party_pp, popup = content_pp, color = ~pal_grn_percent(tcp_party_pp$PercentGRN), stroke = FALSE, radius = 4, fillOpacity = 0.6, group = "Greens Primary") %>%
  hideGroup("2016 Boundary") %>%
  addLayersControl(
    baseGroups = c("Two Party Preferred", "Coalition Primary", "ALP Primary", "Greens Primary"),
    overlayGroups = c("Notional Margin", "2016 Boundary"),
    options = layersControlOptions(collapsed = FALSE)
  )


# Delete the stuff we don't need any more

# rm(boundaries_new_2016)
# rm(m)
