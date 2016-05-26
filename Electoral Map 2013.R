require(sp)
require(rgdal)

# Load the reduced detail map of CEDs from Mapshaper.org
aust_2013 <- readOGR("//home4/mullerd$/My Documents/Data/2013 Commonwealth Boundaries/Reduced", "COM20111216_ELB_region")
tpp_2013 <- read.csv("//HOME4/mullerd$/My Documents/Data/2013 Federal Election/2013 HoR TPP by Division.csv")
aust_2013_tpp <- merge(aust_2013, tpp_2013, by.x = "ELECT_DIV", by.y = "DivisionNm")

aust_2013_tpp_coalition <- aust_2013_tpp[aust_2013_tpp$PartyAb %in% c("LP", "NP", "LNP"),]
aust_2013_tpp_labor <- aust_2013_tpp[aust_2013_tpp$PartyAb %in% c("ALP", "CLP"),]

library(leaflet)

pal_coalition <- colorNumeric(
  palette = "Blues", 
  domain = c(48,74)
)

pal_labor <- colorNumeric(
  palette = "Reds", 
  domain = c(48,74)
)

content_coalition <- paste(
  "<b>", aust_2013_tpp_coalition$ELECT_DIV, "</b><br/>",
  "Party: ", aust_2013_tpp_coalition$PartyAb, "<br/>",
  "Coalition TPP Votes: ", aust_2013_tpp_coalition$Liberal.National.Coalition.Votes, "<br/>",
  "ALP TPP Votes: ", aust_2013_tpp_coalition$Australian.Labor.Party.Votes, "<br/>",
  "Coalition TPP Percentage: ", aust_2013_tpp_coalition$Liberal.National.Coalition.Percentage, "&#37;<br/>",
  "ALP TPP Percentage: ", aust_2013_tpp_coalition$Australian.Labor.Party.Percentage, "&#37;<br/>"
)

content_labor <- paste(
  "<b>", aust_2013_tpp_labor$ELECT_DIV, "</b><br/>",
  "Party: ", aust_2013_tpp_labor$PartyAb, "<br/>",
  "Coalition TPP Votes: ", aust_2013_tpp_labor$Liberal.National.Coalition.Votes, "<br/>",
  "ALP TPP Votes: ", aust_2013_tpp_labor$Australian.Labor.Party.Votes, "<br/>",
  "Coalition TPP Percentage: ", aust_2013_tpp_labor$Liberal.National.Coalition.Percentage, "&#37;<br/>",
  "ALP TPP Percentage: ", aust_2013_tpp_labor$Australian.Labor.Party.Percentage, "&#37;<br/>"
)

m <- leaflet()

m %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = aust_2013_tpp_coalition, color = ~pal_coalition(Liberal.National.Coalition.Percentage), popup = content_coalition, stroke = FALSE) %>% 
  addPolygons(data = aust_2013_tpp_labor, color = ~pal_labor(Australian.Labor.Party.Percentage), popup = content_labor, stroke = FALSE)