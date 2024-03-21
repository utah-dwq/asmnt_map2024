setwd("/Users/alanochoa/Documents/GitHub/asmnt_map2024/")
library(leaflet)

#Does the main directory being referenced here need to be commented out for hosting on GitHub Pages? Just use second direct directory?
#assessments=read.csv(file="/Users/alanochoa/Documents/GitHub/IR-2024/2024Draft/2024_IR_draft.csv")
assessments=read.csv(file="2024_IR_draft.csv")


assessments=within(assessments, {
	au_label=paste0(
		"AU name: ", ASSESSMENT_UNIT_NAME, "<br />",
		"AU ID: ", ASSESSMENT_UNIT_ID, "<br />",
		"Numeric category: ", EPA_IR_CATEGORY_ID, "<br />",
		"DWQ category: ", DWQ_Category,"<br />",
		HNNC
	)
	param_label=paste0(PARAMETER_CODE_NAME,": ", PARAMETER_ATTAINMENT, ", ", Parameter_Status)
	
})

au_param_labs=aggregate(param_label~ASSESSMENT_UNIT_ID+EPA_IR_CATEGORY_ID+DWQ_Category+au_label, assessments, paste, collapse = "<br/>")
au_param_labs=within(au_param_labs, {
	lab=paste0(au_label,"<br/> Parameters:<br/> ", param_label)
	lab=gsub("<br/> Parameters:<br/> NA: NA, NA", "", lab)
})

aup=wqTools::au_poly

suppressWarnings({centroids=sf::st_centroid(aup)})

# Note: removing these 2 never assessed AU polygons
#subset(aup, !ASSESS_ID %in% au_param_labs$ASSESSMENT_UNIT_ID)

aup=merge(aup, au_param_labs, by.x="ASSESS_ID", by.y="ASSESSMENT_UNIT_ID")
if(!dim(aup)[1]==length(unique(assessments$ASSESSMENT_UNIT_ID))){stop("Lengths do not match. Check aggregation and merging")}

map=leaflet()%>%
  addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
  addMapPane("au_poly", zIndex = 415)  %>%
  addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="3",],group="Cat 3: Insufficient data",fillOpacity = 0.3,weight=2,color="#a6a6a6", options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="5",],group="Cat 5: Not supporting, TMDL required",fillOpacity = 0.3,weight=2,color="#e41a1c", options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="4A",],group="Cat 4A: Approved TMDL",fillOpacity = 0.3,weight=2,color="#984ea3", options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="2",],group="Cat 2: No evidence of impairment",fillOpacity = 0.3,weight=2,color="#255d8a", options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addPolygons(data=aup[aup$EPA_IR_CATEGORY_ID=="1",],group="Cat 1: Fully Supporting",fillOpacity = 0.3,weight=2,color="#118a11", options = pathOptions(pane = "au_poly"),
              popup=~lab
  ) %>% 
  addCircles(data = centroids, group = "AUID",stroke=F, fill=F, label=~ASSESS_ID,
             popup = aup$ASSESS_ID) %>%
  addCircles(data = centroids, group = "AUName",stroke=F, fill=F, label=~AU_NAME,
             popup = aup$AU_NAME)%>%
  addLayersControl(position ="topleft",overlayGroups = c("Cat 1: Fully Supporting", "Cat 2: No evidence of impairment", "Cat 3: Insufficient data", "Cat 4A: Approved TMDL", "Cat 5: Not supporting, TMDL required"),
                                options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))%>%
  addLegend("topright", 
	colors=c("#118a11", "#255d8a", "#a6a6a6", "#984ea3", "#e41a1c"), 
	labels = c("Cat 1: Fully Supporting", "Cat 2: No evidence of impairment", "Cat 3: Insufficient data", "Cat 4A: Approved TMDL", "Cat 5: Not supporting, TMDL required"),
	title = "Assessment Category",opacity = 0.6
  )%>%
  wqTools::addMapResetButton()%>%
  leaflet.extras::addSearchFeatures(
    targetGroups = c('AUID','AUName'),
    options = leaflet.extras::searchFeaturesOptions(
      zoom=12, openPopup = FALSE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))

htmlwidgets::saveWidget(map, "index.html", title="Utah 2024 IR submission")
getwd()
####################################################
# To deploy:
# run code and export save map as html widget ("index.html")
# examine the index.html file locally
# Commit changes to GitHub repo master branch
# Deployed map should update in a minute or two





