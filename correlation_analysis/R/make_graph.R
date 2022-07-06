library(sf)
library(here)
library(igraph)
library(dplyr)
library(visNetwork)
library(tmap)
# Import data -------------------------------------------------------------

watersheds.file <- here::here("data", "watersheds_fixed_geometry.geojson")

watersheds <- sf::st_read(watersheds.file)


# Make graph --------------------------------------------------------------

# get from-to list
watersheds.df <- watersheds %>% as.data.frame() %>% select(-geometry)
nodes <- data.frame(id = watersheds.df$Ph2_SWSID %>% as.numeric()) %>% arrange(id)
edges <- data.frame(from = watersheds.df$Ph2_SWSID %>% as.numeric(),
                    to = watersheds.df$Ph2_DSID %>% as.numeric()
                      ) %>% arrange(from)
visNetwork(nodes, edges, height = "500px") %>%
  visIgraphLayout() %>%
  visNodes(size = 10)



# scratch -----------------------------------------------------------------
watersheds.df$SWSID_in_DSID <- watersheds.df$Ph2_SWSID %in% watersheds.df$Ph2_DSID
watersheds.df$DSID_in_SWSID <- watersheds.df$Ph2_DSID %in% watersheds.df$Ph2_SWSID

watersheds.subset <- watersheds %>% filter(DSID_in_SWSID==TRUE)
watersheds.subset.df <- watersheds %>% as.data.frame() %>% select(-geometry)

nodes <- data.frame(id = watersheds.subset.df$Ph2_SWSID %>% as.numeric())
edges <- data.frame(from = watersheds.subset.df$Ph2_SWSID %>% as.numeric(),
                    to = watersheds.subset.df$Ph2_DSID %>% as.numeric()
)
visNetwork(nodes, edges, height = "500px") %>%
  visIgraphLayout() %>%
  visNodes(size = 10)


tm_shape(watersheds.subset) + tm_fill("DSID_in_SWSID") #dsid is a subset of swsid

