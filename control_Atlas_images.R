# Find Atlas images
# for a specific structure or given coordinates
#
# Results should include
# 1) list - Atlas images' ID (in order)
# 2) list - synchronized (x,y) for each image
#
# additional function
# - download image series for required size


# USEFUL IMFORMATION
# data_set_id:  100048576
# 132 coronal P56 images
# path:         9907042031 ~ 9907042162 (1)
# section no.:  1 ~ 527 (4) {(81,86),(522,527)}


# for given structure id,
# return a vector (centroid_image_id, centroid_x, centroid_y) **not yet
# INPUT
# struct_id:  ID of structure
#             186 for LH
get_centroid_id_xy <- function(struct_id = 186) {
  
  library("xml2")
  
  api_query <- paste("http://api.brain-map.org/api/v2/structure_to_image/100048576.xml?structure_ids=",as.character(struct_id), sep="")
  
  api_xml <- read_xml(api_query)
  
  atlas_image_id <- xml_find_all(api_xml, "//section-image-id")
  centroid_x <- xml_find_all(api_xml, "//x")
  centroid_y <- xml_find_all(api_xml, "//y")
  
  # result checking
  cat("atlas_image_id:\t", xml_text(atlas_image_id))
  cat("centroid_x:\t", xml_text(centroid_x))
  cat("centroid_y:\t", xml_text(centroid_y))
  
}


# using Image-to-Reference synchronization,
# find (x,y,z) in the Reference Space
#
# INPUT
# id: atlas image id
# x:  centroid_x
# y:  centroid_y
get_xyz <- function(id, x, y) {
  
  library("xml2")
  
  api_query <- paste("http://api.brain-map.org/api/v2/image_to_reference/",
                     as.character(id),
                     ".xml?x=", as.character(x),
                     "&y=", as.character(y), sep = "")
  # example api
  api_query <- 'http://api.brain-map.org/api/v2/image_to_reference/100960240.xml?x=5909.71681846069&y=3515.56709735465'
  
  api_xml <- read_xml(api_query)
  
  # in microns
  ref_x <- xml_find_all(api_xml, "//x")
  ref_y <- xml_find_all(api_xml, "//y")
  ref_z <- xml_find_all(api_xml, "//z")
  
  #result checking
  cat("ref_x:\t", xml_text(ref_x))
  cat("ref_y:\t", xml_text(ref_y))
  cat("ref_z:\t", xml_text(ref_z))
  
}


# using Reference-to-Image synchronization,
# generate a list of image ids of atlas
#
# INPUT
# id: ID of centroid image
# n:  required list size (odds are recommended)
# s:  space between two images in microns
get_image_ids <- function(id, n, s=10) {
  
  library("xml2")
  
  
  
}


# using Image-to_Image API,
# make a list of size 2n+1
# with IDs, (x,y) for each
#
# INPUT
# id = image id
# x,y = (x,y)
# (from get_centroid_id_xy result)
# 
get_synced_image_list <- function(id, x, y) {
  
  library("xml2")
  
  # half number of images
  # result list size would be doubled
  n = 3
  
  
  
  api_query <- paste("http://api.brain-map.org/api/v2/image_to_image/",as.character(id),
                     ".xml?x=",as.character(x),
                     "&y=",as.character(y),
                     "&section_data_set_ids=100048576", sep="")
  
  api_xml <- read_xml(api_query)
  
  api_ids <- xml_find_all(api_xml,"//id")
  
  xml_text(api_ids)
}