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
#
# ReferenceSpace
# x: anterior -> posterior
# y: superior -> inferior
# z: left -> right


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
  centroid_x <- xml_find_first(api_xml, "//x")
  centroid_y <- xml_find_first(api_xml, "//y")
  
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
  # example API
  api_query <- 'http://api.brain-map.org/api/v2/image_to_reference/100960240.xml?x=5909.71681846069&y=3515.56709735465'
  
  api_xml <- read_xml(api_query)
  
  # in microns
  ref_x <- xml_find_first(api_xml, "//x")
  ref_y <- xml_find_first(api_xml, "//y")
  ref_z <- xml_find_first(api_xml, "//z")
  
  #result checking
  cat("ref_x:\t", xml_text(ref_x))
  cat("ref_y:\t", xml_text(ref_y))
  cat("ref_z:\t", xml_text(ref_z))
  
}



# using Reference-to-Image synchronization,
# generate a list of image ids of atlas
# NOTE: x increases anterior -> posterior
#
# INPUT
# x,y,z:  (x,y,z) of centroid
# n:  required list size / 2
# s:  space between two images in microns
get_synced_atlas_images <- function(x, y, z, n, s) {
  
  library("xml2")
  
  coronal_x <- seq(from = x + n * s, to = x - n * s, by = (-1) * s)
  atlas_list <- c(-1,-1,-1)  # will be the last line of the list
  
  for (i in coronal_x) {
    api_query <- paste0('http://api.brain-map.org/api/v2/reference_to_image/10.xml?x=',i,'&y=',y,'&z=',z,'&section_data_set_ids=100048576')
    
    api_xml <- read_xml(api_query)
    
    # generate a vector for an image
    id <- xml_find_first(api_xml, "//section-image-id")
    ix <- xml_find_first(api_xml, "//x")
    iy <- xml_find_first(api_xml, "//y")
    
    v <- c(xml_text(id), xml_text(ix), xml_text(iy))
    print(v)
    atlas_list <- rbind(v,atlas_list)
  }

}

get_synced_atlas_images(6951, 3230, 6159, 3, 100)


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