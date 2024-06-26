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
  
  cat("atlas_image_id:\t", xml_text(atlas_image_id))
  cat("centroid_x:\t", xml_text(centroid_x))
  cat("centroid_y:\t", xml_text(centroid_y))
}

# INPUT
# n = number of images
# 
get_list_of_Atlas <- function() {
  
  library("xml2")
  
  api_query <- 'http://api.brain-map.org/api/v2/data/query.xml?criteria=model::AtlasImage,rma::criteria,[annotated$eqtrue],atlas_data_set(atlases[id$eq1]),alternate_images[image_type$eq%27Atlas+-+Adult+Mouse%27],rma::options[order$eq%27sub_images.section_number%27][num_rows$eqall]'
  
  api_xml <- read_xml(api_query)
  
  api_ids <- xml_find_all(api_xml,"//id")
  
  xml_text(api_ids)
}