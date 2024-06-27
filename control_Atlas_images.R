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
# return a list of (id,x,y) for each atlas image
# NOTE: x increases anterior -> posterior
#
# INPUT
# x,y,z:  (x,y,z) of centroid
# n:  required list size / 2
# s:  space between two images in microns
get_synced_atlas_images <- function(x, y, z, n, s) {
  
  library("xml2")
  
  coronal_x <- seq(from = x - n * s, to = x + n * s, by = s)
  atlas_list <- c(-1,-1,-1)  # first line of the list
  
  for (i in coronal_x) {
    api_query <- paste0('http://api.brain-map.org/api/v2/reference_to_image/10.xml?x=',i,'&y=',y,'&z=',z,'&section_data_set_ids=100048576')
    
    api_xml <- read_xml(api_query)
    
    # generate a vector for an image
    id <- xml_find_first(api_xml, "//section-image-id")
    ix <- xml_find_first(api_xml, "//x")
    iy <- xml_find_first(api_xml, "//y")
    
    v <- c(xml_text(id), xml_text(ix), xml_text(iy))
    atlas_list <- rbind(atlas_list, v)
    
  }
  
  # test
  print(atlas_list)
  
  return(atlas_list)

}




# using Image-to-Image synchronization,
# return a list of (id,x,y) for a single gene
# which is synced to the atlas list
#
# INPUT
# list: synced atlas images
#       each component includes (id, x, y) in str
#       first row is (-1,-1,-1)
# id: gene experiment's ID
get_synced_gene_images <- function(list, setid) {
  
  library("xml2")
  
  gene_list <- c(-1,-1,-1)
  
  for (i in 2:nrow(list)) {
    
    api_query <- paste0('http://api.brain-map.org/api/v2/image_to_image/',list[i,1],".xml?x=",list[i,2],"&y=2368&section_data_set_ids=",setid)
    
    api_xml <- read_xml(api_query)
    
    # generate a vector for an image
    id <- xml_find_first(api_xml, "//section-image-id")
    ix <- xml_find_first(api_xml, "//x")
    iy <- xml_find_first(api_xml, "//y")
    
    v <- c(xml_text(id), xml_text(ix), xml_text(iy))
    gene_list <- rbind(gene_list, v)
    
  }
  
  # test
  print(gene_list)
  
}

# test
get_synced_gene_images(get_synced_atlas_images(6951, 3230, 6159, 3, 100),67810540)
