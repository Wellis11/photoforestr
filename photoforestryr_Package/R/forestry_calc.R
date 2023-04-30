#' PhotoDBH
#'
#' Convert photo pixel counts to Diameter at Breast Height (DBH)
#' @param pixels The number of pixels occupied by the image of a tree trunk at 1.3 m height
#' @param distx The distance between the camera and the tree trunk measured in m
#' @return The diameter of the tree in cm
#' @examples 
#' diam1 <- PhotoDBH(50, 1.2);
#' @export
PhotoDBH <- function(pixels, distx){
  DBH <- pixels/(33.198*(distx^-1.018)); 
  return(DBH);
}
#' BasalArea
#'
#' Calculates the Basal Area of each tree in cm^2
#' @param DBH The width of the tree trunk at 1.3 m height measured in cm
#' @param distx The distance between the camera and the tree trunk measured in m
#' @return The cross-sectional area of the tree trunk in cm^2
#' @examples 
#' BA1 <- BasalArea(1.45);
#' @export
BasalArea <-function(DBH){
  BA <- pi*((DBH/2)^2);
  return(BA);
}
#' TreeHeight
#'
#' Calculates the Height of each tree in m
#' @param angle The angle between the horizontal and the top of the tree from the observer in degrees
#' @param distz The distance between the observers eye and the top of the tree in m
#' @return The height of the tree in m
#' @examples 
#' Height1 <- TreeHeight(4.45, 47);
#' @export
TreeHeight <- function(distz, angle){
  radians=angle*(pi/180);
  Height <- (sin(radians)*distz)+1.75 ;
  return(Height);
}
