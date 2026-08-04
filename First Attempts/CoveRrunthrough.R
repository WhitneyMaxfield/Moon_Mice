###### CovR workthrough ###### 

install.packages('coveR2')
library(coveR2)

devtools::install_gitlab("fchianucci/coveR2")

BOHO01 <- "/Users/whitneymaxfield/Downloads/BOHO01_Canopy.JPG"
result <- coveR2(BOHO01)
result

GOME03 <- "/Users/whitneymaxfield/Downloads/100_0064.JPG"
result2 <- coveR2(GOME03)
result2
