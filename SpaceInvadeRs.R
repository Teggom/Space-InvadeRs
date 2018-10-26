if(!("tcltk2" %in% installed.packages())){install.packages("tcltk2");library(tcltk2)}else{library(tcltk2)}
if(!("png" %in% installed.packages())){intall.packages("png");library(png)}else{library(png)}
if(!("raster" %in% installed.packages())){install.packages("raster");library("raster")}else{library("raster")}
## Creates empty plot
#plot(x = c(each, each/2), y = c(2.5, 7), xlim = c(0,1000), ylim = c(0, 1000), 
#     ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', axes = F,
#     type = 'l')

## Loads Pictures 
Pointer_IMG = readPNG("Pointer.png")




Running = T;Menu_Position=0;Last_Click_Action_Wait=10;
Current_Place = "Menu" #c("Menu", "Game", "Boss", "Credits")
while(Running){
  if(Current_Place == "Menu"){
    text(c(250, 500, 750), 250, c("Start", "Credits", "Quit"), col="#FFFFFF")    
    rasterImage(Pointer_IMG, 150+Menu_Position*250, 230, 190+Menu_Position*250, 280)
  }
  ## Keyboard Input logic here, for menu navigation
}


par(bg = "#000000")
plot(x = c(0, 1000), y = c(0, 1000), xlim = c(0,1000), ylim = c(0, 1000), 
     ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', axes = F,
     type = 'l')



