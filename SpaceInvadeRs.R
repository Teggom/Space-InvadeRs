if(!("tcltk2" %in% installed.packages())){install.packages("tcltk2");library(tcltk2)}else{library(tcltk2)}
if(!("png" %in% installed.packages())){intall.packages("png");library(png)}else{library(png)}
if(!("raster" %in% installed.packages())){install.packages("raster");library("raster")}else{library("raster")}
#if(!("audio" %in% installed.packages())){install.packages("audio");library("audio")}else{library("audio")}
# Ubuntu wont detect my audio driver, add sound later

setwd("~/Space-InvadeRs/")

## Creates empty plot
#plot(x = c(each, each/2), y = c(2.5, 7), xlim = c(0,1000), ylim = c(0, 1000), 
#     ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', axes = F,
#     type = 'l')

## Loads Pictures 
Pointer_IMG = readPNG("Pointer.png")
Ship_IMG = readPNG("Ship.png")
Enemy_IMG = readPNG("Enemy.png")
Player_Bullet_IMG = readPNG("Player_Bullet.png")
Enemy_Bullet_IMG = readPNG("Enemy_Bullet.png")
## Timing System
TimeCheck <- function(Denom=60){
  #print(Sys.time()-Time)
  while(Sys.time()-Time < 1/Denom){
    #print(Sys.time()-Time)
  }
  Time <<- Sys.time()
}


## Device for Keyboard
window <- tktoplevel()
Label <- ttklabel(window, text = "Breakout Controller")
tkpack(Label)
tkbind(window, "<Key-Left>", function() {Direction<<-"Left";print("Left")})
tkbind(window, "<Key-Right>", function() {Direction<<-"Right";print("Right")})
tkbind(window, "<Key-Up>", function(){Action<<-TRUE;print("Up---------------------------------------")})
# Pause
#tkbind(window, "<Key-p>", function() {Paused<<-!Paused;print("Pausing/Unpausing");Sys.sleep(1)})
# Quick Quit
#tkbind(window, "<Key-q>", function() {print("Killing");Kill()})



Running = T;Menu_Position=0;Last_Click_Action_Wait=10;Direction = "None";Time<<-Sys.time();Action=F;Ship_Speed=30;

# Variables 
Ship_Pos = 500 # Y is always 50
Enemies_X = c(0:9)*90+95
Enemies_Y = c(0,0,0,0,0,0,0,0,0,0)*-80+950
Enemies <- matrix(ncol=2,c(Enemies_X, Enemies_Y))
Bullets <- list()
Enemy_Bullet_Speed = 10;Player_Bullet_Speed = 20
Current_Place = "Menu" #c("Menu", "Game", "Boss", "Credits")
while(Running){
  .time <- Sys.time()
  # Auto Draw
  par(bg = "#000000")
  plot(x = c(0, 1000), y = c(0, 1000), xlim = c(0,1000), ylim = c(0, 1000), 
       ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', axes = F,
       type = 'l')
  if(Current_Place == "Credits"){
    text(500, 700, c("Art: Stephen Kozak"), col = "#FFFFFF")
    text(500, 500, c("Programming: Stephen Kozak"), col = "#FFFFFF")
    text(500, 300, c("Sound: Stephen Kozak"), col = "#FFFFFF")
    if(Action == T){
      print("Moving to Menu")
      Current_Place = "Menu"
      Action = F
      Menu_Position = 0
    }
    TimeCheck(Denom=10)
  } else if(Current_Place == "Menu"){
    print("in Menu")
    # Move Pointer
    if(Direction == "Left"){
      if(Menu_Position!=0){
        Menu_Position = Menu_Position - 1
      }
      Direction = "None"
    } 
    if(Direction == "Right"){
      if(Menu_Position!=2){
        Menu_Position = Menu_Position + 1
      }
      Direction = "None"
    }
    text(c(250, 500, 750), 250, c("Start", "Credits", "Quit"), col="#FFFFFF")  
    rasterImage(Pointer_IMG, 150+Menu_Position*250, 230, 190+Menu_Position*250, 280)
    if(Action==T){
      if(Menu_Position == 0){
        Current_Place = "Game"
        Action=F
      } else if (Menu_Position == 1){
        Current_Place = "Credits"
        Action=F
      }else if (Menu_Position == 2){
        stop("Game was closed! Thanks for playing")
      }
    }
    TimeCheck(Denom=10)
  } else if (Current_Place == "Game"){
    # Draw Ship
    time = Sys.time()
    rasterImage(Ship_IMG, Ship_Pos-65, 5, 65+Ship_Pos, 95)
    
    # Draw Enemies
    apply(Enemies, 1, function(x){rasterImage(image = Enemy_IMG, xleft = x[1]-40, ybottom = x[2]-30, xright = x[1]+40, ytop = x[2]+30)})
    
    # Draw Bullets
    if(length(Bullets)>0){
      lapply(Bullets, function(x){if(x[[1]]=="Player"){g=Player_Bullet_IMG}else{g=Enemy_Bullet_IMG};rasterImage(image=g,xleft=x[[2]]-7, ybottom=x[[3]]-20, xright=x[[2]]+7, ytop=x[[3]]+20)})
    }
    # Check Collision
    position = 1
    while(position <= length(Bullets)){
      Update = T
      if(Bullets[[position]][1]=="Player"){ # From player
        enemy_index = 1
        while(enemy_index < length(Enemies[,1]) && length(Bullets)>0){
          eUpdate = T
          if(abs(Bullets[[position]][[2]]-Enemies[enemy_index,1])<83 && abs(Bullets[[position]][[3]]-Enemies[enemy_index,2])<30){
            ## Enemy hit
            Enemies = Enemies[-enemy_index,]
            Bullets = Bullets[-position]
            eUpdate = F
          }
          if(eUpdate){
            enemy_index = enemy_index + 1
          }
        }
        if(Update && length(Bullets)>0){
          Bullets[[position]][[3]] = Bullets[[position]][[3]]+Player_Bullet_Speed
        }
      } else { # From enemy
        if(Bullets[[position]][[3]] < 80){
          if(abs(Bullets[[position]][[2]]-Ship_Pos) < 50){
           # KILL PLAYER #################################### End Game Here
          }
          Bullets[[position]][[3]] = Bullets[[position]][[3]]-Enemy_Bullet_Speed
        } else if(Bullets[[position]][[3]] < 20 || Bullets[[position]][[3]]>1000){
          Bullets = Bullets[-position]
          Update = F
        }
      }
      
      if(Update && length(Bullets)>0){
        position = position + 1
      }
    }
    
    
    # Check Input - Update Position of Ship
    if(Direction=="Left"){
      Ship_Pos = Ship_Pos - Ship_Speed
    }
    if(Direction=="Right"){
      Ship_Pos = Ship_Pos + Ship_Speed
    }
    Direction = "None"
    # Check Input - Shoot
    
    if(Action){
      Bullets[[length(Bullets)+1]] = list("Player", Ship_Pos, 110)
      Action = F
    }
    
    # Update Enemies
    
    
    
    TimeCheck(Denom=20)
    print(paste(as.character(floor(1/as.numeric((Sys.time()-time)))), "Frames per Second-----"))
    
  } else if (Current_Place == "Boss"){
    
    TimeCheck(Denom=30)
  }
  
  
  
  #print(Sys.time()-.time)
  
  ## Keyboard Input logic here, for menu navigation
  
}

