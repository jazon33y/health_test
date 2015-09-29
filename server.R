# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("updated_EBC.r")
source('breadths.r')
spinner = function(k,n,z,cols,main) pie(main=main,c(k/n,1-(k/n)),c(paste('Chances of being ',z[1]),paste('Chances of being ',z[2])),col=cols,cex=1.2,cex.main=2)

################
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    pk = input$prevk
    pn = input$prevn
    sk = input$speck
    sn = input$specn
    tk = input$sensk
    tn = input$sensn
    
    
    r = input$result #list("Positive" = "sick", "Negative" = "well"),selected = "sick")
    
    sick = 'ppv'
    well = 'npv'
    if (r=='sick'){answer = ppv(pk,pn, sk,sn, tk,tn);text=c('sick','well');cols=c('red','blue')}
    if (r=='well'){answer = npv(pk,pn, sk,sn, tk,tn);text=c('well','sick');cols=c('blue','red')}
    
    Ebc = ebc(answer)
    #sayebc(Ebc,c('patient',r))
    main = paste0("Your chances of being ",text[1]," are ",Ebc[1]," out of ",Ebc[3],"    ")
    n = Ebc[1]
    k = Ebc[3]
      
      

    library('plotrix')
    par(mar=c(0,0,0,0))
    icon_array <- function(n=100,k=50,colors=c('blue','red')){
      grid = ceiling(n/10)
      element = 1:10/5
      ticktockticktock = 0
      plot(NULL,xlim = c(0,1),ylim=c(0,1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      nn = 0
      for (ii in 1:grid){
        #print(ticktockticktock)
        #print(ii)
        ticktock = 1
        for (i in 1:(n/grid)){
          nn = nn + 1
          #print(i)
          if (nn>(n-k)){col=colors[2]}else{col=colors[1]}
          y = element[ticktock]
          ticktock = ticktock + 1
          draw.circle((.5 *.5)+ticktockticktock-.22,(.505- y+1.6)*.5,.015*.5,border=NA,col=col)
          #left arm circle
          draw.circle((.475*.5)+ticktockticktock-.22,(.47- y+1.6)*.5,.0075*.5, border=NA,col=col)
          #left arm 
          polygon(x = (c(.475-.0075,.475+.0075,.475+.0075,.475-.0075)*.5)+ticktockticktock-.22,y = (c(.47,.47,.425,.425)- y+1.6)*.5,border=NA,col=col)
          #right arm circle
          draw.circle((.525*.5)+ticktockticktock-.22,(.47- y+1.6)*.5,.0075*.5,border=NA,col=col)
          #right arm
          polygon(x = (c(.525-.0075,.525+.0075,.525+.0075,.525-.0075)*.5)+ticktockticktock-.22,y = (c(.47,.47,.425,.425)- y+1.6)*.5,border=NA,col=col)
          #torso
          polygon(x = (c(.485,.515,.515,.485)*.5)+ticktockticktock-.22,y = (c(.47,.47,.4,.4)- y+1.6)*.5,border=NA,col=col)    
          #left leg
          polygon(x = (c(.485,.498,.498,.485)*.5)+ticktockticktock-.22,y = (c(.4,.4,.35,.35)- y+1.6)*.5,border=NA,col=col)    
          #right leg
          polygon(x = (c(.502,.515,.515,.502)*.5)+ticktockticktock-.22,y = (c(.4,.4,.35,.35)- y+1.6)*.5,border=NA,col=col)    
        }
        ticktockticktock = (ticktockticktock+.05)
      }
      if((n%%grid==0) == FALSE){
        ticktock=1
        for (i in 1:(n-floor(n/grid)*grid)){
          y = element[ticktock]
          ticktock = ticktock + 1
          draw.circle((.5 *.5)+ticktockticktock-.22,(.505- y+1.6)*.5,.015*.5,border=NA,col=col)
          #left arm circle
          draw.circle((.475*.5)+ticktockticktock-.22,(.47- y+1.6)*.5,.0075*.5, border=NA,col=col)
          #left arm 
          polygon(x = (c(.475-.0075,.475+.0075,.475+.0075,.475-.0075)*.5)+ticktockticktock-.22,y = (c(.47,.47,.425,.425)- y+1.6)*.5,border=NA,col=col)
          #right arm circle
          draw.circle((.525*.5)+ticktockticktock-.22,(.47- y+1.6)*.5,.0075*.5,border=NA,col=col)
          #right arm
          polygon(x = (c(.525-.0075,.525+.0075,.525+.0075,.525-.0075)*.5)+ticktockticktock-.22,y = (c(.47,.47,.425,.425)- y+1.6)*.5,border=NA,col=col)
          #torso
          polygon(x = (c(.485,.515,.515,.485)*.5)+ticktockticktock-.22,y = (c(.47,.47,.4,.4)- y+1.6)*.5,border=NA,col=col)    
          #left leg
          polygon(x = (c(.485,.498,.498,.485)*.5)+ticktockticktock-.22,y = (c(.4,.4,.35,.35)- y+1.6)*.5,border=NA,col=col)    
          #right leg
          polygon(x = (c(.502,.515,.515,.502)*.5)+ticktockticktock-.22,y = (c(.4,.4,.35,.35)- y+1.6)*.5,border=NA,col=col)
        }
      }
    }
    
    icon_array(n=k,k=n)


  })

})
