################################### functions #################################

##### Tratamento dos dados.
                                       
trat<-function(data){
  
  require(tidyverse)
  require(lubridate)
  
  data<-data %>% separate(Data, c("d", "m", "y"))
  
  data<-data%>% unite(Data, c("d", "m", "y"), sep = "/")
  
  data$Data<-dmy(data$Data)
  
  return(data)
  
}

##### Função para criar figuras com multipos plot

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


### Variação Percentual

VP<- function(Data,DataInicial,DataFinal){
  'Data= dataset, DataInicial, DataFinal'
  
  require(tidyverse)
  require(lubridate)
  
  a <-Data%>%
    filter(Data ==DataFinal )
  
  a_1<-a$Último
  
  b <-Data%>%
    filter(Data ==DataInicial)
  
  b_1<-b$Último
  
  x = (a_1/b_1 - 1) * 100
  
  return(x)
}
