# Make a copy of the data set
Multiple_Trained_Score_2copy <- Multiple_Trained_Score_2
# Convert Applicatons and Starts to a factor
Multiple_Trained_Score_2copy$A_S_Label <- factor(Multiple_Trained_Score_2copy$A_S_Label)
# Map Applications and starts to fill, make the bars NOT stacked, and make them semitransparent
# Creat values for each school
Ashford_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=Ashford.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
AIU_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=AIU.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
Argosy_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=Argosy.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
CTU_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=CTU.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
Everest_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=Everest.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
NorthCentral_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=NorthCentral.Prob, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
UltimateMedical_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=Ultimate.Medical.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
GrandCanyon_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=GrandCanyon.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
ColoradoChristian_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=Col.Christian.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
South_H <- ggplot(Multiple_Trained_Score_2copy, aes(x=South.Probability, fill=A_S_Label)) + geom_histogram(position="identity", alpha=0.4, bins = 100) + scale_y_log10()
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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
#
#
#
## Create Multiple Image Plot of All Schools (Loged Values)
multiplot(Ashford_H, AIU_H, Argosy_H, CTU_H, NorthCentral_H, ColoradoChristian_H, Everest_H, GrandCanyon_H, South_H, UltimateMedical_H)
