
#######################################################
plot.level <- function(data, varname, title, subtitle)
{
newdata <- melt(data[,c("date", varname)], id="date")
ids     <- data$date[grep("*-0[3|9]-*", data$date)]

ggplot(newdata, aes(x = date, y = value, group=1) ) +
theme_bw() +
scale_x_discrete(label=ids, breaks=ids) +
ylim(0,NA) +
theme(legend.position = "none",
	  axis.text.x = element_text(color = "black", size = 9, angle=90, vjust=.5),
	  panel.grid.major.x = element_blank(),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11, hjust = 0) ) +
geom_line( color = "blue", size=1 ) +
xlab("") + ylab("") +
labs(title=title, subtitle=subtitle, caption="")
}

#######################################################
plot.ret <- function(data, varname, title, subtitle)
{
newdata <- melt(data[,c("date", varname)], id="date")
ids     <- data$date[grep("*-0[3|9]-*", data$date)]

mu  <- mean(newdata$value)
sig <- sd(newdata$value)

ggplot(newdata, aes(x = date, y = value, group=1) ) +
theme_bw() +
scale_x_discrete(label=ids, breaks=ids) +
theme(legend.position = "none",
	  axis.text.x = element_text(color = "black", size = 9, angle=90, vjust=.5),
	  panel.grid.major.x = element_blank(),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11, hjust = 0) ) +
geom_line( color = "blue", size=1 ) +
geom_hline( color = "red", linetype="dashed", size=.5, aes( yintercept=0 ) ) +
geom_hline( color = "red", size=.5, aes( yintercept=mu ) ) +
geom_hline( color = "green", size=.5, aes( yintercept=mu+1.96*sig ) ) +
geom_hline( color = "green", size=.5, aes( yintercept=mu-1.96*sig ) ) +
xlab("") + ylab("") +
labs(title=title, subtitle=subtitle,
	 caption=paste("media = ", round(100*mu,2), "DP = ", round(100*sig,2)) )
}

#######################################################
# dates
#######################################################

####################################################
date2month <- function(data)
{
newdata <- as.character(data)

m1 <- paste0( c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), "-15")
m2 <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
for(i in seq_along(m1)) newdata <- gsub(m1[i], m2[i], newdata )
return(newdata)
}

####################################################
month2date <- function(data)
{

m1 <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
m2 <- paste0( c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), "-15")
for(i in seq_along(m1)) data <- gsub(m1[i], m2[i], data )
return(as.Date(data))
}

