nba_stats_win <- nba_stats[order(nba_stats$WinPercentage), ]
nba_stats_teamfg <- nba_stats[order(nba_stats$TeamFGPercentage), ]
nba_stats_opptfg <- nba_stats[order(nba_stats$OpptFGPercentage), ]

library(ggplot2)

#Visualize the teams with the best Win Percentage in ascending order

ggplot(nba_stats_win, aes(x = reorder(Team, WinPercentage), y = WinPercentage)) +
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Teams with the best Wining %") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "NBA Teams", y = "Win %") 

#Visualize the teams with the best Field Goal Percentage in ascending order

ggplot(nba_stats_teamfg, aes(x = reorder(Team, TeamFGPercentage), y = TeamFGPercentage)) +
  geom_col(fill = "red") +
  ggtitle("Teams with the best Field Goal %") +
  labs(x = "NBA Teams", y = "Field Goal %", xlab = "Team") +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(face = "bold", size = 10),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10))


#Visualize the teams with the best Opponent Goal Percentage in ascending order

ggplot(nba_stats_opptfg, aes(x = reorder(Team, OpptFGPercentage), y = OpptFGPercentage)) +
  geom_col(fill = "red") +
  ggtitle("Teams with the best Opponent Field Goal %") +
  labs(x = "NBA Teams", y = "Opponent Fied Goal Percentage", xlab = "Team") +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(face = "bold", size = 10),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10))

#Perform regression analysis 
#WinPercentage as the response variable 
#and TeamFGPercentage and OpptFGPercentage as the predictor variables

lmwin_perc <- lm(nba_stats$WinPercentage ~ nba_stats$TeamFGPercentage + nba_stats$OpptFGPercentage)
summary(lmwin_perc)


#Calculate VIF value to get more reliable estimates of the regression coefficients
library(car)
vif(lmwin_perc)

# Normal probability plot of residuals
residual <- resid(lmwin_perc)
qqnorm(residual)
qqline(residual)


# Extracting relevant columns as numeric vectors
x = as.numeric(nba_stats_teamfg$TeamFGPercentage)
y = as.numeric(nba_stats_opptfg$OpptFGPercentage)
z = as.numeric(nba_stats_win$WinPercentage)

# Creating an interactive 3D plot
plot3d(x, y, z, xlab = "Team FG%", ylab = "Opp FG%", zlab = "Win%")

# Creating the regression plane
surface3d(x.pred, y.pred, z.pred, alpha=0.4, front = "lines", back = "lines")

# Creating residual lines
segments3d(interleave(x, x),
           interleave(y, y),
           interleave(z, fitpoints), alpha=0.4, col="red")








