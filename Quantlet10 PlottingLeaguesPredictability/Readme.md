[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **PlottingLeaguesPredictability** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: PlottingLeaguesPredictability

Published in: Statistical Programming Languages - Student Project on Analysis of a FIFA Dataset

Description: 'In this quantlet we are plotting leagues predictability  which has been calculated in Quantlet9.
Plots are saved to pdf files.'

Keywords: Entropy, competitivness, plot, predictability

Author: Kseniia Ovadenko

See also: other quantlets in this project

Submitted: 14.03.2018

```


### R Code:

#### Plotting predictability of the leagues using the dataset created in Quantlet9

```{r}
library("rio")
library("ggplot2")

colors = c("blue", "green", "red", "violet", "orange")

ggplot(dat = league.entropy, aes(x = season, y = ent)) + geom_line(aes(colour = league_name, 
    group = league_name)) + geom_point(aes(colour = league_name, group = league_name)) + 
    scale_colour_manual(values = colors) + xlab("Season") + ylab("Average Entropy by League") + annotate("text", x = 4, y = 1.035, 
    label = "less predictable") + annotate("text", x = 4, y = 0.93, label = "more predictable") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("leagues.pdf", width = 10, height = 5, dpi = 100)
```

#### Plotting predictability of the leagues by team using the dataset created in Quantlet9

```{r}
ggplot(dat = team.entropy, aes(x = season, y = ent)) + geom_point(aes(colour = league_name, 
    group = league_name), position = position_dodge(width = 0.5)) + scale_colour_manual(values = colors) + 
    xlab("Season") + ylab("Average Entropy by Team") + 
    annotate("text", x = 4, y = 1.1, label = "less predictable") + annotate("text", 
    x = 4, y = 0.6, label = "more predictable") + annotate("text", x = 7.8, 
    y = 0.64, label = "Barcelona", size = 2.2) + annotate("text", x = 7.65, 
    y = 0.65, label = "B.Munich", size = 2.2) + annotate("text", x = 7.75, 
    y = 0.73, label = "Real Madrid", size = 2.2) + annotate("text", x = 7.67, 
    y = 0.79, label = "PSG", size = 2.2) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("teams.pdf", width = 10, height = 5, dpi = 100)
```
