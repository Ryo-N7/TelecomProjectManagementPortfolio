rm(list = ls())
graphics.off()
fiscalyear <- c(2006,2007,2008,2009,2010,2011,2012)
revenue <- c(20.000000,21.430000,22.500000,23.345000,23.978000,24.670000,26.234000)
expenses <- c(17.567000, 18.950000, 20.755000, 23.850000, 22.930000, 22.135000, 22.500000)
profit <- revenue - expenses
profit
profitmargin <- (profit / revenue) * 100
profitmargin  
Telecom <- data.frame(fiscalyear, revenue, expenses, profit, profitmargin)

# Use dplyr to shift FiscalYear from ROW to COLUMNS? NOOOO. 
TelecomData <- data.frame(c(2006, 2007, 2008, 2009, 2010, 2011, 2012), revenue, expenses, profit, profitmargin)

TelecomData <- data.frame(t(Telecom))

write.table(Telecom, file = "TelecomDatata")
write.csv(Telecom, file = "TelecomData")
?write.csv


str(Telecom)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)

# Revenue + Expenditure chart

pdf("revexp.pdf")
ggplot(Telecom, aes(fiscalyear, revenue)) +
  geom_line(aes(y = revenue, color = "Revenue"), size = 1.5) +
  geom_line(aes(y = expenses, color = "Expenses"), size = 1.5) +
  theme_economist() +
  ggtitle("Projected Revenue & Expenses (2006-2012)") +
  ylab("Revenue & Expenses (In Millions of Dollars)") + 
  xlab("Fiscal Year") +
  scale_x_continuous(breaks = fiscalyear) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(color = "black", size = 0.00001),
        legend.title = element_blank())

graphics.off()


# Profit and Profit margin chart

pdf("profitandmargin.pdf")
ggplot(Telecom, aes(fiscalyear)) +
  geom_line(aes(y = profit, color = "Profit"), size = 1.5) +
  geom_line(aes(y = profitmargin, color = "Profit Margin"), size = 1.5) +
  theme_economist() +
  labs(title = "Projected Profit & Profit Margin (2006-2012)", 
       x = "Fiscal Year", y = "Profit & Profit Margin - In Millions of Dollars") +
  scale_x_continuous(breaks = fiscalyear) +
  scale_y_continuous(breaks = pretty_breaks(n = 20)) +
  geom_hline(yintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), 
        panel.grid.major.x = element_line(color = "black", size = 0.00001)) +
  scale_color_manual(values = c("Profit" = "green", "Profit Margin" = "red"))

graphics.off()

# Profit margin
ggplot(Telecom, aes(fiscalyear)) +
  geom_line(aes(y = profitmargin, color = I("red")), size = 1.5) +
  theme_economist() +
  labs(title = "Projected Profit Margin (2006-2012)", 
       x = "Fiscal Year", y = "Profit Margin - Percentage (%)") +
  scale_x_continuous(breaks = fiscalyear) +
  scale_y_continuous(breaks = pretty_breaks(n = 20)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 9.5, color = I("grey"), size = 1.5) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "black", size = 0.00001)) +
  annotate("text", x = 2009, y = 9.5, label = "Industry\nAverage", 
           color = "red")
?annotate
##############################################################################################
ggplot(Telecom, aes(fiscalyear)) +
  geom_line(aes(y = profit, color = I("green")), size = 1.5) +
  theme_economist() +
  labs(title = "Projected Profit (2006-2012)", 
       x = "Fiscal Year", y = "Profit - In Millions of Dollars") +
  scale_x_continuous(breaks = fiscalyear) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 3.0, color = I("grey"), size = 1.5) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "black", size = 0.00001)) +
  annotate("text", x = 2009, y = 3.0, label = "Industry\nAverage", 
           color = "red")
        

ggplot(Telecom, aes(fiscalyear)) +
  geom_line(aes(y = profitmargin, color = "Profit Margin")) +
  theme_economist()

# +scale_color_manual(values=c("profit"="green", "profitmargin"="red4")))

# + scale_linetype(guide = guide_legend(override.aes = list(alpha = 1)), labels = 
#                 c(expression(A[1]), expression(A[2]), expression(A[3]), expression(A[4]), expression(A[5]))) + 
#  theme(legend.position = c(.75, .25)

graphics.off()
# + theme(plot.title = element_text(hjust = 0.5),
#      panel.grid.major.x = element_line(color = "black")


# lolmessingaroundwithaesthetics chart

pdf("profitandmargin.pdf")
ggplot(Telecom, aes(fiscalyear)) + 
  geom_line(aes(y = profit)) +
  geom_line(aes(y = profitmargin, color = I("red"))) +
  theme_economist() +
  labs(title = "Projected Profit & Profit Margin (2006-2012)", x = "Fiscal Year", y = "Profit & Profit Margin - In Millions of Dollars") +
  scale_x_continuous(breaks = fiscalyear) +
  scale_y_continuous(breaks = pretty_breaks(n = 20)) +
  theme(axis.text.x = element_text(color = "red"), 
        axis.text.y = element_text(color = "blue"), 
        axis.title.x = element_text(color = "green"), 
        axis.title.y = element_text(color = "Red"),
        panel.grid.major.x = element_line(color = "black")) +
  geom_hline(yintercept = 0, color = I("blue")) +
  theme(plot.title = element_text(hjust = 0.5))
graphics.off()


Telecom %>% 
  filter(fiscalyear == 2010) %>% 
  select(revenue, expenses)




# PIE CHARTS:
# 2006 chart

pdf("2006ExpensePie.pdf")
costchart <- data.frame(expensetype = c("Operating", "Infrastructure & Capital", "Financing"), 
                        value = c(45, 32, 23))

bp <- ggplot(costchart, aes("", y = value, fill = expensetype)) + 
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5)

pie <- bp + coord_polar("y", start=0) +
  theme(axis.text=element_blank(),
        axis.ticks= element_blank(), 
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5) +
  labs(title = "Expense Breakdown (2006)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Expense Type",
                    labels=c("Infrastructure & Capital", "Financing", "Operating")) +
  guides(fill = guide_legend(reverse = TRUE))

pie

graphics.off()

# 2012 chart v1

costchart2 <- data.frame(expensetype = c("Operating", "Infrastructure & Capital", "Financing"), 
                        value = c(40, 38, 22))

bp2 <- ggplot(costchart2, aes("", y = value, fill = expensetype)) + 
  geom_bar(width = 1, stat = "identity")
bp2
pdf("2012ExpensePie.pdf")
pie2 <- bp2 + coord_polar("y", start=0) +
  theme(axis.text.x=element_blank(),
        axis.ticks= element_blank(), 
        panel.grid = element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5) +
  labs(title = "Expense Breakdown (2012)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Expense Type",
                    labels=c("Infrastructure & Capital", "Financing", "Operating"))

pie2

graphics.off()

pie

# 2009 chart

costchart3 <- data.frame(expensetype = c("Operating", "Infrastructure & Capital", "Financing"), 
                         value = c(38, 32, 30))
bp3 <- ggplot(costchart3, aes("", y = value, fill = expensetype)) + 
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5)
pdf("2009ExpensePie.pdf")
pie3 <- bp3 + coord_polar("y", start=0) +
  theme(axis.text=element_blank(),
        axis.ticks= element_blank(), 
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5) +
  labs(title = "Expense Breakdown (2009)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Expense Type",
                    labels=c("Infrastructure & Capital", "Financing", "Operating")) +
  guides(fill = guide_legend(reverse = TRUE))

pie3

graphics.off()


# 2012 reworked

costchart4 <- data.frame(expensetype = c("Operating", "Infrastructure & Capital", "Financing"), 
                         value = c(44, 29, 27))

bp4 <- ggplot(costchart4, aes("", y = value, fill = expensetype)) + 
  geom_bar(width = 1, stat = "identity")
bp4
pdf("2012ExpensePieV2.pdf")

pie4 <- bp4 + coord_polar("y", start=0) +
  theme(axis.text.x=element_blank(),
        axis.ticks= element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5) +
  labs(title = "Expense Breakdown (2012)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Expense Type",
                    labels=c("Infrastructure & Capital", "Financing", "Operating")) +
  guides(fill = guide_legend(reverse = TRUE))

pie4

graphics.off()



par(mfrow = c(1,3))

library(grid)
# 1 Row, 3 Columns.
pushViewport(viewport(layout = grid.layout(1,3)))
print(pie, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pie3, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(pie4, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))

# 3 Rows, 1 Column.
pushViewport(viewport(layout = grid.layout(3,1)))
print(pie, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pie3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(pie4, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))

graphics.off()








# NO legend
pie3 <- bp3 + coord_polar("y", start=0) +
  theme(axis.text=element_blank(),
        axis.ticks= element_blank(), 
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5) +
  labs(title = "Expense Breakdown (2009)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Expense Type",
                    labels=c("Infrastructure & Capital", "Financing", "Operating")) 

pie3

pie <- bp + coord_polar("y", start=0) +
  theme(axis.text=element_blank(),
        axis.ticks= element_blank(), 
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5) +
  labs(title = "Expense Breakdown (2006)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Expense Type",
                    labels=c("Infrastructure & Capital", "Financing", "Operating"))

pie

pie4 <- bp4 + coord_polar("y", start=0) +
  theme(axis.text.x=element_blank(),
        axis.ticks= element_blank(), 
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5) +
  labs(title = "Expense Breakdown (2012)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Expense Type",
                    labels=c("Infrastructure & Capital", "Financing", "Operating"))
pie4

# scale_fill_colorblind()

# scale_colour_brewer(name = "My Legend")


# at <- nrow(costchart2) - as.numeric(cumsum(sort(table(costchart2)))-0.5*sort(table(costchart2)))

