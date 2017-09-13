library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)

fiscalyear <- c(2006,2007,2008,2009,2010,2011,2012)
revenue <- c(20.000000,21.430000,22.500000,23.345000,23.978000,24.670000,26.234000)
expenses <- c(17.567000, 18.950000, 20.755000, 23.850000, 22.930000, 22.135000, 22.500000)
profit <- revenue - expenses
profit
profitmargin <- (profit / revenue)
profitmargin  
Telecom <- data.frame(fiscalyear, revenue, expenses, profit, profitmargin)

# Use dplyr to shift FiscalYear from ROW to COLUMNS? NOOOO. 
TelecomData <- data.frame(c(2006, 2007, 2008, 2009, 2010, 2011, 2012), revenue, expenses, profit, profitmargin)

# TelecomData <- data.frame(t(Telecom))
?t()   # from matrix/data.frame "x" >>>> transpose of "x"    ex. 7 obs of 5 var. >>> 5 obs. of 7 var.

glimpse(Telecom)

# Revenue + Expenditure chart

pdf("revexp.pdf")
ggplot(Telecom, aes(fiscalyear, revenue)) +
  geom_line(aes(y = revenue, color = "Revenue"), size = 1.5) +
  geom_line(aes(y = expenses, color = "Expenses"), size = 1.5) +
  ggtitle("Projected Revenue & Expenses (2006-2012)") +
  ylab("Revenue & Expenses (In Millions of Dollars)") + 
  xlab("Fiscal Year") +
  scale_x_continuous(breaks = fiscalyear) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme_economist() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(color = "black", size = 0.00001),
        legend.title = element_blank())

graphics.off()


# Profit and Profit margin chart

pdf("profitandmargin.pdf")

ggplot(Telecom, aes(fiscalyear)) +
  geom_line(aes(y = profit, color = "Profit"), size = 1.5) +
  geom_line(aes(y = profitmargin*100, color = "Profit Margin"), size = 1.5) +
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
  geom_line(aes(y = profitmargin), size = 1.3, color = "red") +
  labs(title = "Projected Profit Margin (2006-2012)", 
       x = "Fiscal Year", y = "Profit Margin - Percentage (%)") +
  scale_x_continuous(breaks = fiscalyear) +
  scale_y_continuous(labels = percent_format(), limits = c(-0.05, 0.15), breaks = pretty_breaks(5)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.0095, color = "grey", size = 1.25) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "black", size = 0.00001)) +
  annotate(geom = "text", x = 2011.5, y = 0.0095, label = "Industry\nAverage", 
           color = "black", size = 2.75)
?annotate
##############################################################################################
ggplot(Telecom, aes(fiscalyear)) +
  geom_line(aes(y = profit, color = I("green")), size = 1.5) +
  theme_bw() +
  labs(title = "Projected Profit (2006-2012)", 
       x = "Fiscal Year", y = "Profit - In Millions of Dollars") +
  scale_x_continuous(breaks = fiscalyear) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 3.0, color = I("grey"), size = 1.5) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        panel.grid.major.x = element_line(color = "black", size = 0.00001)) +
  annotate("text", x = 2009, y = 3.0, label = "Industry\nAverage")
        

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

pdf("lolgraph.pdf")
ggplot(Telecom, aes(fiscalyear)) + 
  geom_line(aes(y = profit)) +
  geom_line(aes(y = profitmargin, color = I("red"))) +
  theme_economist() +
  labs(title = "Projected Profit & Profit Margin (2006-2012)", y = "Profit & Profit Margin - In Millions of Dollars") +
  scale_x_continuous(breaks = fiscalyear, name = "Yearinhos", labels = percent) +
  scale_y_continuous(breaks = pretty_breaks(n = 10), labels = scientific) +
  theme(axis.text.x = element_text(color = "red", angle = 120), 
        axis.text.y = element_text(color = "blue", size = 14, angle = 45), 
        axis.title.x = element_text(color = "green", size = 9, angle = 30), 
        axis.title.y = element_text(color = "black", size = 6),
        axis.line.x = element_line(color = "darkblue", size = 3, linetype = "dashed"),
        axis.line.y = element_line(color = "purple", size = 0.4, linetype = "dotdash"),
        panel.grid.major.x = element_line(color = "orange")) +
  geom_hline(yintercept = 0, color = I("blue")) +
  theme(plot.title = element_text(hjust = 0.5))
graphics.off()


Telecom %>% 
  filter(fiscalyear == 2010) %>% 
  select(revenue, expenses)






# CREATE DOT PLOT based on pie charts for ^visualization!!!!!!!!!!!!!!!

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

# Doot chart:
costchart <- data.frame(expensetype = c("Operating", "Infrastructure & Capital", "Financing"), 
                        Y2006 = c(0.45, 0.32, 0.23),   # 
                        Y2007 = c(0.44, 0.32, 0.24),
                        Y2008 = c(0.40, 0.34, 0.26),
                        Y2009 = c(0.38, 0.32, 0.30),   # 
                        Y2010 = c(0.36, 0.36, 0.28),
                        Y2011 = c(0.37, 0.37, 0.26),
                        Y2012 = c(0.40, 0.38, 0.22))    # 

costgather <- costchart %>% 
  melt(id.vars = "expensetype",
       measure.vars = c("Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012"),
       variable.name = "Yearoinho", value.name = "Percentage")


# dotplot
ggplot(costgather, aes(x = Percentage, y = reorder(expensetype, Percentage), group = Yearoinho)) +
  geom_point(aes(shape = Yearoinho, color = Yearoinho), size = 5) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "Percentages", y = NULL) +
  ggtitle("Fig 8.4 Task Data Unified") +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey60"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1.1), face = "bold"),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(3, "lines"))

?scale_shape_manual()



ggplot(costgather, aes(x = Yearoinho, y = Percentage, group = expensetype)) +
  geom_line() + 
  facet_grid(. ~ expensetype) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(labels = percent)





my_data <- data.frame(
  task = c("Connect a stereo", "Create a mixed tape", "Program radio stations", 
           "Record on one side of a tape", "Program the CD player", 
           "Set the clock", "Set the timer to record", 
           "Listen to a song on a CD", "Listen to a tape", 
           "Listen to the radio"),
  Revised = c(8.8, 9.6, 7.7, 5.8, 2.6, 2.3, 2.2, 2.1, 2.2, 1.0),
  Original = c(18.6, 15.9, 11.3, 9.7, 6.4, 4.0, 3.7, 3.8, 2.6, 0.9))

library(reshape2)
my_data_long <- melt(my_data, id.vars = "task", 
                     measure.vars = c("Revised", "Original"),
                     variable.name = "type", value.name = "time")



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
pie3.1 <- bp3 + coord_polar("y", start=0) +
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

pie1.1 <- bp + coord_polar("y", start=0) +
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

pie4.1 <- bp4 + coord_polar("y", start=0) +
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

