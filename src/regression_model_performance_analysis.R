#regression model performance of dodgers data 

dodgers <- read.csv(file ="c:/Users/jaspr/iCloudDrive/QUARTER 6/DA 420/2/dodgers.csv", header = TRUE)
colnames(dodgers_data)
dodgers_data$day_of_week <- factor(dodgers_data$day_of_week , levels= c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))
dodgers_data$attend <- dodgers_data$attend /1000

plot(dodgers_data$day_of_week, dodgers_data$attend,
     main = "Dodgers Attendence By Day Of Week", 
     xlab = "Day of Week", 
     ylab = "Attendance (thousands)", 
     ylim(25,55),
     col = "grey", las = 1)

boxplot(dodgers_data$attend ~ dodgers_data$day_of_week, 
       
        col =  c('grey', 'grey','grey','grey','grey','grey','grey'),
        ylab = "Attendence (thousands)", 
        xlab =  "Day of Week", 
        par(mar = c(12, 5, 4, 2)+ 0.1))

title("Figure 2.1 Dodgers Attendence by Day of Week")


boxplot(dodgers_data$attend ~ dodgers_data$month, 
        col =  c('grey', 'grey','grey','grey','grey','grey','grey'),
        ylab = "Attendence (thousands)", 
        xlab =  "Month", 
        par(mar = c(12, 5, 4, 2)+ 0.1))

title("Figure 2.1 Dodgers Attendence by Month")


ggplot(dodgers_data, aes(x=temp, y=attend, color=fireworks)) +
   geom_point()+
   facet_wrap(day_night~skies)+
   
   theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=10))+xlab("Temperature (Degree Farenheit)")+
   ylab("Attendance (Thousands)")
title("Dodgers Attendance By Temperature By Time of Game and Skies")

#Strip Plot of Attendance by opponent or visiting team
ggplot(dodgers_data, aes(x=attend/1000, y=opponent, pch=day_night)) + 
  geom_point() + 
  ggtitle("Dodgers Attendance By Opponent") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=10)) +
  xlab("Attendance (Thousands)") +
  ylab("Opponent (Visiting Team)")






#define an day_of_week variable
#for plots and data summaries
dodgers$ordered_day_of_week <- with(data=dodgers,
                            ifelse ((day_of_week == "Monday"),1,
                            ifelse ((day_of_week == "Tuesday"),2,
                            ifelse ((day_of_week == "Wednesday"),3,
                            ifelse ((day_of_week == "Thursday"),4,
                            ifelse ((day_of_week == "Friday"),5,
                            ifelse ((day_of_week == "Saturday"),6,7)))))))
                           
dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels = 1:7,
                              labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))



#exploratory data analysis with standard graphics: attendance by the day of week
with(data=dodgers, plot(ordered_day_of_week, attend/1000,
                        xlab = "Day of Week", ylab = "Attendence (thousands)",
                        col="grey", las=1))

#when do the Dodgers use bobblehead promotions
with(data=dodgers, table(bobblehead, ordered_day_of_week)) # bobbleheads on Tuesday


#define an ordered month variable
#for plots and data summaries
dodgers$ordered_month <- with(data=dodgers,
                              ifelse ((month == "APR"),4,
                              ifelse ((month == "MAY"),5,
                              ifelse ((month == "JUN"),6,
                              ifelse ((month == "JUL"),7,
                              ifelse ((month == "AUG"),8,
                              ifelse ((month == "SEP"),9,10)))))))
dodgers$ordered_month<- factor(dodgers$ordered_month, levels = 4:10,
                                      labels=c("April", "May", "June", "July", "Aug", "Sept", "Oct"))
#exploratory data analysis with standard graphics: attendance by the month
with(data=dodgers, plot(ordered_month, attend/1000,
                        xlab = "Month", ylab = "Attendence (thousands)",
                        col="violet", las=1))

#exploratory data analysis displaying many variables
#looking at attendence and conditioning on day/night
# the skies and whether or not fireworks are displayed

#preparing a graphical summary for Dodgers datagrouop

group.labels <- c("No Fireworks", "Fireworks")
group.symbols <- c(21,24)
group.colors <- c("black" , "black")
group.fill <- c("black","red")
xyplot(attend/1000 ~ temp | skies +day_night,
       data = dodgers, groups = fireworks, pch = group.symbols,
       aspect = 1,cex =1.5, col = group.colors, fill =group.fill,
       layout = c(2,2), type = c("p","g"),
       strip = strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),
       xlab = "Temperature (Degress Fahrenheit)",
       ylab = "Attendence (thousands)",
       key = list(space ="top",
       text = list(rev(group.labels), col = rev(group.colors)),
      points = list(pch = rev(group.symbols), col = rev(group.colors),
                    fill = rev(group.fill))))
 

#attendance by opponent and day/night game    

group.labels <- c("Day", "Night")
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night,
      xlab = "Attendence (thousands)",
      panel= function(x,y,groups, subscripts, ...)
      {panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
        panel.stripplot(x,y,groups = groups, subscripts = subscripts,
        cex = group.symbols.size, pch = group.symbols, col = "darkblue")
      }, 
     
      text = list(group.symbols, col = "black"),
      points = list(pch = group.symbols, cex = group.symbols.size,
      col = "darkblue"))



# employ training-and-test regimen for model validation 
set.



# Create a model with the bobblehead variable entered last
my.model <- {attend ~ month + day_of_week + bobblehead}

# Prepare a Training and Test dataset

# Reseed for repeatability
set.seed(1234)

training_test <- c(rep(1, trunc((2/3)*nrow(dodgers))), rep(2, trunc((1/3)*nrow(dodgers))))

# sample(training_test)

# Create a variable in dodgers data frame to identify Test and Training row
dodgers$Training_Test <- sample(training_test)

dodgers$Training_Test <- factor(dodgers$Training_Test, levels = c(1, 2), labels = c("TRAIN", "TEST"))


dodgers.Train <- subset(dodgers, Training_Test == "TRAIN")
dodgers.Test <- subset(dodgers, Training_Test == "TEST")

# Fit model to training set
train.model.fit <- lm(my.model, data = dodgers.Train)

# Predict from Training Set
dodgers.Train$Predict_Attend <- predict(train.model.fit)

# Evaluate The Fitted Model on the Test Set
dodgers.Test$Predict_Attend <- predict(train.model.fit, newdata = dodgers.Test)

#round(cor(DodgersData.Test$attend, DodgersData.Test$Predict_Attend)^2, digits=3)

# compute the proportion of response variance accounted for when predicting Test Data
cat("\n","Proportion of Test Set Variance Accounted for: ", 
    round(cor(dodgers.Test$attend, 
              dodgers.Test$Predict_Attend)^2, digits=3), 
    "\n", sep="")

dodgers.Training_Test <- rbind(dodgers.Train, dodgers.Test)


ggplot(dodgers.Training_Test, aes(x=attend/1000, y=Predict_Attend/1000, color=bobblehead)) + 
  geom_point() + 
  geom_line(data = data.frame(x = c(25,60), y = c(25,60)), aes(x = x, y = y), colour = "red") +
  facet_wrap(~Training_Test) +
  #geom_smooth(method = "lm", se=FALSE) +
  ggtitle("Regression Model Performance : Bobblehead and Attendance") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=10)) +
  xlab("Actual Attendance (Thousands)") +
  ylab("Predicted Attendance (Thousands)")

#Strip Plot of Attendance by opponent or visiting team
ggplot(dodgers, aes(x=attend/1000, y=opponent, color=day_night)) + 
  geom_point() + 
  ggtitle("Dodgers Attendance By Opponent") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=10)) +
  xlab("Attendance (Thousands)") +
  ylab("Opponent (Visiting Team)")






# employ training-and-test regimen for model validation
set.seed(1234)#set seed for repeatability of training-and-test split

training_test <- c(rep(1, trunc((2/3)*nrow(dodgers))), rep(2, trunc((1/3)*nrow(dodgers))))
rep(2, length =(nrow(dodgers) - trunc((2/3)*nrow(dodgers))))
dodgers$training_test <- sample(training_test) # random permutation
dodgers$training_test <- factor(dodgers$training_test,
                                levels=c(1,2), labels = c("TRAIN" , "TEST"))
dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) # check training data frame
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # check test data frame

# specify a simple model with bobblehead entered last
my.model <- {attend ~ month + day_of_week + bobblehead}

#fit the model to the training set
train.model.fit <-  lm(my.model, data = dodgers.train)
# summary of model fit to the training set
print(summary(train.model.fit))
#training set predictions from the model fit to the training set
dodgers.train$predict_attend <- predict(train.model.fit)
# test set predictions from the model fit to the training set
dodgers.test$predict_attend <- predict(train.model.fit,
newdata = dodgers.test)

# merge the training and test sets for plotting 
dodgers.plotting.frame <- rbind(dodgers.train, dodgers.test)
  

# generate predictive modeling visual for management 
group.labels <- c("No Bobblehead", "Bobbleheads")
group.symbols <- c(21,24)
group.colors <- c("black", "black")
group.fill <- c("black", "red")
xyplot(predict_attend/1000 ~ attend/1000 | training_test,
       data = dodgers.plotting.frame, groups = bobblehead, cex = 2,
       pch = group.symbols, col = group.colors, fill = group.fill,
       layout = c(2,1), xlim = c(20,65), ylim = c(20,65),
       aspect=1, type = c("p","g"),
       panel=function(x,y,...)
       {panel.xyplot(x,y,...)
         panel.segments(25,25,60,60, col ="black", cex=2)
        
         },
       strip=function(...) strip.default(...,style=1),
       xlab = "Actual Attendance (thousands)",
ylab = "Predicted Attendence (thousands)",
key = list(space = "top",
                    text = list(rev(group.labels), col = rev(group.colors)),
                      points = list(pch = rev(group.symbols),
                                    col = rev(group.colors),
                                    fill = rev(group.fill))))
                       

    
