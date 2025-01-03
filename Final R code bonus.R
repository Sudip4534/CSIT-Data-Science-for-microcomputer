### Answer BoNUS ######
# Load necessary libraries
library(ggplot2)

# Load the dataset
data2 <- read.csv("F:/R for Agricultural Research/Humayun sir/Book2.csv")

# Examine the structure of the dataset to identify variables
str(data2)

# Assuming your dataset has a time or sequential variable (e.g., "Time") and a measurement variable (e.g., "Value")
# Create a line graph with ggplot
ggplot(data2, aes(x = Date.Time, y = Humidity, group = 1)) +  # Adjust 'Time' and 'Value' to your column names
  geom_line(color = "blue") +  # Add a line
  geom_point(color = "red") +  # Optionally, add points to the line
  labs(title = "Line Graph Example", x = "Time", y = "Humidity") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) 

# Save the plot as a JPEG
ggsave("Line_Graph_Humidity.jpeg", width = 8, height = 6, dpi = 300)
