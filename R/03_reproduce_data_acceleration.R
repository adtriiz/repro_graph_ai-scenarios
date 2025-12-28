# Create AI Acceleration projection
## Will try to reproduce the underlying function from the data
sequence_acceleration <- c(38689, 43763, 50791, 60554, 74147, 93101, 119560, 156527, 
                           208205, 280478, 381585, 523058, 721044, 998149, 1386021, 
                           1928965, 2689010, 3752998, 5242505, 7327739, 10246989, 
                           14333866, 20055416, 28065508, 39279564, 54979164, 76958528)
time <- 1:(length(sequence_acceleration))
plot(
  time,
  sequence_acceleration
)

## Exponential model
model_exp <- lm(log(sequence_acceleration) ~ time)
summary(model_exp)
prediction_exp <- exp(predict(model_exp))

## Quadratic model (degree 2)
model_poly2 <- lm(log(sequence_acceleration) ~ time + I(time^2))
summary(model_poly2)
prediction_poly2 <- exp(predict(model_poly2))

df_predictions <- tibble(
  time = time,
  sequence_acceleration = sequence_acceleration,
  prediction_exp = prediction_exp,
  prediction_poly2 = prediction_poly2,
  error_exp = sequence_acceleration - prediction_exp,
  error_poly2 = sequence_acceleration - prediction_poly2
)

# Plot predictions
ggplot(df_predictions,aes(x = time)) +
  geom_point(aes(y = sequence_acceleration)) +
  geom_line(aes(y = prediction_exp), colour = "red") +
  geom_line(aes(y = prediction_poly2), colour = "blue")

# Plot absolute errors
ggplot(df_predictions, aes(x = time)) +
  geom_point(aes(y = abs(error_exp)), colour = "red") +
  geom_point(aes(y = abs(error_poly2)), colour = "blue")

# Final Conclusion
## Underlying data is not based on any function, but just data points that kind of ressemble an exponential or polynomial function

  