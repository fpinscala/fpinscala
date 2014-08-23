/*
 * Exercise 9: Write a program that reads degrees fahrenheit as `Double` values from a file,
 * converts each temperature to celsius, and writes results to another file.
 */

// This process defines the here is core logic, a transducer that converts input lines
// (assumed to be temperatures in degrees fahrenheit) to output lines (temperatures in
// degress celsius). Left as an exercise to supply another wrapper like `processFile`
// to actually do the IO and drive the process.
def convertFahrenheit: Process[String,String] =
  filter((line: String) => !line.startsWith("#")) |>
  filter(line => line.trim.nonEmpty) |>
  lift(line => toCelsius(line.toDouble).toString)

def toCelsius(fahrenheit: Double): Double =
  (5.0 / 9.0) * (fahrenheit - 32.0)
