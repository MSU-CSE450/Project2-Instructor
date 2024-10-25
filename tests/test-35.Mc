// Calculate the greatest common denominator
var a = 48;
var b = 18;

// Euclidean algorithm to find GCD
while (b != 0) {
  if (a > b) {
    a = a - b;
  } else {
    b = b - a;
  }
}

print("The GCD of 48 and 18 is {a}.");
