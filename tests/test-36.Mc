// Step count for the Collatz Conjecture
var n = 27; // Any positive integer
var count = 0;

print("Start value = {n}");
while (n != 1) {
  if (n % 2 == 0) {
    n = n / 2;
  } else {
    n = 3 * n + 1;
  }
  count = count + 1;
  print("Step {count}: {n}");
}

print("Steps to reach 1: {count}");
