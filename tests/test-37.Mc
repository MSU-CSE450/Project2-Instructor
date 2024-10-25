// Check if powers of two + 1 are prime.
var power = 1;
while (power <= 16) {
  var value = 2 ** power + 1;
  var is_prime = 1;
  var test_cap = value ** 0.5;  // Only need to test to square root
  var test_val = 2;
  while (is_prime && test_val <= test_cap) {
    if (value % test_val == 0) {
      is_prime = 0;
    }
    else test_val = test_val + 1;
  }
  if (is_prime) {
    print("{value} is PRIME.");    
  } else {
    var result = value / test_val;
    print("{value} is NOT prime ({value} / {test_val} = {result}).");
  }
  power = power + 1;
}
