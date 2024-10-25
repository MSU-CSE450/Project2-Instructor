// Print the first 20 Fibonacci
var val1 = 0;
var val2 = 1;
print(val1);
print(val2);
var step = 2;
while (step < 20) {
  var sum = val1 + val2;
  val1 = val2;
  val2 = sum;
  print(sum);
  step = step + 1;
}
