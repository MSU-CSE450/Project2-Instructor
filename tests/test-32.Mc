// Test a while loop that has an assignment inside the test, but no body.
var value = 1;
while ((value = value * 2) < 1000);
print(value);
