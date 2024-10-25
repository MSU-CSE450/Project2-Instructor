// Test AND and OR operators
var high = 1000;
var low = 0.1;
print(high > 50 && low < 1);  // true  && true  ->  true
print(high > 50 || low < 1);  // true  || true  ->  true
print(high < 50 && low < 1);  // false && true  ->  false
print(high < 50 || low < 1);  // false || true  ->  true
print(high > 50 && low == 1); // true  && false ->  false
print(high > 50 || low == 1); // true  || false ->  true
print(high < 50 && low == 1); // false && false ->  false
print(high < 50 || low == 1); // false || false ->  false
