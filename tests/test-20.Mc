// AND and OR should short circuit if the answer is already determined
// by the first clause.

var val1 = 1;
var val2 = 2;

print(val1 == 10 && (val2=0));  // Should short circuit and NOT change val2
print(val2);
print(val2 || (val1=17));  // Should short circuit and not change val1
print(val1);
