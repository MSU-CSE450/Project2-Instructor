// Make sure assignments work inside other expressions.
var v1 = 0;
var v2 = 1 + (v1 = 10) * 25;
print(v1);
print(v2);
