// Make sure we can nest multiple levels!
var a = 10;
print("1: a = {a}");
if (a > 5) {
  a = a * 40 % 17;  // a should be 9
  print("2: a = {a}");
  if (a < 10) {
    a = a ** 2 - 9; // a should be 160
    print("3: a = {a}");
    if (a / 8 < 21) {
      a = (a/10) ** 0.5;  // Square root of 16 is 4.
      print("4: a = {a}");
      if (a > 5) print("Whoops!");
      else print ("Yaaay!");
    }
  }
}
