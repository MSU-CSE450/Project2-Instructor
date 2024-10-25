// We can shadow a variable inside a block.
var num = 1;
print(num);
{
  var num = 2;
  print(num);
}
print(num);
