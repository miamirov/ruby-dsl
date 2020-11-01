a = 1;

b = 10;

puts(a+b);

def fun(a, b, c)
  if a == 10
  then
    puts("Good");
  else
    puts("Bad", b, c);
  end
end

fun(1, 2, 3);
fun(10, 0, 0);