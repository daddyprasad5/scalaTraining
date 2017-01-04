def sum (f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
}

sum(x=>x*2)(1,3) // should be 12

def product (f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
}

product(x => x+1)(1,3) //should be 24

def fac(n: Int): Int = {
  product(x=>x)(1,n)
}

fac(5) // should be 120

def sp (f: Int => Int, uo: Int, g: (Int, Int) => Int)(a: Int, b: Int): Int = {
  if (a > b) uo
  else g(f(a), sp(f, uo, g)(a + 1, b))
}

sp(x=>x*2, 0, _plus)