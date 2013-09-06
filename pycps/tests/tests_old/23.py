a = 3
def b():
 a = 4
 def c():
  return a
 print(c())

b()
