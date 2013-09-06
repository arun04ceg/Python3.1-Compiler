def f():
 raise 3 ;

try:
 f()
 print("fail")
except:
 print(20)
