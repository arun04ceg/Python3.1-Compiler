

def nested_loop(var): 
  for i in [1,2,3,4,5]:
   print(i)
   try: 
    for j in (7,6,5,4):
      print("begin inner"); print(i); print(j) 
      if i > 4 and j == 4:
        print("raise"); raise "reached 6"
      elif i < 2 and j > 5:
        print("inner break"); break
      elif 2 < i < 4 and j == 4:
        print("inner continue"); continue
      print("end inner")
    print("j after inner:")
    print(j)
   except:
    print("caught exception") 
   if i == 3:
      print("outer continue"); continue
   elif i == var:
      print("outer break"); break  
   print("end outer")

nested_loop(-1)
nested_loop(1)
