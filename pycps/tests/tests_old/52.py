try:
  try:
    try:
      print("Begin...")
      raise "hi1"
    except:
      raise "hi2"
  except:
    raise "hi3"
except:
  print("Good!")

