def identity(x):
    return x

a = {3:5}
print(a)
a = [identity(identity(4)),identity(1), 3,5]
print(a)
a = (identity(identity(4)),identity(1), 3,5,)
print(a)
