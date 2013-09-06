def identity(x):
    return x

def identity2(x):
    return 2*x

fncs = [identity, identity2]

for ii in [1, 2, 3, 4, 5]:
    print(identity(ii))
    print(identity2(ii))

