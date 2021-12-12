from functools import reduce, partial
from operator import add

def sliceIterator(lst, sliceLen):
    for i in range(len(lst) - sliceLen + 1):
        yield lst[i:i + sliceLen]

def pipeline(seed, *funcs):
    return reduce(lambda accu,func: func(accu), funcs, seed)

def read_file(name):
    with open(name) as file:
        lines = list(map(lambda s:int(s.strip()),
                         file.readlines()))
    return lines

def listSum(lst):
    return reduce(add, lst, 0)

def countIncreases(lst):
    return len([1 for (x,y) in sliceIterator(lst,2) if x < y])

nums = read_file('input.txt')

print(countIncreases(nums))
print(pipeline(
    sliceIterator(nums,3)
    , partial(map, listSum)
    , list
    , countIncreases))
