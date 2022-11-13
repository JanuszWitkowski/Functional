from typing import Any
from numbers import Number

def isEmpty(li : list) -> bool:
    return len(li)==0

def head(li : list) -> Any: 
    return li[0]

def tail(li : list) -> list:
    return li[1:]

def clear_list(li : list) -> list:
    if isEmpty(li):
        return []
    return [head(li)] + [e for e in tail(li) if head(li) < e]

# def index_of_max(li : list) -> Number:
#     if isEmpty(li):
#         return -1
#     return 

def ssm(li : list) -> list:
    # clearList(list)
    # incrementDistance(sublist, id, dists, paths)
    # getLongestPath(paths)
    dists = [0 for _ in li]
    dists[0] = 1
    paths = [[e] for e in li]
    for i in range(1, len(li)):
        imax = 0
        max = dists[0]
        for j in range(1, i):
            if li[j] < li[i] and max < dists[j]:
                imax = j
                max = dists[j]
        dists[i] = dists[imax] + 1
        paths[i] = [e for l in [paths[imax], paths[i]] for e in l]
    imax = 0
    max = dists[0]
    for i in range(len(dists)):
        if max < dists[i]:
            imax = i
            max = dists[i]
    return paths[imax]

# my_list = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8,4,6,2]
my_list = [3,1,69,4,5,6,7,8,9,0,77,1]
print(my_list)
my_cld_list = clear_list(my_list)
print(my_cld_list)
my_longest_subpath = ssm(my_cld_list)
print(my_longest_subpath)
