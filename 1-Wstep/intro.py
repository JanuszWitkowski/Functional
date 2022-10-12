from math import prod
from os import access
from typing import Any
from numbers import Number

# Wprawki w programowaniu w Python'ie
# Staramy się programować starannie, używając gdzie można "Type Hinting" -
#   po to są te importy w nagłówku
# Ale: jeśli student posługuje się starszymi wersjami Pythona,
# to można mu odpuścić typowanie

# to jest funkcja do eksperymentowania w przyszłości
def myFnc(x, y):
    return x * (x + y)


# Python ma listy
# robimy kilka swoich funkcji pomocniczych

def isEmpty(li : list) -> bool:
    return len(li)==0

# pierwszy element listy
def head(li : list) -> Any: 
    return li[0]

# Ogon listy
def tail(li : list) -> list:
    return li[1:]


# rekurencyjna wersja sumy
def mySum(li : list) -> Number :
    if isEmpty(li) : 
      return 0
    else:  
      return head(li) + mySum(tail(li))


# ZADANIE: przerób mySum na wersję indukcji ogonowej

def mySum2(li : list, acc : Number) -> Number :
    if isEmpty(li):
        return acc
    else:
        return mySum2(tail(li), acc + head(li))

# ZADANIE 11
def sum_of_list(li : list) -> Number :
    if isEmpty(li):
        return 0
    else:
        return head(li) + sum_of_list(tail(li))

def product_of_list(li : list) -> Number:
    if isEmpty(li):
        return 1
    return head(li) * product_of_list(tail(li))

def min_from_list(li : list, min : Number) -> Number:
    if isEmpty(li):
        return min
    elif head(li) < min:
        return min_from_list(tail(li), head(li))
    else:
        return min_from_list(tail(li), min)

def max_from_list(li : list, max : Number) -> Number:
    if isEmpty(li):
        return max
    elif head(li) > max:
        return max_from_list(tail(li), head(li))
    else:
        return max_from_list(tail(li), max)

list = [3, 1, 4, 2]
# print(mySum(list))
# print(mySum2(list, 0))

print(sum_of_list(list))
print(product_of_list(list))
print(min_from_list(list, 123))
print(max_from_list(list, -123))
