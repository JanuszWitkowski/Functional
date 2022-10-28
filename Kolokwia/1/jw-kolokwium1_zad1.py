from typing import Any
from numbers import Number

# Implementacje funkcji pomocniczych
def isEmpty(li : list) -> bool:
    return len(li)==0

# Pierwszy element listy
def head(li : list) -> Any: 
    return li[0]

# Ogon listy
def tail(li : list) -> list:
    return li[1:]

# IMPLEMENTACJA FUNKCJI W ZADANIU 1
def zadanie1(li : list, i = 0, acc = 0) -> Number:
    if isEmpty(li):
        return acc
    # Zwracamy w argumentach ogon listy, inkrementujemy index, oraz dodajemy do akumulatora wymagana wartosc.
    return zadanie1(tail(li), i + 1, acc + (pow(head(li), 3) * i))


# Przykladowe wywolanie funkcji
list = [4, 3, 2, 1] # zadanie1([4,3,2,1]) powinno byc rowne 0*4^3 + 1*3^3 + 2*2^3 + 3*1^3 = 0 + 27 + 16 + 3 = 46
print(zadanie1(list))
