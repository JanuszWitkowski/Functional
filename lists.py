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
