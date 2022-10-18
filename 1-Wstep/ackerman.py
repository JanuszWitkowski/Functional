from typing import Any
from numbers import Number

def ackerman(m : Number, n : Number) -> Number :
    if m == 0:
        return n + 1
    elif n == 0:
        return ackerman(m-1, 1)
    else:
        return ackerman(m-1, ackerman(m, n-1))

print(ackerman(3, 4))
