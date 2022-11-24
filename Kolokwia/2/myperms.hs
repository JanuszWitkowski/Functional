myperms [] = [[]]
myperms xs = [x:p | x <- xs, p <- myperms (filter (/= x) xs)]

