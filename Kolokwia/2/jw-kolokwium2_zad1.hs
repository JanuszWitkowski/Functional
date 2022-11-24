-- Napisz funkcję zbierającą duplikaty, np.
-- collect [0,0,2,0,2,3,3,1,3] = [[0,0,0],[1],[2,2],[3,3,3]]
collect [] = []
collect (x:xs) = (foldl (\e i -> if i == x then x:e else e) [x] xs):(collect (filter (/= x) xs))    -- foldl zbiera duplikaty w jedną listę, a potem konkatenuje się z przefiltrowaną listą (z rekurencyjnym odwołaniem)

-- przykład: collect list1 = [[0,0,0],[2,2],[3,3,3],[1]]
list1 = [0,0,2,0,2,3,3,1,3]
