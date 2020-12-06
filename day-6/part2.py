alpha = "abcdefghijklmnopqrstuvwxyz"
values = [0]*26

f = open("./input.txt", "r")
lines = f.readlines()
stripped = []

for line in lines:
    stripped = stripped + [line.strip()]

num = 0
groups = 0

for line in stripped:
    if line == "" :
        for val in values :
            if val == num:
                groups += 1 
        num = 0
        values = [0]*26
    else:
        num += 1
        for char in line:
            values[alpha.find(char)] += 1

for val in values :
    if val == num:
        groups += 1 


print(groups)
