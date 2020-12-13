f = open("./input.txt", "r")
lines = f.readlines()
values = []

for line in lines:
    values = values + [int(line.strip())]

preamble = values[:25]
message = values[25:]

print(preamble)

def check(preamble, value):
    is_valid = False
    for x in preamble:
        for y in preamble:
            if x+y == value:
                is_valid = True
    return is_valid


first_invalid_value = 0
i = 0
found = False

while i < len(message) and not(found):
    if check(preamble, message[i]):
        preamble = preamble[1:]
        preamble.append(message[i])
        i += 1
    else:
        first_invalid_value = message[i]
        found = True

print(first_invalid_value)
