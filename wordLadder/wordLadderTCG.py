import random


def change(s, i, j):
    return s[:i] + j + s[i+1:]


start = raw_input()
dist = int(raw_input())
ans = [start]
count = 0
alpa = "abcdefghijklmnopqrstuvwxyz"
temp = start

while(count < dist):
    print "temp = ", temp
    l = len(temp)
    x = random.randrange(0, l, 1)
    xxx = []
    for i in alpa:
        temp1 = change(temp, x, i)
        if temp1 == temp:
            continue
        else:
            ans.append(temp1)
            xxx.append(temp1)
    xxxl = len(xxx)
    y = random.randrange(0,xxxl,1)
    temp = xxx[y]
    count+=1
print ans