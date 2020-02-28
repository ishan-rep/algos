from collections import deque
from copy import deepcopy
from collections import defaultdict

def findLadders( start, end, dictV):
    print "hello"
    if start == end:
        return [[start]]
    dictV = list(set(dictV))
    #print dictV
    q = deque()
    q.append([start])
    count = None
    result = []
    d = defaultdict(set)
    l = {}
    l[start] = 0

    while q:
        s = q.popleft()
        if count is not None and len(s) > count:
            break
        for i in getChildren(s[-1],end,dictV):
            if i in s:
                continue


            if i == end:
                count = len(s)
                result.append(s[:]+[i])
            else:
                q.append(s[:]+[i])
    return result
    temp = []
    result = []
    if not d[end]:
        return result
    temp.append([end])
    #print result
    #print d
    #print d
    while temp:
        #print temp
        x = temp.pop(0)

        if x[-1] in d and d[x[-1]]:
            for i in d[x[-1]]:
                temp.append(x[:] + [i])
        else:
            result.append(x[::-1])


    return result





def getChildren(start , end, dictV):
    children = []
    for i in dictV:
        flag = 0
        for j in range(len(i)):
            if start[j] != i[j]:
                if flag:
                    flag = 0
                    break
                flag = 1
        if flag:
            children.append(i)
    return children



start = raw_input()
end = raw_input()
size = int(raw_input())
li = []
x = raw_input()
li = x.split(' ')
# for i in range(size):
#     li.append(raw_input())
print len(li)

li.append(start)
li.append(end)
print findLadders(start, end, li)
