
import fileinput

familyTree = {
}

def parseCommand(command, statement):
    if len(command)==0:
        return
    if command[0] == 'E' or command[0] == 'e':
        createNewRelation(command)
    if command[0] == 'X' or command[0] == 'x' or command[0] == 'W' or command[0] == 'w':
        print(statement)
        getRelatedList(command, command[0])
        print("\n")

def createNewRelation(command):
    for x in range (1,len(command)):
        if command[x] not in familyTree:
            familyTree[command[x]] = {
                'parents':[],
                'spouses':[],
                'children':[]
            }

    for x in range (1,len(command)):
        if x == 1:
            if command[2] not in familyTree[command[1]]['spouses']:
                familyTree[command[1]]['spouses'].append(command[2])
        if x == 2:
            if command[1] not in familyTree[command[2]]['spouses']:
                familyTree[command[2]]['spouses'].append(command[1])
        if x == 3:
            familyTree[command[3]]['parents'].append(command[1])
            familyTree[command[3]]['parents'].append(command[2])
            familyTree[command[1]]['children'].append(command[3])
            familyTree[command[2]]['children'].append(command[3])

def getRelatedList(command, flag):
    ret = set()
    person = command[len(command)-1]
    relation = command[1] if flag == "W" else command[2]
    if person in familyTree:
        if relation == 'Ancestor' or relation == 'ancestor':
            getAncestors(person, ret)
        if relation == 'Spouse' or relation == 'spouse':
            for s in familyTree[person]['spouses']:
                ret.add(s)
        if relation == 'Cousin' or relation == 'cousin':
            if len(command) == 3:
                getCousins(person, ret, 1, 'true')
            else:
                if len(command) == 5:
                    getCousins(person, ret, int(command[3]), 'true')
                else:
                    getCousins(person, ret, int(command[2]), 'true')
        if relation == 'Sibling' or relation == 'sibling':
            getCousins(person, ret, 0, 'true')
        if relation == 'Child' or relation == 'child':
            for s in familyTree[person]['children']:
                ret.add(s)
        if relation == 'Unrelated' or relation == 'unrelated':
            getUnrelated(person, ret)
    else:
        print(person, " is not in familyTree")

    if(flag == 'W'):
        for s in sorted(ret):
            print(s)
    elif(flag == 'X'):
        if command[1] in ret:
            print("Yes")
        else:
            print("No")

def getCousins(person, baseSet, level, modified):
    if person == '':
        return
    if level == 0:
        getSiblings(person,baseSet)
        return

    topAncestors = []
    familyLineSet = set()

    getAncestorsOnLevel(person, topAncestors, level, familyLineSet)

    for p in topAncestors:
        getDescendantsOnLevel(p, baseSet, level, familyLineSet)

    if modified == 'true':
        temp = baseSet.copy()
        set2 = set()
        getAncestors(person, set2)
        for pers in set2:
            getCousins(pers, baseSet, level, 'false')

        for pers in temp:
            getDescendants(pers, baseSet)


def getAncestorsOnLevel(person, list_, level, familyLineSet):
    if person == '':
        return

    if level<0:
        list_.append(person)
        return

    familyLineSet.add(person)
    if len(familyTree[person]['parents']) == 0 : #DONE: need to see however we are representing parents
        return

    getAncestorsOnLevel(familyTree[person]['parents'][0], list_, level-1, familyLineSet)#DONE: get mom
    getAncestorsOnLevel(familyTree[person]['parents'][1], list_, level-1, familyLineSet)#DONE: get DAD


def getAncestors(person, set_, isFirst='true'):
    if person == '':
        return

    if len(familyTree[person]['parents']) == 0: #DONE
        set_.add(person)
        return

    if isFirst == 'false':
        set_.add(person)

    getAncestors(familyTree[person]['parents'][0], set_, 'false') #DONE
    getAncestors(familyTree[person]['parents'][1], set_, 'false') #DONE

def getDescendants(person, set_):
    if person == '':
        return

    set_.add(person)

    if len(familyTree[person]['children']) == 0:
        return

    for child in familyTree[person]['children']:
        getDescendants(child, set_)

def getDescendantsOnLevel(person, set_, level, familyLineSet):
    if person == '':
        return

    if level < 0:
        set_.add(person)
        return

    for child in familyTree[person]['children']:
        if child not in familyLineSet:
            getDescendantsOnLevel(child, set_, level-1, familyLineSet)

def getSiblings(person, set_):
    if person == '':
        return
    if len(familyTree[person]['parents'])==0:
        return

    for par in familyTree[person]['parents']:
        for child in familyTree[par]['children']:
            if child != person:
                set_.add(child)

def getUnrelated(person, set_):
    for key in familyTree:
        set_.add(key)

    ancestors = set()
    getAncestors(person, ancestors)

    descendants = set()
    for anc in ancestors:
        getDescendants(anc, descendants)

    for des in descendants:
        ancestors.add(des)

    for person in ancestors:
        set_.remove(person)


for line in fileinput.input(): ############### This where input is accepted
    command = line.split()
    statement = line
    parseCommand(command, statement)
