import os

path = './grading'
files = os.listdir(path)
test_files = []
for file in files:
    if os.path.splitext(file)[1] == '.test':
        test_files.append('./grading/{}'.format(file))

err_count = 0
for file in test_files:
    flag = False
    print('start test file: {}'.format(file))
    myOutput = os.popen("../../bin/lexer {} | ./parser".format(file)).read()
    stdOutput = os.popen("../../bin/lexer {} | ../../bin/parser".format(file)).read()
    while True:
        try:
            myEnd = myOutput.index("\n")
            stdEnd = stdOutput.index("\n")
            if myOutput[0 : myEnd] != stdOutput[0 : stdEnd]:
                print("my yacc ", myOutput[0 : myEnd])
                print("std yacc", stdOutput[0 : stdEnd])
                flag = True
            myOutput = myOutput[myEnd + 1 :]
            stdOutput = stdOutput[stdEnd + 1 :]
        except ValueError:
            break
    if flag:
        err_count += 1
print('error cnt is {}'.format(err_count))

