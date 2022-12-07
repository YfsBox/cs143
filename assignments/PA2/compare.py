import os

coolfile = "../../examples/arith.cl"
print(coolfile)
myOutput = os.popen("./lexer {}".format(coolfile)).read()
stdOutput = os.popen("../../bin/.i686/lexer {}".format(coolfile)).read()

beginIndex = myOutput.index("#name")
myOutput = myOutput[beginIndex:]

while True:
    myEnd = myOutput.index("\n")
    stdEnd = stdOutput.index("\n")
    if myOutput[0 : myEnd] != stdOutput[0 : stdEnd]:
        print("my flex ", myOutput[0 : myEnd])
        print("std flex", stdOutput[0 : stdEnd])
        print("")
    myOutput = myOutput[myEnd + 1 :]
    stdOutput = stdOutput[stdEnd + 1 :]