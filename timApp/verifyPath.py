import os 

STATIC_PATH = "./static"
# TODO: TESTSS
def verifyPath(name, file):
    thisPath = STATIC_PATH + "/" + file.strip()
    print(os.path.dirname(thisPath) + "\n" + STATIC_PATH)
    if (os.path.dirname(thisPath) == STATIC_PATH):
        print("Path successfully verified")
        return True
    else: 
        print("Path may be corrupt")
        return False
