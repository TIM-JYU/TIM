import os 

STATIC_PATH = "./static"
DATA_PATH = "./static/data"

# TODO: TESTS
def verifyDataPath(file):
    thisPath = DATA_PATH + "/" + file.strip()
    if (os.path.dirname(os.path.dirname(thisPath)) == DATA_PATH):
        print(thisPath)
        print("Path successfully verified")
        return True
    else: 
        print(os.path.dirname(os.path.dirname(thisPath)) + "\n" 
                + DATA_PATH)
        print("Path may be corrupt")
        return False
