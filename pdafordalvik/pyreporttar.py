import os
import shutil
import subprocess
import sys

CUR_GRAPH_PATH = sys.argv[1]
TAR_NAME = sys.argv[2]

def run():

    print "current script path:"

    oldcwd = os.getcwd()
    
    print oldcwd

    os.chdir(CUR_GRAPH_PATH)

    files = CUR_GRAPH_PATH + "/*"

    cmd0 = "cp ../statistics/* ."

    os.system(cmd0)
    
    cmd = "tar -zcvf " + TAR_NAME + " .*"
    
    #os.system("tar -zcvf graph.tar.gz ./*")

    os.system(cmd)
    
  #  if(subprocess.check_call(["tar", "-zcvf", "graph.tar.gz", CUR_GRAPH_PATH])):
   #     print "tar graph finished"
    #    os.chdir(oldcwd)
     #   return
    
    os.chdir(oldcwd)
    

if __name__ == "__main__":
    run()
