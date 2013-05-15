import os
import time

dirpath = "/Users/shuying/Documents/bk/wk_if/PlayPushDownOO/public/apks" #"/public/apks"

destpath = "/Users/shuying/Documents/bk/wk_if/PlayPushDownOO/target/scala-2.9.1/classes/public/apks"

old_stat = os.stat(dirpath)
old_contents = os.listdir(dirpath)



g_folders_dict= {}


def getAllFiles(path):
    fileList = []
    for root, subFolders, files in os.walk(path):
        for file in files:
            fileList.append(os.path.join(root, file))
    return fileList

old_allFiles = len(getAllFiles(dirpath))


def apk_paths (apks, gp):
    res = []
    for a in apks:
        res.append(os.path.join(gp,a))
    return res
    
def handle(apks,gp):
    apkps = apk_paths(apks,gp)
    for apk in apkps:
        os.system("cp " + apk + " "+ destpath)

def handle_dir(gp):

    print "handledir???", gp
    print destpath
    os.system("cp -r " +  gp + " " + destpath)
    

def check_handle_g():
    #print "executing check-handle_g"
    #print g_folders_dict
   # print "new st", nst
    
    for gp in g_folders_dict:
        nst = os.stat(gp)
        ont = g_folders_dict[gp][0]

        
        if ont.st_ctime != nst.st_ctime or len(getAllFiles(dirpath)) != old_allFiles:
            
            handle_dir(gp)

            nc = getAllFiles(gp)
            oc = g_folders_dict[gp][1]
            diff = list(set(nc).difference(set(oc)))

            print "nc--------->", nc
            print "oc--------->", oc
            
            print "diff-------->", diff
            if len(diff)!=0:
                handle_dir(gp)
                g_folders_dict[gp][1]=nc
            g_folders_dict[gp][0]=nst
                    
                    
            
    
while True:

    #
    # Sleep for 1 second, you can use float point number,
    # e.g. 0.1 to sleep for 0.1 second.
    #
    time.sleep(1)

    #
    # Get current directory state
    #
    nst = os.stat(dirpath)
    check_handle_g()

    print dirpath
    print nst.st_ctime
    print old_stat.st_ctime

    nallFiles = len(getAllFiles(dirpath))
    print "new all Files: ", nallFiles
    print "old all Files: ", old_allFiles
    
    #
    # Check whether the directory has been changed or not
    #
    if (nst.st_ctime != old_stat.st_ctime) or (nallFiles != old_allFiles):

        #
        # Directory changed, so get its new content
        #

        ncts = os.listdir(dirpath)

        #
        # Get the new added files and dirs by comparing
        # the differences between new content and the old
        # one.
        #
        nfs = set(ncts).difference(set(old_contents))

        # nfs is a Set, we convert it to list for easier use
        lnfs = list(nfs)

        
        
        print "lnfs", lnfs
        #
        # Check whether new files or dirs added or not,
        # sometime the dir change time is different, but
        # no new content added.
        #
        if (len(lnfs) != 0) or (nallFiles != old_allFiles):
            for gf in  lnfs:
                gpath = os.path.join(dirpath, gf)
                allCurFiles = getAllFiles(gpath)
                print "CUR FILE", allCurFiles
                
                g_folders_dict[gpath] = list((os.stat(gpath), getAllFiles(gpath)))
                #  os.listdir(gpath)))
                print g_folders_dict
                
            for gp in g_folders_dict:
                g_folders_dict[gp][1]= getAllFiles(gp)
                
            old_contents = ncts    # save updated new dir content for next check
            old_allFiles = len(getAllFiles(dirpath))

    # save updated new state
    old_stat = nst

    
    # End of loop


