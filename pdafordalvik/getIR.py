import os
import shutil
import subprocess
import sys

#the script folder
CURFOLD=os.getcwd()


APK_SRC=  sys.argv[1] #"apks"
APK_TOOL =  "../apktool/apktool"
JDex2Sex_CP= os.path.join(os.getcwd(),'../jdex2sex/bin')

# in the script fodler
#print os.getcwd()

JA_PATH = "/usr/bin/java"
DDX  = JA_PATH +" -cp " + JDex2Sex_CP + " hu.uw.pallergabor.dedexer.Dedexer"
STANDARD_DEX_FN = "classes.dex"
SEXP_OUT = "dedexout"
pd_dict = {}

SCRT_GET_PERMS = "./GetManifestInfo.py"
SCRT_GET_Hands = "./gethandlers.py"

PY_PATH = "/usr/bin/python"
RKT_PATH = "/Applications/Racket/bin/racket"
DOT_PATH  = "/usr/local/bin/dot"






def apkfil(f):
    p1, pext = os.path.splitext(f)
    return pext == ".apk"
def dotfil(f):
    p1, pext = os.path.splitext(f)
    return pext == ".dot"

def build_abspath(cur_p, fnames):
    fs = filter(apkfil, fnames)
    if fs:
        return map(lambda(x): os.path.join(cur_p, x), fs)

def build_abspath_kind(cur_p, fnames, fil):
    fs = filter(fil, fnames)
    if fs:
        #print fs
        return map(lambda(x):os.path.join(cur_p, x), fs)


def build_projdir_apkf_dict(abs_fnames):
    for absfp in abs_fnames:
        proj_nm, ext = os.path.splitext(absfp)
        pd_dict[absfp] = proj_nm

def toSVG(proj_dir):
    print "...Turn dot file to svg file!"
    call_graph_path = os.path.join(proj_dir, "state-graph")
    graph_dir = os.walk(call_graph_path)
    if len(list(graph_dir)) == 0:
        print "no graph file generated"
        exit()
    
    fps = []
    for cur_p, dir, fnames in graph_dir:
        #print fnames
        fps = build_abspath_kind(cur_p, fnames, dotfil)
    
    print fps
    svg_files = []
    for f in fps:
        name, ext = os.path.splitext(f)
        #print name
        svg_fn = name + ".svg"
        #print "%s finished" % svg_fn
        svg_files.append(svg_fn)
        #svg_fn.replace("$", "_")
        #os.system("dot -Tsvg %s > %s"%(f,svg_fn))
        os.system(DOT_PATH + " -Tsvg %s > %s" %(f,svg_fn))
    print "All dot->svg done!"




def run():
    
    print APK_TOOL
    print SEXP_OUT
    print APK_SRC
    
    for cur_p, dir, fnames in os.walk(APK_SRC):
        print cur_p
        fps = build_abspath(cur_p, fnames)
        
        if fps is None:
            return
        
        print "APK_SRC--------"
        print APK_SRC
        
        print "apk dir---------"
        
        
        build_projdir_apkf_dict(fps)
        
        print pd_dict
        
        for apk_fp in pd_dict:
            print "extracting %s" % apk_fp
            prj_d = pd_dict[apk_fp]
            print prj_d
            
            if( subprocess.check_call([APK_TOOL, "d" , "-f", "--no-src", apk_fp, prj_d])):
                print "extracting failed --- Are you sure valid apk?"
                return
            print "finished extracting apk file"
                        
            sex_out=os.path.join(os.getcwd(), prj_d, SEXP_OUT)
            
            print "sex out in getIR 1 is*******->"
            print sex_out
            if(not os.path.exists(sex_out)):
                subprocess.check_call(["mkdir", sex_out])
            # print os.path.exists(sex_out)
            sdx = os.path.join(os.getcwd(), prj_d, STANDARD_DEX_FN)
            print os.path.exists(sdx)
            print sex_out
            print sdx
            sex_out = os.path.join(os.getcwd(),sex_out)
            sdx = os.path.join(os.getcwd(), sdx)
            print "sex_out 2 ------*****>"
            print sex_out

            print DDX
        
            oldcwd = os.getcwd()
            print "old"
            print oldcwd
            os.chdir(os.path.join(os.getcwd(), "../jdex2sex"))
            
            J_BIN = os.path.join(os.getcwd(),"bin")
            K_CP = os.path.join(J_BIN, "hu", "uw", "pallergabor", "dedexer", "hu.uw.pallergabor.dedexer.Dedexer")
            
            if(subprocess.check_call(["make", "bin"])):
                print "cant make bin"
                return
    
            #            if(subprocess.check_call([DDX, "-d", sex_out, sdx])):
            if(subprocess.check_call([JA_PATH, "-cp", J_BIN,
                                      "hu.uw.pallergabor.dedexer.Dedexer",
                                      "-d",
                                      sex_out, sdx])):
                
                print "JSex2Dex failed!"
                return
            print "finished Sexing..."

            os.chdir(oldcwd)

            if(subprocess.check_call([PY_PATH, SCRT_GET_PERMS, prj_d])):
                print "Parsing Manifest file failed"
                return
            if(subprocess.check_call([PY_PATH, SCRT_GET_Hands, prj_d])):
                print "Handlers getting falied"
                return




if __name__ == "__main__":
    run()
