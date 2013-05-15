import xml.dom.minidom as md
import sys
import os

PROJ_DIR = sys.argv[1]

ManFileName = os.path.join(PROJ_DIR, "AndroidManifest.xml")
OUT = os.path.join(PROJ_DIR, "man_perms.txt")

PermElemName = 'uses-permission'
PermAttrName = 'android:name'


PERMS = []

of = open(OUT, 'w+')
#of.write("(")

def getPermissions():
    man_dom = md.parse(ManFileName)
    
    perms_lst = man_dom.getElementsByTagName(PermElemName)
    if perms_lst.length == 0 :
#        print "empty permissions"
        #of.write(")")
        of.close()
        return
#    print perms_lst
    for perm_dom in perms_lst:
        if perm_dom.hasAttribute(PermAttrName):
 #           print "has"
            PERMS.append(str(perm_dom.getAttribute(PermAttrName)))
            of.write(str(perm_dom.getAttribute(PermAttrName)))
            of.write ('\n')
#    of.write(")")
    of.write("\n")
    of.close()
    

getPermissions()
print PERMS

        
    
    
