import os
import xml.dom.minidom as md
import sys

PROJ_DIR = sys.argv[1]

cur_path = os.path.join(PROJ_DIR, "res")
OUT = os.path.join(PROJ_DIR, "handlers.txt")

# the dictionary is not complete!!!!
wigetHandlerDict = {"Button": "android:onClick", "EditText": "android:imeOptions",
          "CheckBox": "android:onClick", "RadioButton": "android:onClick",
          "ToggleButton": "android:onClick"}

of = open(OUT, 'w+')
#of.write("(")


def fil(f):
    p1, pext = os.path.splitext(f)
    return pext == ".xml"
def build_abspath( cur_p, fnames):
    xmls = filter(fil,fnames)
    if xmls:
        print "in build_path"
        print xmls
        return map (lambda(x): os.path.join(cur_p, x), xmls)

def getAllXmlFiles():
    xml_files = []
    for cur_p, dir, fnames in os.walk(cur_path):
        xmls = build_abspath(cur_p, fnames) #filter(fil, fnames)
        if xmls:
            print xmls
            xml_files.extend(xmls)
    return xml_files
            
def getHandlers():
    handlers = []
    xml_files = getAllXmlFiles()
    for xf in xml_files:
        xf_dom  = md.parse(xf)
        for w in wigetHandlerDict:
            w_lst = xf_dom.getElementsByTagName(w)
            if w_lst.length == 0:
                pass
            else:
                for we in w_lst:
                    if we.hasAttribute(wigetHandlerDict[w]):
                        of.write(str(we.getAttribute(wigetHandlerDict[w])))
                        of.write ('\n')
#    of.write(')')
                        
    
print "processing all xml files"
print getAllXmlFiles()
print "finished getting all xml files"
getHandlers()
print "finished processingx"

of.close()

#os.chmod(OUT, 0755)
