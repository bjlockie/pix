#!/usr/bin/env python3


# Import the os module, for the os.walk function
import os


def annotate(fname):
    ok=False
    if fname.endswith( '.jpg' ):
        newFnameTuple = fname.partition('.jpg')
        ok=True
    elif fname.endswith( '.JPG' ):
        newFnameTuple = fname.partition('.JPG')
        ok=True
    
    if ok:        
        newFname = newFnameTuple[0] + '_annotated.jpg'
        #print('fname={0}'.format(newFnameTuple[0]))
        infoTextFname = newFnameTuple[0] + '.txt'
        #print('infoFfname={0}'.format(infoTextFname))
        
        # slurp info text into a list of lines
        f = None
        infoText=None
        try:
            f = open(infoTextFname, 'r+')
        except IOError:
            print("WARNING: Couldn't open the info text file ({0})".format(infoTextFname))
            pass
        else:
            infoText = f.readlines()
        finally:
            if f: f.close()

		# if loaded
        if infoText:
            # strip carriage return and line feed
            infoTextWhen = 'When: '+infoText[0].rstrip('\n\r')

            # replace double quotes, single quotes and round brackets with curly brackets
            infoTextWhen=infoTextWhen.replace('"', '\\')
            infoTextWhen=infoTextWhen.replace("'", "")
            infoTextWhen=infoTextWhen.replace("(", "{")
            infoTextWhen=infoTextWhen.replace(")", "}")

            # strip carriage return and line feed
            infoTextWhoWhat = 'Who/What: '+infoText[1].rstrip('\n\r')

            # replace double quotes, single quotes and round brackets with curly brackets
            infoTextWhoWhat=infoTextWhoWhat.replace('"', '\\')
            infoTextWhoWhat=infoTextWhoWhat.replace("'", "")
            infoTextWhoWhat=infoTextWhoWhat.replace("(", "{")
            infoTextWhoWhat=infoTextWhoWhat.replace(")", "}")

            # strip carriage return and line feed
            infoTextWhere = 'Where: '+infoText[2].rstrip('\n\r')

            # replace double quotes, single quotes and round brackets with curly brackets
            infoTextWhere=infoTextWhere.replace('"', '\\')
            infoTextWhere=infoTextWhere.replace("'", "")
            infoTextWhere=infoTextWhere.replace("(", "{")
            infoTextWhere=infoTextWhere.replace(")", "}")

            #print('infoTextWhere={0}'.format(infoTextWhere))
            #print('%s' % infoTextFname)
            #print('%s' % newFname)
            fromLeft = 2
            fromTop = 40
            downLine = 40
        
            # convert "1 1.jpg" -background YellowGreen -splice 0x150 -font Courier -pointsize 36 -draw 'text 2,40 "When"' -draw 'text 2,80 "Who"' -draw 'text 2,120 "Where"' "1 1_annotated.jpg"

		    # call convert from ImageMagick to make a copy of the jpg (append "_annotated" to the file name) with the height increased at the top and the text written            
            cmd='convert -background YellowGreen -splice 0x150 -font Courier -pointsize 36 -draw \'text '+ str(fromLeft)+','+str(fromTop)+' \"'+infoTextWhen+'\"\' -draw \'text '+ str(fromLeft)+','+str(fromTop+40)+' \"'+infoTextWhoWhat+'"\' -draw \'text '+ str(fromLeft)+','+str(fromTop+80)+' "'+infoTextWhere+'"\' "' +fname+'" '+'"'+newFname+'"'
            
            # print the command we are running
            print('cmd={0}'.format(cmd))
            os.system( cmd )


def listdirs(rootdir):
    for it in os.scandir(rootdir):
        if it.is_dir():
            #print('dir: {0}'.format(it.path))
            # recursively call listDirs on each directory
            listdirs(it)
            pass
        else:
            #print('path, file: {0}, {1}'.format( it.path, it.name ))
            # annotate 
            annotate(it.path)
 
# Set the directory you want to start from
rootDir = '.'
listdirs(rootDir)
