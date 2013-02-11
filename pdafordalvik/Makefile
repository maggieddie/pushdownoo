compile:
	ant compile
	ant jar

run-batik:
	java -XX:MaxPermSize=512m -Xms1024m  -Xmx2048m -Xss1536m -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --k 1 --gc --lra --aco --godel --obranches 60 ./test/BatikAndroid2.apk


run-antlr-godel:
	java -XX:MaxPermSize=512m -Xms1024m  -Xmx2048m -Xss1536m -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --k 1 --gc --lra --aco --godel --obranches 60  ./test/AndroidAntlr.apk

run-bloat-godel:
	java -XX:MaxPermSize=512m -Xms1024m  -Xmx2048m -Xss1536m -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --k 1 --gc --lra --aco --godel --obranches 60  ./test/BloatAndroid.apk

run-jython-godel:
	java -XX:MaxPermSize=512m -Xms1024m  -Xmx2048m -Xss1536m -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --k 1 --gc --lra --aco --godel --obranches 60  ./test/Jyphton-unalighed.apk

run-pmd-godel:
	java -XX:MaxPermSize=512m -Xms1024m  -Xmx2048m -Xss1536m -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --k 1 --gc --lra --aco --godel --obranches 60  ./test/AndroidPmd-seqcreater2.apk

run-xalan-godel:
	java -XX:MaxPermSize=512m -Xms1024m  -Xmx2048m -Xss1536m -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --k 1 --gc --lra --aco --godel --obranches 60  ./test/XalanAndroid-debug-unaligned.apk

run-luindex-godel:
	java -XX:MaxPermSize=512m -Xms1024m  -Xmx2048m -Xss1536m -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --k 1 --gc --lra --aco --godel --obranches 60  ./test/LuIndexAndroid.apk

run-lusearch-godel:
	java -XX:MaxPermSize=512m -Xms1024m  -Xmx2048m -Xss1536m -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --k 1 --gc --lra --aco --godel --obranches 60  ./test/AndroidLusearch.apk

