
# Pushdown OO (Version extracted from APAC second delievery)

This is just analytic engine *without* `play` framework. 

## Environment

### Scala 2.9.1 

The reason didn't move to 2.10: 
* StackOverflow error in Scala 2.10
* Code changes such as case class must have parameters
* Scala 2.9.1 works perfectly with Play 2.0.4

### JVM -- compiled successfully on the following options:

##### OpenJDK Runtime Environment (IcedTea6 1.11.5) (6b24-1.11.5-0ubuntu1~12.04.1). 

##### If you are on Mac, use JDK 6 or 7 later (Compiling openJDK on Mac is complicated and prone to fail).

*NOTE*, the default sbt configuration needs to be changed.

Change  the sbt file:

	/usr/local/bin/sbt

to the following lines:

       #!/bin/sh
       test -f ~/.sbtconfig && . ~/.sbtconfig
       exec java -Xmx1024M -Xss2G  ${SBT_OPTS} -jar /usr/local/Cellar/sbt/0.13.0/libexec/sbt-launch.jar "$@"

(You can change the stack size to 1024M or smaller)
In this way, no stackoverflow compilation error using the native JRE on Mac.

### Graphviz 

For converting to svg dyck state graph. (It will get choked on large dot files)

## Compile

	cd pdafordalvik
	ant compile
	ant jar

### Note that the subproject jdex2sex should be compiled too. To do so:

    	 cd jdex2sex
	 make clean
	 make compile
	 

## Run
Still in `pdafordalvik` folder:

```
java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis \
    [--k <number>] [--gc] [--lra] [--aco] [--godel] [--dump-graph] \
    [--interrupt-after <number-of-states>] \
    [--interrupt-after-time <number of minutes>] \
    path/to/your/filename.apk
```

e.g.

```
java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis \
    --k 1 --gc --lra --aco --godel --dump-graph ./test/Bookworm.apk
```

### For Intent Fuzzer

```
java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis \
    --k 1 --gc --lra --aco --godel --for-intent-fuzzer --intraprocedural \
    ./test/Twitter_3.7.1.apk
```

*NOTE*: Recently, I added the flow-sensitive paths in text (apposed to in graph before) with intent operations/data involves. However, the output is produced after the depth first search on the analyzed graphs, which can take a long time!**

*NOTE*: In case of large apps, please use JVM options (before -jar) to increase run time heap/stack like this (or larger):

```
-XX:MaxPermSize=512m -Xms512m  -Xmx1024M -Xss1024m
```

### For DaCapo benchmark evaluation
* The benchmark apks locates in benchmark-dacapo-apks
* During analysis, `--obranches [number]` for branch optimization to termiate fast safely.