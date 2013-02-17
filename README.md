pushdownoo
=========

The pushdown exception-flow analysis of object-oriented languages.


+ Program strcture:

  CRAPLE-LICENCE.txt

  benchmarks -- a set of Dalvik applications (or directly IRs for easy tests) to test the pushdown analyzer.
                Andorid applications released by DARPA can not be distributed online,
                so only the apks ported from DaCapo and the apks downloaded from google store/code are available.
                

  lib -- Scala SDK, and compiler

  src -- source files

  android-knowledge -- configuration files to help analyzer to detect entry points for Android applications.

  JVMClassReport -- Classes specs of JVM loaded in, to deal with exceptions can possibly thrown by JVM (not yet for Davik VM 

  sas13-experiment-data -- data history run locally in the paper

-----------------------------------------------------------------------------------------------------------------------------

Use following commands to compile and test the project (in the project root directory):

ant all       	  -- compiles the project from scratch, builds the JAR file and zipped sources

ant zip.project   -- Packs all sources files into the ZIP archive artifacts/pushdownexflow.zip

ant compile       -- compile the project

ant jar           -- build an executable jar and the jar will be in ./artifacts/

or just the command:

make compile

----------------------------------------------------------------------------------------------------------------------------

How to run:

java -jar artifacts/PushdownOO_Exflow.jar  org.ucombinator.dalvik.cfa.cesk.RunAnalysis  [options] irFolderName

e.g. java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis --gc --lra --interrupt-after 16000  benchmarks/dedexed_tests/sexps_lucene

(for sas13 virutual images: running benchmarks available for distribution)

+ all:

make run-available

+ antlr:

make antlr-pd  or
make antlr-pd-agc-lra

+ lucene:

make lucene-pd or
make lucene-pd-agc-lra

+ pmd:

make pmd-pd
make pmd-pd-agc-lra

+ SwiFTP:

make swiFTP-pd
make swiFTP-pd-agc-lra

----------------
Additional note: 

If you would like to generate the IRs from applications instead of the ones available in the benchmarks/dedexed_tests, 
please use "jdex2sex" which is also publically available on my account. 
Instruction on how to generate IR is in jdex2sex project directory.

The repo here will not be changed during review. 



