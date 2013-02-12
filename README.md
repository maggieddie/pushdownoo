pushdownoo
=========

The pushdown exception-flow analysis of object-oriented languages.


+ Program strcture:

  CRAPLE-LICENCE.txt

  benchmarks -- a set of Dalvik applications (or directly IRs for easy tests) to test the pushdown analyzer
                Andorid applications released by DARPA can not be distributed online here. 
                so only the apks ported from DaCapo and the apks downloaded from google code are available.
                

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

----------------------------------------------------------------------------------------------------------------------------

How to run:

java -jar artifacts/PushdownOO_Exflow.jar  org.ucombinator.dalvik.cfa.cesk.RunAnalysis  [options] irFolderName

e.g.
java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis --gc --lra benchmarks/dedexed_tests/sexps_antlr



