compile:
	@ant compile
	@ant jar

antlr-pd:
	@java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  benchmarks/dedexed_tests/sexps_antlr2

antlr-pd-agc-lra:
	@java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis --gc --lra benchmarks/dedexed_tests/sexps_antlr2

lucene-pd:
	@java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis  --interrupt-after 100000  benchmarks/dedexed_tests/sexps_lucene

lucene-pd-agc-lra:
	@java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis --gc --lra --interrupt-after 16000  benchmarks/dedexed_tests/sexps_lucene

pmd-pd:
	@java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis --interrupt-after 60000  benchmarks/dedexed_tests/sexps_pmdseq2

pmd-pd-agc-lra:
	@java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis --gc --lra  --interrupt-after 4000  benchmarks/dedexed_tests/sexps_pmdseq2

swiFTP-pd:
	@java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis --k 0 --gc --lra --interrupt-after 150000  benchmarks/dedexed_tests/sexps_swiFTP

swiFTP-pd-agc-lra:
	@java -jar artifacts/PushdownOO_Exflow.jar org.ucombinator.dalvik.cfa.cesk.RunAnalysis --gc --lra --interrupt-after 5000  benchmarks/dedexed_tests/sexps_swiFTP

run-available:
	antlr-pd antlr-pd-agc-lra lucene-pd lucene-pd-agc-lra pmd-pd pmd-pd-agc-lra swiFTP-pd swiFTP-pd-agc-lra

clean:
	@ant clean
