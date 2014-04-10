# Introduction

TIFMO (Textual Inference Forward-chaining MOdule) is an unsupervised Recognizing Textual Entailment 
(RTE) system based on Dependency-based Compositional Semantics (DCS) and logical inference.

# Quick start

 * Download the code: git clone https://github.com/tianran/tifmo.git
 * Run 'make' to compile
 * Run FraCaS demo: ./DemoFraCaS.sh input/fracas.xml > fracas-out.txt
 * Check accuracy: ./AccuracyFraCaS.sh fracas-out.txt single 1 80
 * Run RTE demo: ./DemoRTE.sh input/RTE2_dev.xml > rte2-dev-out.txt
 * Check accuracy: ./AccuracyRTE.sh rte2-dev-out.txt 0.4

