SCALA=scala
SCALAC=scalac
SCALADOC=scaladoc

CORE=\
	tifmo/dcstree/SemRole.class \
	tifmo/dcstree/Quantifier.class \
	tifmo/dcstree/WordBase.class \
	tifmo/dcstree/TokenBase.class \
	tifmo/dcstree/Executor.class \
	tifmo/dcstree/Selection.class \
	tifmo/dcstree/Relation.class \
	tifmo/dcstree/Denotation.class \
	tifmo/dcstree/Statement.class \
	tifmo/dcstree/DCSTreeEdge.class \
	tifmo/dcstree/DCSTreeNode.class \
	tifmo/dcstree/Declarative.class \
	tifmo/dcstree/SelCorefBase.class \
	mylib/misc/oneFromEach.class \
	mylib/misc/listPartitions.class \
	mylib/misc/listCoverings.class \
	tifmo/inference/Dimension.class \
	tifmo/inference/IEngineCore.class \
	tifmo/inference/FuncComplement.class \
	tifmo/inference/FuncDIall.class \
	tifmo/inference/FuncDIno.class \
	tifmo/inference/FuncSingle.class \
	tifmo/inference/FuncNegation.class \
	tifmo/inference/IEDump.class \
	tifmo/inference/IEngine.class \
	tifmo/onthefly/AEngine.class \
	tifmo/onthefly/Path.class \
	tifmo/onthefly/PathAlignment.class \
	tifmo/onthefly/alignPaths.class \
	tifmo/onthefly/contextCandidates.class \
	mylib/misc/FibonacciHeap.class \
	tifmo/onthefly/OnTheFly.class \
	tifmo/document/SelCoref.class \
	tifmo/document/SelNum.class \
	tifmo/document/RelPartialOrder.class \
	tifmo/document/SelSup.class \
	tifmo/document/Document.class \
	tifmo/document/tentRootNeg.class \
	tifmo/document/tentRoles.class \
	tifmo/document/tentRoleOrder.class \
	

CORENLP_VERSION=stanford-corenlp-full-2014-01-04
CLASSPATH_EN=lib/*:lib/en/*:lib/en/$(CORENLP_VERSION)/*:.
TARGET_EN=\
	lib/en/$(CORENLP_VERSION).zip \
	lib/en/$(CORENLP_VERSION) \
	mylib/res/en/EnStopWords.class \
	tifmo/main/en/EnWord.class \
	resources/en/WordVectors \
	resources/en/wn3.1.dict.tar.gz \
	resources/en/dict \
	mylib/res/en/EnWordNet.class \
	tifmo/main/en/normalize.class \
	tifmo/main/en/ARG.class \
	tifmo/main/en/parse.class \
	mylib/misc/longestCommSeq.class \
	tifmo/main/en/EnResources.class \
	tifmo/main/en/EnSimilarity.class \
	resources/en/WordVectors/Turian10-embeddings-scaled.EMBEDDING_SIZE-50.txt.gz \
	resources/en/WordVectors/Turian10.cdb \
	mylib/res/en/EnTurian10.class \
	tifmo/main/en/EnSimilarityTurian10.class \
	resources/en/WordVectors/Mikolov13-GoogleNews-vectors-negative300.txt.bz2 \
	resources/en/WordVectors/Mikolov13.cdb \
	mylib/res/en/EnMikolov13.class \
	tifmo/main/en/EnSimilarityMikolov13.class \
	
#################################

all: $(CORE) scaladoc $(TARGET_EN)


tifmo/dcstree/DCSTreeNode.class: src/tifmo/dcstree/DCSTreeNode.scl src/tifmo/dcstree/Ref.scl src/tifmo/dcstree/Context.scl
	$(SCALAC) $^
	
tifmo/inference/IEngineCore.class: src/tifmo/inference/IEngineCore.scl src/tifmo/inference/Term.scl src/tifmo/inference/Finder.scl src/tifmo/inference/Debug_RuleTrace.scl src/tifmo/inference/IEPred.scl src/tifmo/inference/IEFunction.scl src/tifmo/inference/RuleArg.scl src/tifmo/inference/Trigger.scl src/tifmo/inference/RulesQuick.scl src/tifmo/inference/RulesLight.scl src/tifmo/inference/RulesHeavy.scl
	$(SCALAC) $^

tifmo/document/Document.class: src/tifmo/document/Document.scl src/tifmo/document/Token.scl src/tifmo/document/TokenNode.scl
	$(SCALAC) $^


scaladoc:
	mkdir scaladoc
	$(SCALADOC) -d scaladoc src/tifmo/dcstree/*.scl src/tifmo/inference/*.scl src/tifmo/onthefly/*.scl src/tifmo/document/*.scl


lib/en/$(CORENLP_VERSION).zip:
	wget http://nlp.stanford.edu/software/stanford-corenlp-full-2014-01-04.zip -O lib/en/$(CORENLP_VERSION).zip

lib/en/$(CORENLP_VERSION):
	unzip lib/en/$(CORENLP_VERSION).zip -d lib/en

resources/en/WordVectors:
	mkdir -p resources/en/WordVectors

resources/en/wn3.1.dict.tar.gz:
	wget http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz -O resources/en/wn3.1.dict.tar.gz

resources/en/dict:
	tar xvfz resources/en/wn3.1.dict.tar.gz -C resources/en

resources/en/WordVectors/Turian10-embeddings-scaled.EMBEDDING_SIZE-50.txt.gz:
	wget http://metaoptimize.s3.amazonaws.com/cw-embeddings-ACL2010/embeddings-scaled.EMBEDDING_SIZE=50.txt.gz -O resources/en/WordVectors/Turian10-embeddings-scaled.EMBEDDING_SIZE-50.txt.gz

resources/en/WordVectors/Turian10.cdb:
	gunzip -c resources/en/WordVectors/Turian10-embeddings-scaled.EMBEDDING_SIZE-50.txt.gz > resources/en/WordVectors/Turian10.txt
	$(SCALA) -classpath "lib/*" lib/en/WordVectors/mkCdb.scala resources/en/WordVectors/Turian10.txt resources/en/WordVectors/Turian10.cdb 50

resources/en/WordVectors/Mikolov13-GoogleNews-vectors-negative300.txt.bz2:
	wget --no-check-certificate https://googledrive.com/host/0B_-oZIbBJszXS00tcG04YnBYZkU -O resources/en/WordVectors/Mikolov13-GoogleNews-vectors-negative300.txt.bz2

resources/en/WordVectors/Mikolov13.cdb:
	bunzip2 -c resources/en/WordVectors/Mikolov13-GoogleNews-vectors-negative300.txt.bz2 > resources/en/WordVectors/Mikolov13.txt
	$(SCALA) -classpath "lib/*" lib/en/WordVectors/mkCdb.scala resources/en/WordVectors/Mikolov13.txt resources/en/WordVectors/Mikolov13.cdb 300

mylib/res/en/%.class: src/mylib/res/en/%.scl
	$(SCALAC) -classpath $(CLASSPATH_EN) $<

tifmo/main/en/%.class: src/tifmo/main/en/%.scl
	$(SCALAC) -classpath $(CLASSPATH_EN) $<

tifmo/main/en/ARG.class: src/tifmo/main/en/roles.scl
	$(SCALAC) $<


mylib/misc/%.class: src/mylib/misc/%.scl
	$(SCALAC) $<

tifmo/%.class: src/tifmo/%.scl
	$(SCALAC) $<


#################################

clean:
	rm -rf mylib tifmo scaladoc

#################################

.PHONY: clean
