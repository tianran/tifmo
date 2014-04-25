PRODUCTION_PATH=target/production
SCALAC=scalac -d $(PRODUCTION_PATH) -classpath $(PRODUCTION_PATH)
SCALA=scala -classpath "lib/*"

CORE=\
	$(PRODUCTION_PATH) \
	$(PRODUCTION_PATH)/tifmo/dcstree/SemRole.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/Quantifier.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/WordBase.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/TokenBase.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/Executor.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/Selection.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/Relation.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/Denotation.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/Statement.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/DCSTreeEdge.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/DCSTreeNode.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/Declarative.class \
	$(PRODUCTION_PATH)/tifmo/dcstree/SelCorefBase.class \
	$(PRODUCTION_PATH)/mylib/misc/oneFromEach.class \
	$(PRODUCTION_PATH)/mylib/misc/listPartitions.class \
	$(PRODUCTION_PATH)/mylib/misc/listCoverings.class \
	$(PRODUCTION_PATH)/tifmo/inference/Dimension.class \
	$(PRODUCTION_PATH)/tifmo/inference/IEngineCore.class \
	$(PRODUCTION_PATH)/tifmo/inference/FuncComplement.class \
	$(PRODUCTION_PATH)/tifmo/inference/FuncDIall.class \
	$(PRODUCTION_PATH)/tifmo/inference/FuncDIno.class \
	$(PRODUCTION_PATH)/tifmo/inference/FuncSingle.class \
	$(PRODUCTION_PATH)/tifmo/inference/FuncNegation.class \
	$(PRODUCTION_PATH)/tifmo/inference/IEDump.class \
	$(PRODUCTION_PATH)/tifmo/inference/IEngine.class \
	$(PRODUCTION_PATH)/tifmo/onthefly/AEngine.class \
	$(PRODUCTION_PATH)/tifmo/onthefly/Path.class \
	$(PRODUCTION_PATH)/tifmo/onthefly/PathAlignment.class \
	$(PRODUCTION_PATH)/tifmo/onthefly/alignPaths.class \
	$(PRODUCTION_PATH)/tifmo/onthefly/contextCandidates.class \
	$(PRODUCTION_PATH)/mylib/misc/FibonacciHeap.class \
	$(PRODUCTION_PATH)/tifmo/onthefly/OnTheFly.class \
	$(PRODUCTION_PATH)/tifmo/document/SelCoref.class \
	$(PRODUCTION_PATH)/tifmo/document/SelNum.class \
	$(PRODUCTION_PATH)/tifmo/document/RelPartialOrder.class \
	$(PRODUCTION_PATH)/tifmo/document/SelSup.class \
	$(PRODUCTION_PATH)/tifmo/document/Document.class \
	$(PRODUCTION_PATH)/tifmo/document/tentRootNeg.class \
	$(PRODUCTION_PATH)/tifmo/document/tentRoles.class \
	$(PRODUCTION_PATH)/tifmo/document/tentRoleOrder.class \
	

CORENLP_VERSION=stanford-corenlp-full-2014-01-04
LIBRES_EN=\
	lib/en/$(CORENLP_VERSION) \
	resources/en/dict \
	resources/en/WordVectors \
	resources/en/WordVectors/Turian10.cdb \
	resources/en/WordVectors/Mikolov13.cdb \


CLASSPATH_EN=lib/*:lib/en/*:lib/en/$(CORENLP_VERSION)/*
TARGET_EN=\
	$(PRODUCTION_PATH)/mylib/res/en/EnStopWords.class \
	$(PRODUCTION_PATH)/tifmo/main/en/EnWord.class \
	$(PRODUCTION_PATH)/mylib/res/en/EnWordNet.class \
	$(PRODUCTION_PATH)/tifmo/main/en/normalize.class \
	$(PRODUCTION_PATH)/tifmo/main/en/ARG.class \
	$(PRODUCTION_PATH)/tifmo/main/en/parse.class \
	$(PRODUCTION_PATH)/mylib/misc/longestCommSeq.class \
	$(PRODUCTION_PATH)/tifmo/main/en/EnResources.class \
	$(PRODUCTION_PATH)/tifmo/main/en/EnSimilarity.class \
	$(PRODUCTION_PATH)/mylib/res/en/EnTurian10.class \
	$(PRODUCTION_PATH)/tifmo/main/en/EnSimilarityTurian10.class \
	$(PRODUCTION_PATH)/mylib/res/en/EnMikolov13.class \
	$(PRODUCTION_PATH)/tifmo/main/en/EnSimilarityMikolov13.class \
	
#################################

all: $(CORE) enlibres $(TARGET_EN)


$(PRODUCTION_PATH):
	mkdir -p $(PRODUCTION_PATH)

$(PRODUCTION_PATH)/tifmo/dcstree/DCSTreeNode.class: src/tifmo/dcstree/DCSTreeNode.scala src/tifmo/dcstree/Ref.scala src/tifmo/dcstree/Context.scala
	$(SCALAC) $^
	
$(PRODUCTION_PATH)/tifmo/inference/IEngineCore.class: src/tifmo/inference/IEngineCore.scala src/tifmo/inference/Term.scala src/tifmo/inference/Finder.scala src/tifmo/inference/Debug_RuleTrace.scala src/tifmo/inference/IEPred.scala src/tifmo/inference/IEFunction.scala src/tifmo/inference/RuleArg.scala src/tifmo/inference/Trigger.scala src/tifmo/inference/RulesQuick.scala src/tifmo/inference/RulesLight.scala src/tifmo/inference/RulesHeavy.scala
	$(SCALAC) $^

$(PRODUCTION_PATH)/tifmo/document/Document.class: src/tifmo/document/Document.scala src/tifmo/document/Token.scala src/tifmo/document/TokenNode.scala
	$(SCALAC) $^


enlibres: $(LIBRES_EN)

lib/en/$(CORENLP_VERSION):
	if [ ! -e lib/en/$(CORENLP_VERSION).zip ]; then \
		wget http://nlp.stanford.edu/software/stanford-corenlp-full-2014-01-04.zip -O lib/en/$(CORENLP_VERSION).zip; fi
	unzip lib/en/$(CORENLP_VERSION).zip -d lib/en

resources/en/WordVectors:
	mkdir -p resources/en/WordVectors

resources/en/dict:
	if [ ! -e resources/en/wn3.1.dict.tar.gz ]; then \
		wget http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz -O resources/en/wn3.1.dict.tar.gz; fi
	tar xvfz resources/en/wn3.1.dict.tar.gz -C resources/en

resources/en/WordVectors/Turian10.cdb:
	if [ ! -e resources/en/WordVectors/Turian10-embeddings-scaled.EMBEDDING_SIZE-50.txt.gz ]; then \
		wget http://metaoptimize.s3.amazonaws.com/cw-embeddings-ACL2010/embeddings-scaled.EMBEDDING_SIZE=50.txt.gz -O resources/en/WordVectors/Turian10-embeddings-scaled.EMBEDDING_SIZE-50.txt.gz; fi
	gunzip -c resources/en/WordVectors/Turian10-embeddings-scaled.EMBEDDING_SIZE-50.txt.gz > resources/en/WordVectors/Turian10.txt
	$(SCALA) lib/en/WordVectors/mkCdb.scala resources/en/WordVectors/Turian10.txt resources/en/WordVectors/Turian10.cdb 50

resources/en/WordVectors/Mikolov13.cdb:
	if [ ! -e resources/en/WordVectors/Mikolov13-GoogleNews-vectors-negative300.txt.bz2 ]; then \
		wget --no-check-certificate https://googledrive.com/host/0B_-oZIbBJszXS00tcG04YnBYZkU -O resources/en/WordVectors/Mikolov13-GoogleNews-vectors-negative300.txt.bz2; fi
	bunzip2 -c resources/en/WordVectors/Mikolov13-GoogleNews-vectors-negative300.txt.bz2 > resources/en/WordVectors/Mikolov13.txt
	$(SCALA) lib/en/WordVectors/mkCdb.scala resources/en/WordVectors/Mikolov13.txt resources/en/WordVectors/Mikolov13.cdb 300


$(PRODUCTION_PATH)/mylib/res/en/%.class: src/mylib/res/en/%.scala
	$(SCALAC):$(CLASSPATH_EN) $<

$(PRODUCTION_PATH)/tifmo/main/en/%.class: src/tifmo/main/en/%.scala
	$(SCALAC):$(CLASSPATH_EN) $<

$(PRODUCTION_PATH)/tifmo/main/en/ARG.class: src/tifmo/main/en/roles.scala
	$(SCALAC) $<


$(PRODUCTION_PATH)/mylib/misc/%.class: src/mylib/misc/%.scala
	$(SCALAC) $<

$(PRODUCTION_PATH)/tifmo/%.class: src/tifmo/%.scala
	$(SCALAC) $<


#################################

scaladoc:
	mkdir scaladoc
	scaladoc -d scaladoc src/mylib/misc/*.scala src/tifmo/dcstree/*.scala src/tifmo/inference/*.scala src/tifmo/onthefly/*.scala src/tifmo/document/*.scala

clean:
	rm -rf target scaladoc

#################################

.PHONY: scaladoc clean
