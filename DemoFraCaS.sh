#!/bin/sh
SCALA=scala
CORENLP_VERSION=stanford-corenlp-full-2014-01-04

JAVA_OPTS=-Xmx2g
export JAVA_OPTS

CLASSPATH_EN=lib/*:lib/en/*:lib/en/$CORENLP_VERSION/*

exec $SCALA -classpath $CLASSPATH_EN "$0" "$@"
!#

if (args.length != 1) {
	println("USAGE: DemoFraCaS.sh fracas_xml")
	sys.exit()
}

import tifmo.inference.IEngine
import tifmo.main.en.parse
import tifmo.main.en.EnWord

import mylib.res.en.EnWordNet

val fracas_xml = args(0)

val f = xml.XML.loadFile(fracas_xml)

for (p <- (f \ "problem")) {
	
	val id = (p \ "@id").text
	
	val fracas_answer = (p \ "@fracas_answer").text
	
	val sm = if ((p \ "p").length == 1) "single" else "multi"
	
	if (fracas_answer == "undef") {
		
		println(id + "," + sm + ",undef,ignore")
		
	} else {
		
		val t = (p \ "p").map(_.text.trim).mkString("", " ", "")
		val h = (p \ "h").text.trim
		
		val (tdoc, hdoc) = parse(t, h)
		
		val words = tdoc.allContentWords[EnWord] ++ hdoc.allContentWords[EnWord]
		def sameWordSynonym(x: IEngine) {
			for (s <- words.subsets(2)) {
				val a = s.head
				val b = (s - a).head
				if (EnWordNet.stem(a.lemma, a.mypos) == EnWordNet.stem(b.lemma, b.mypos)) {
					x.subsume(a, b)
					x.subsume(b, a)
				}
			}
		}
		
		val prem = tdoc.makeDeclaratives.flatMap(_.toStatements)
		val hypo = hdoc.makeDeclaratives.flatMap(_.toStatements)
		
		val ie = new IEngine
		
		prem.foreach(ie.claimStatement(_))
		hypo.foreach(ie.checkStatement(_))
		
		sameWordSynonym(ie)
		
		if (hypo.forall(ie.checkStatement(_))) {
			
			println(id + "," + sm + "," + fracas_answer + ",yes")
			
		} else {
			
			// negate hdoc
			for (n <- hdoc.allRootNodes) {
				n.rootNeg = !n.rootNeg
			}
			val nhypo = hdoc.makeDeclaratives.flatMap(_.toStatements)
			
			val nie = new IEngine
			
			prem.foreach(nie.claimStatement(_))
			nhypo.foreach(nie.checkStatement(_))
			
			sameWordSynonym(nie)
			
			if (nhypo.forall(nie.checkStatement(_))) {
				
				println(id + "," + sm + "," + fracas_answer + ",no")
				
			} else {
				
				println(id + "," + sm + "," + fracas_answer + ",unknown")
				
			}
			
		}
		
	}
	
}




