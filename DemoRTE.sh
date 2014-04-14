#!/bin/sh
SCALA=scala
CORENLP_VERSION=stanford-corenlp-full-2014-01-04

JAVA_OPTS=-Xmx4g
export JAVA_OPTS

CLASSPATH_EN=lib/*:lib/en/*:lib/en/$CORENLP_VERSION/*:.

exec $SCALA -classpath $CLASSPATH_EN "$0" "$@"
!#

if (args.length != 1) {
	println("USAGE: DemoRTE.sh rte_xml")
	sys.exit()
}

import tifmo.dcstree.SemRole
import tifmo.dcstree.Statement
import tifmo.inference.IEngine
import tifmo.onthefly.AEngine
import tifmo.onthefly.OnTheFly
import tifmo.onthefly.PathAlignment
import tifmo.main.en.normalize
import tifmo.main.en.parse
import tifmo.main.en.EnResources
import tifmo.main.en.EnWord
import tifmo.main.en.TIME
import tifmo.main.en.EnSimilarityMikolov13
import tifmo.main.en.EnSimilarityTurian10

import scala.collection.mutable
import scala.util.Sorting

val rte_xml = args(0)

val f = xml.XML.loadFile(rte_xml)

for (p <- (f \ "pair")) {
	
	val id = (p \ "@id").text
	
	val task = (p \ "@task").text
	
	val gold_label = if (
		(p \ "@value").text == "TRUE" 
			|| (p \ "@entailment").text == "ENTAILMENT" 
			|| (p \ "@entailment").text == "YES"
	) {
		"Y"
	} else {
		"N"
	}
	
	val t = normalize((p \ "t").text.trim)
	val h = normalize((p \ "h").text.trim)
	val (tdoc, hdoc) = parse(t, h)
	
	val prem = tdoc.makeDeclaratives
	val hypo = hdoc.makeDeclaratives
	
	val ie = new IEngine
	
	prem.flatMap(_.toStatements).foreach(ie.claimStatement(_))
	hypo.flatMap(_.toStatements).foreach(ie.checkStatement(_))
	
	System.err.println("id: " + id + " gold: " + gold_label)
	System.err.println("TEXT:")
	System.err.println(" " + t)
	System.err.println("HYPO:")
	System.err.println(" " + h)
	
	// add linguistic knowledge
	val res = new EnResources
	val words = tdoc.allContentWords[EnWord] ++ hdoc.allContentWords[EnWord]
	for (s <- words.subsets(2)) {
		val a = s.head
		val b = (s - a).head
		if (res.synonym(a, b)) {
			System.err.println("synonym: " + a + " = " + b)
			ie.subsume(a, b)
			ie.subsume(b, a)
		} else if (Set(a.mypos, b.mypos).subsetOf(Set("J", "R")) && res.antonym(a, b)) {
			System.err.println("antonym: " + a + " <> " + b)
			ie.disjoint(a, b)
		} else {
			if (res.hyponym(a, b) && !b.isNamedEntity) {
				System.err.println("hyponym: " + a + " -> " + b)
				ie.subsume(a, b)
			}
			if (res.hyponym(b, a) && !a.isNamedEntity) {
				System.err.println("hyponym: " + b + " -> " + a)
				ie.subsume(b, a)
			}
		}
	}
	
	val proven = (x:IEngine) => hypo.flatMap(_.toStatements).forall(x.checkStatement(_))
	
	if (proven(ie)) {
		
		System.err.println("Proven.")
		println(id + "," + task + "," + gold_label + ",1.0")
		
	} else {
		
		val ae = new AEngine(prem)
		hypo.foreach(ae.addGoal(_))
		
		val otf = new OnTheFly(ie, ae)
		
		val sim = new EnSimilarityMikolov13(res, 0.7f)
		//val sim = new EnSimilarityTurian10(res, 0.7f)
		
		val score = (x:PathAlignment) => {
			// evaluate path alignment
			
			val PathAlignment(psub, psup) = x
			if (psup.rnrs.exists(x => x._1 == TIME || x._3 == TIME) 
						&& !psub.rnrs.exists(x => x._1 == TIME || x._3 == TIME)) {
				// time role unmatch, filtered.
				0.0
			} else {
				val wsub = psub.rnrs.map(x => x._2.token.getWord.asInstanceOf[EnWord]).filter(!_.isStopWord)
				val wsup = psup.rnrs.map(x => x._2.token.getWord.asInstanceOf[EnWord]).filter(!_.isStopWord)
				if (wsub.isEmpty || wsup.isEmpty) {
					// filtered.
					0.0
				} else if (wsup.exists(x => (x.isNamedEntity || x.mypos == "D") 
						&& !wsub.exists(y => res.synonym(y, x) || res.hyponym(y, x)))) {
					// time or named entity unmatch, filtered.
					0.0
				} else {
					// phrase similarity
					sim.similarity(wsub, wsup)
				}
			}
		}
		
		val (ret, rec) = otf.tryKnowledge(score, 0.1, proven)
		
		if (ret) {
			
			if (rec.isEmpty) {
				
				System.err.println("Proven.")
				println(id + "," + task + "," + gold_label + ",1.0")
				
			} else {
				
				System.err.println("Proven with on-the-fly knowledge.")
				println(id + "," + task + "," + gold_label + "," + rec.last._2)
				
				var necessary = Nil:List[(String, String, Double)]
				
				var tmpie = new IEngine
				tmpie.load(ie.dump())
				def loop() {
					val bak = tmpie.dump()
					rec.find(x => {
						x._1.foreach(_._2.toStatements.foreach(tmpie.claimStatement(_)))
						proven(tmpie)
					}) match {
						case Some((algs, scr)) => {
							val ss = for ((x, y) <- algs) yield {
								val sub = for ((r1, n, r2) <- x.subPath.rnrs) yield {
									val pre = r1.toString + "(" + n.token.getWord + ")"
									if (r2 == null) pre else (pre + r2)
								}
								val sup = for ((r1, n, r2) <- x.supPath.rnrs) yield {
									val pre = r1.toString + "(" + n.token.getWord + ")"
									if (r2 == null) pre else (pre + r2)
								}
								(sub.mkString("", "-", ""), sup.mkString("", "-", ""))
							}
							for ((sub, sup) <- ss) necessary = (sub, sup, scr) :: necessary
							tmpie = new IEngine
							tmpie.load(bak)
							algs.foreach(_._2.toStatements.foreach(tmpie.claimStatement(_)))
							if (!proven(tmpie)) loop()
						}
						case None => throw new Exception("this should not happen")
					}
				}
				loop()
				
				System.err.println("ON THE FLY:")
				for ((sub, sup, scr) <- necessary) {
					System.err.println(" score: " + scr)
					System.err.println("  subPath: " + sub)
					System.err.println("  supPath: " + sup)
				}
				
			}
			
		} else {
			
			System.err.println("Not proven.")
			println(id + "," + task + "," + gold_label + ",0.0")
			
		}
		
	}
}

