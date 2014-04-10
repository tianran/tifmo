package tifmo

import dcstree.QuantifierALL
import dcstree.QuantifierNO
import document.SelNum
import document.SelSup
import document.Token
import document.Document
import document.tentRootNeg
import document.tentRoles
import document.tentRoleOrder

import mylib.res.en.EnWordNet

import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedDependenciesAnnotation
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefClusterIdAnnotation

import java.util.Properties

import scala.collection.mutable
import scala.collection.JavaConversions._

package main.en {
	
	object parse extends ((String, String) => (Document, Document)) {
		
		private[this] val props = new Properties
		props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
		private[this] val pipeline = new StanfordCoreNLP(props)
		
		def apply(text: String, hypo: String) = {
			
			val annotext = new Annotation(text)
			pipeline.annotate(annotext)
			
			val doctext = makeDocument(annotext, "text")
			
			addCoreferences(annotext, doctext)
			
			makeDCSTrees(annotext, doctext)
			
			tentRootNeg(doctext)
			
			///////////
			
			val annohypo = new Annotation(hypo)
			pipeline.annotate(annohypo)
			
			val dochypo = makeDocument(annohypo, "hypo")
			
			makeDCSTrees(annohypo, dochypo)
			
			tentRootNeg(dochypo)
			
			//////////////
			
			val rolesDic = tentRoles(Set(doctext, dochypo))
			
			tentRoleOrder(rolesDic, doctext)
			
			tentRoleOrder(rolesDic, dochypo)
			
			/////////////////
			
			(doctext, dochypo)
		}
		
		private[this] def makeDocument(anno: Annotation, id: String) = {
			
			val tokens = for {
				sentence <- anno.get(classOf[SentencesAnnotation])
				atoken <- sentence.get(classOf[TokensAnnotation])
			} yield {
				
				val ret = new Token(atoken.get(classOf[TextAnnotation]))
				
				val pos = atoken.get(classOf[PartOfSpeechAnnotation])
				val ner = atoken.get(classOf[NamedEntityTagAnnotation])
				val lemma = if (ner == "DATE" || ner == "TIME") {
					atoken.get(classOf[NormalizedNamedEntityTagAnnotation])
				} else {
					atoken.get(classOf[LemmaAnnotation])
				}
				val mypos = if (ner == "DATE" || ner == "TIME") {
					"D"
				} else if (pos.matches("JJ.*")) {
					"J"
				} else if (pos.matches("NN.*")) {
					"N"
				} else if (pos.matches("RB.*")) {
					"R"
				} else if (pos.matches("VB.*")) {
					"V"
				} else {
					"O"
				}
				// work around for "each"
				val nner = if (lemma == "each") "O" else ner
				ret.word = EnWord(lemma, mypos, nner)
				
				ret
			}
			
			new Document(id, tokens.toIndexedSeq)
		}
		
		private[this] def makeDCSTrees(anno: Annotation, doc: Document) {
			
			var counter = 0
			for (sentence <- anno.get(classOf[SentencesAnnotation])) {
				
				def tokeninfo(x: IndexedWord) = {
					val xid = counter + x.get(classOf[IndexAnnotation]) - 1
					val token = doc.tokens(xid)
					val word = token.word.asInstanceOf[EnWord]
					val pos = x.get(classOf[PartOfSpeechAnnotation])
					(token, word, pos)
				}
				
				var edges = (for (e <- sentence.get(classOf[CollapsedDependenciesAnnotation]).edgeIterable) yield {
					val ptk = tokeninfo(e.getGovernor)
					val rel = e.getRelation.getShortName
					val spc = e.getRelation.getSpecific
					val ctk = tokeninfo(e.getDependent)
					(ptk, rel, spc, ctk)
				}).toSet
				
				// copula
				edges = {
					var ret = edges
					for (ptk @ (t, wd, pos) <- edges.map(_._1); if wd.lemma == "be" && pos.matches("VB.*")) {
						val filcomp = edges.filter(x => x._1 == ptk && x._2.matches("[cx]?comp"))
						if (filcomp.size == 1) {
							val (pcomp, relcomp, spccomp, ccomp) = filcomp.head
							ret = ret - filcomp.head
							for ((pp, rrel, sspc, cc) <- edges; if pp == ptk && !rrel.matches("[cx]?comp")) {
								ret = ret - ((pp, rrel, sspc, cc)) + ((ccomp, (if (rrel.matches("[nc]?subj")) "copula" else rrel), sspc, cc))
							}
							for ((pp, rrel, sspc, cc) <- edges; if cc == ptk) {
								ret = ret - ((pp, rrel, sspc, cc)) + ((pp, rrel, sspc, ccomp))
							}
						} else {
							val filsubj = edges.filter(x => x._1 == ptk && x._2.matches("[nc]?subj"))
							if (filsubj.size == 1) {
								val (psubj, relsubj, spcsubj, csubj) = filsubj.head
								ret = ret - filsubj.head
								for ((pp, rrel, sspc, cc) <- edges; if pp == ptk && !rrel.matches("[nc]?subj")) {
									ret = ret - ((pp, rrel, sspc, cc)) + ((csubj, rrel, sspc, cc))
								}
								for ((pp, rrel, sspc, cc) <- edges; if cc == ptk) {
									ret = ret - ((pp, rrel, sspc, cc)) + ((pp, rrel, sspc, csubj))
								}
							}
						}
					}
					ret
				}
				
				// rcmod
				edges = {
					var ret = edges
					val sensitive = Set("nsubj", "dobj", "iobj", "nsubjpass")
					for ((ptk, rel, spc, ctk) <- edges; if rel == "rcmod") {
						for ((pp, rrel, sspc, cc) <- edges; if pp == ctk && ((cc._3.matches("W.+") && sensitive.contains(rrel)) || cc._2.lemma == "when")) {
							ret = ret - ((pp, rrel, sspc, cc))
							val tmp = if (sensitive.contains(rrel)) rrel else "when"
							ret = ret - ((ptk, rel, spc, ctk)) + ((ptk, rel, tmp, ctk))
						}
					}
					ret
				}
				
				// some of
				edges = {
					var ret = edges
					for ((ptk, rel, spc, ctk) <- edges; if rel == "prep" && spc == "of") {
						if (ptk._2.lemma == "some" || ptk._2.lemma == "most") {
							ret = ret - ((ptk, rel, spc, ctk))
							for ((pp, rrel, sspc, cc) <- edges; if cc == ptk) {
								ret = ret - ((pp, rrel, sspc, cc)) + ((pp, rrel, sspc, ctk))
							}
						}
						if (ptk._2.lemma == "all" || ptk._2.lemma == "each") {
							ret = ret - ((ptk, rel, spc, ctk)) + ((ctk, "det", null, ptk))
							for ((pp, rrel, sspc, cc) <- edges; if cc == ptk) {
								ret = ret - ((pp, rrel, sspc, cc)) + ((pp, rrel, sspc, ctk))
							}
						}
						if (ptk._2.lemma == "none") {
							ret = ret - ((ptk, rel, spc, ctk)) + ((ctk, "det", null, ptk))
							for ((pp, rrel, sspc, cc) <- edges; if cc == ptk) {
								ret = ret - ((pp, rrel, sspc, cc)) + ((pp, rrel, sspc, ctk))
							}
						}
						if (ptk._2.lemma.matches("-?[0-9\\.]+")) {
							ret = ret - ((ptk, rel, spc, ctk)) + ((ctk, "num", null, ptk))
							for ((pp, rrel, sspc, cc) <- edges; if cc == ptk) {
								ret = ret - ((pp, rrel, sspc, cc)) + ((pp, rrel, sspc, ctk))
							}
						}
					}
					ret
				}
				
				// most JJ
				edges = {
					var ret = edges
					for ((ptk, rel, spc, ctk) <- edges; if ctk._2.lemma == "most" && ptk._3 == "JJ") {
						ret = ret - ((ptk, rel, spc, ctk)) + (((ptk._1, ptk._2, "JJS"), rel, spc, ctk))
					}
					ret
				}
				
				// cluster named entity
				edges = {
					var ret = edges
					val finish = counter + sentence.get(classOf[TokensAnnotation]).size
					def scan(i: Int) {
						if (i < finish) {
							val theword = doc.tokens(i).word.asInstanceOf[EnWord]
							def sameNE(tk: Token) = {
								val tkword = tk.word.asInstanceOf[EnWord]
								//(doc.tokens(i).corefID == null || tk.corefID == null || doc.tokens(i).corefID == tk.corefID) && 
									tkword.ner == theword.ner && (theword.mypos != "D" || tkword.lemma == theword.lemma)
							}
							if (theword.ner != "O") {
								val tmpmax = {
									var tmp = Set.empty[Int]
									var cachemax = i
									def loop() {
										for ((ptk, rel, spc, ctk) <- ret; if rel != "conj" && rel != "prep") {
											if (ptk._1.id >= i && ptk._1.id <= cachemax && sameNE(ctk._1)) {
												tmp += ctk._1.id
											}
											if (ctk._1.id >= i && ctk._1.id <= cachemax && sameNE(ptk._1)) {
												tmp += ptk._1.id
											}
										}
										if (!tmp.isEmpty && tmp.max != cachemax) {
											cachemax = tmp.max
											loop()
										}
									}
									loop()
									cachemax
								}
								assert(tmpmax >= i)
								if (tmpmax == i) {
									scan(i + 1)
								} else {
									val nword = if (theword.mypos != "D") {
										val nlemma = (i to tmpmax).map(j => doc.tokens(j).surface).mkString("", " ", "")
										EnWord(nlemma, "N", theword.ner)
									} else {
										theword
									}
									doc.tokens(i).word = nword
									
									(i to tmpmax).find(j => doc.tokens(j).corefID != null) match {
										case Some(j) => doc.tokens(i).corefID = doc.tokens(j).corefID
										case None => {}
									}
									
									for ((pp, rrel, sspc, cc) <- ret.toList) {
										if (pp._1.id >= i && pp._1.id <= tmpmax) {
											ret = ret - ((pp, rrel, sspc, cc))
											if (!(cc._1.id >= i && cc._1.id <= tmpmax)) {
												ret = ret + (((doc.tokens(i), nword, pp._3), rrel, sspc, cc))
											}
										}
										if (cc._1.id >= i && cc._1.id <= tmpmax) {
											ret = ret - ((pp, rrel, sspc, cc))
											if (!(pp._1.id >= i && pp._1.id <= tmpmax)) {
												ret = ret + ((pp, rrel, sspc, (doc.tokens(i), nword, cc._3)))
											}
										}
									}
									scan(tmpmax + 1)
								}
							} else {
								scan(i + 1)
							}
						}
					}
					scan(counter)
					ret
				}
				
				// annotate
				for ((ptk, rel, spc, ctk) <- edges) {
					rel match {
						case "copula" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
						}
						case "dep" => {
							if (ptk._2.mypos != "O" && ctk._2.mypos != "O") {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							}
						}
						case "agent" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(SUBJ, doc.getTokenNode(ctk._1))
						}
						case "acomp" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(MOD, doc.getTokenNode(ctk._1))
						}
						case "ccomp" => {
							if (ptk._2.mypos == "V") {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(OBJ, doc.getTokenNode(ctk._1))
							} else if (ptk._2.mypos == "J") {
								doc.getTokenNode(ctk._1).outRole = MOD
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							} else {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							}
						}
						case "xcomp" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
						}
						case "dobj" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(OBJ, doc.getTokenNode(ctk._1))
						}
						case "iobj" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(IOBJ, doc.getTokenNode(ctk._1))
						}
						case "pobj" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(IOBJ, doc.getTokenNode(ctk._1))
						}
						case "nsubj" => {
							if (ptk._2.mypos == "V") {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(SUBJ, doc.getTokenNode(ctk._1))
							} else if (ptk._2.mypos == "J") {
								doc.getTokenNode(ctk._1).outRole = MOD
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							} else {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							}
						}
						case "nsubjpass" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(OBJ, doc.getTokenNode(ctk._1))
						}
						case "csubj" => {
							if (ptk._2.mypos == "V") {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(SUBJ, doc.getTokenNode(ctk._1))
							} else if (ptk._2.mypos == "J") {
								doc.getTokenNode(ctk._1).outRole = MOD
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							} else {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							}
						}
						case "csubjpass" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(OBJ, doc.getTokenNode(ctk._1))
						}
						case "conj" => {
							doc.getTokenNode(ptk._1).addConjunction(doc.getTokenNode(ctk._1))
						}
						case "amod" => {
							if (ptk._2.mypos == "N" && ctk._3 == "JJS") {
								doc.getTokenNode(ptk._1).selection = SelSup(EnWordNet.stem(ctk._2.lemma, ctk._2.mypos), ARG)
							} else {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(MOD, doc.getTokenNode(ctk._1))
							}
						}
						case "appos" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
						}
						case "advcl" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
						}
						case "det" => {
							if (ctk._2.lemma == "all" || ctk._2.lemma == "every" || ctk._2.lemma == "each") {
								doc.getTokenNode(ptk._1).quantifier = QuantifierALL
							} else if (ctk._2.lemma == "no" || ctk._2.lemma == "none") {
								doc.getTokenNode(ptk._1).quantifier = QuantifierNO
							}
						}
						case "predet" => {
							if (ctk._2.lemma == "all") {
								doc.getTokenNode(ptk._1).quantifier = QuantifierALL
							}
						}
						case "infmod" => {
							doc.getTokenNode(ctk._1).outRole = OBJ
							doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
						}
						case "partmod" => {
							doc.getTokenNode(ctk._1).outRole = if (ctk._3 == "VBG") SUBJ else OBJ
							val tmp = if (ptk._2.mypos == "V") {
								if (edges.exists(x => x._1 == ptk && (x._2 == "nsubjpass" || x._2 == "csubjpass"))) OBJ else SUBJ
							} else {
								ARG
							}
							doc.getTokenNode(ptk._1).addChild(tmp, doc.getTokenNode(ctk._1))
						}
						case "advmod" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(MOD, doc.getTokenNode(ctk._1))
						}
						case "neg" => {
							doc.getTokenNode(ptk._1).sign = false
						}
						case "rcmod" => {
							if (ctk._2.mypos == "V") {
								val tmpmap = Map("when" -> TIME, "nsubj" -> SUBJ, "dobj" -> OBJ, "iobj" -> IOBJ, "nsubjpass" -> OBJ)
								doc.getTokenNode(ctk._1).outRole = if (spc == null) ARG else tmpmap(spc)
								val tmp = if (spc == "when") TIME else ARG
								doc.getTokenNode(ptk._1).addChild(tmp, doc.getTokenNode(ctk._1))
							} else if (ctk._2.mypos == "J") {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(MOD, doc.getTokenNode(ctk._1))
							} else {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							}
						}
						case "nn" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							val tmp = if (ctk._2.mypos == "D" && ptk._2.mypos != "D") {
								TIME
							} else if (ptk._2.isNamedEntity && ctk._2.isNamedEntity) {
								MOD
							} else {
								ARG
							}
							doc.getTokenNode(ptk._1).addChild(tmp, doc.getTokenNode(ctk._1))
						}
						case "npadvmod" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
						}
						case "tmod" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(TIME, doc.getTokenNode(ctk._1))
						}
						case "num" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							if (ctk._2.mypos == "D") {
								doc.getTokenNode(ptk._1).addChild(TIME, doc.getTokenNode(ctk._1))
							} else if (ptk._2.mypos == "N") {
								doc.getTokenNode(ptk._1).selection = SelNum(ctk._2.lemma, ARG)
							} else if (ctk._2.lemma.matches("-?[0-9\\.%]+")) {
								doc.getTokenNode(ctk._1).selection = SelNum(ctk._2.lemma, ARG)
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							}
						}
						case "number" => {
							if (ctk._2.lemma.matches("-?[0-9\\.%]+") && ptk._2.mypos == "N") {
								doc.getTokenNode(ptk._1).selection = SelNum(ctk._2.lemma, ARG)
							} else {
								doc.getTokenNode(ctk._1).outRole = ARG
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							}
						}
						case "prep" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							if (ctk._2.mypos == "D") {
								// we should add more rules to recognize
								 // relations like before, after, since, from, to, ...
								doc.getTokenNode(ptk._1).addChild(TIME, doc.getTokenNode(ctk._1))
							} else if (ctk._2.mypos == "N" && ptk._2.mypos == "N") {
								if (spc == "as") {
									doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
								} else if (spc == "of" || spc == "by") {
									doc.getTokenNode(ptk._1).addChild(MOD, doc.getTokenNode(ctk._1))
								} else if (spc == null) {
									doc.getTokenNode(ctk._1).outRole = MOD
									doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
								} else {
									doc.getTokenNode(ptk._1).addChild(MOD, doc.getTokenNode(ctk._1))
								}
							} else {
								doc.getTokenNode(ptk._1).addChild(IOBJ, doc.getTokenNode(ctk._1))
							}
						}
						case "prepc" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							if (spc == null) {
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							} else {
								doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
							}
						}
						case "poss" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(MOD, doc.getTokenNode(ctk._1))
						}
						case "prt" => {
							val nword = EnWord(ptk._2.lemma + " " + ctk._2.lemma, ptk._2.mypos, ptk._2.ner)
							ptk._1.word = nword
						}
						case "parataxis" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(ARG, doc.getTokenNode(ctk._1))
						}
						case "vmod" => {
							doc.getTokenNode(ctk._1).outRole = ARG
							doc.getTokenNode(ptk._1).addChild(MOD, doc.getTokenNode(ctk._1))
						}
						case _ => {}
					}
				}
				
				counter += sentence.get(classOf[TokensAnnotation]).size
			}
			
		}
		
		private[this] def addCoreferences(anno: Annotation, doc: Document) {
			
			var counter = 0
			for {
				sentence <- anno.get(classOf[SentencesAnnotation])
				atoken <- sentence.get(classOf[TokensAnnotation])
			} {
				val tmp = atoken.get(classOf[CorefClusterIdAnnotation])
				if (tmp != null) doc.tokens(counter).corefID = tmp.toString
				counter += 1
			}
		}
		
	}
	
}
