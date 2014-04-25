package tifmo

import dcstree.WordBase

import mylib.res.en.EnStopWords

package main.en {
	
	case class EnWord(lemma: String, mypos: String, ner: String) extends WordBase {
		
		override def toString = lemma.replaceAll("[^a-zA-Z0-9]", "_") + "_" + mypos
		
		def isStopWord = (ner == "O" || ner == "NUMBER") && EnStopWords.isStopWord(lemma)
		
		def isNamedEntity = !Set("O", "NUMBER", "DATE", "TIME").contains(ner)
		
	}
	
}
