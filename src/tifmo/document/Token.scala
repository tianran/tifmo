package tifmo

import dcstree.WordBase
import dcstree.TokenBase

package document {
	
	class Token(
		/**
		 * The surface string.
		 */
		val surface: String
	) extends TokenBase with Serializable {
		
		/**
		 * The word of this token.
		 */
		var word = null:WordBase
		
		def getWord = word
		
		/**
		 * The coreference id (id of the mention cluster). `null` if not a mention.
		 */
		var corefID = null:String
		
		private[document] var xid = -1
		/**
		 * The id of the token (in a document). This id will be used to sort the children of a TokenNode.
		 */
		def id = xid
		
		private[document] var xdoc = null:Document
		/**
		 * The document that holds this token.
		 */
		def doc = xdoc
		
	}
}
