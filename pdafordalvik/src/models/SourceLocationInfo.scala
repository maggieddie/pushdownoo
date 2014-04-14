package models

case class SourceLocationInfo(
		riskScore: Integer,
		methodName: String,
		fileName: String,
		className: String,
		shortDesc: String,
		longDesc: String,
		startLineNo: Integer,
		endLineNo:Integer,
		startCol: Integer) {

}