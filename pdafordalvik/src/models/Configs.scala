package models

case class Configs(
  k: Option[Int],
  gc: Option[String],
  doStateCutOff: Option[String],
  stateCutoff: Int,
  
  doTimeCutoff: Option[String],
  timeCutoff: Int,
 // verbose: Option[String],
  doRegex: Option[String],
  regex: String,
  dochecklist: Option[String], 
    
  propertyList: PropertyCheckList
 // test: Boolean
 // filesystem: String,
 // pic: String , 
 // location: String,
  // network: String,
  // mmsOrsmsOrContactetc:String
   
 /* picture : String,
  deviceid: String,
  network: String,
  display: String,
  reflection: String,
  browserbookmark: String,
  voice: String,
  browserhistory: String,
  thread: String,
  ipc: String,
  contact: String,
  sensor: String,
  account: String,
  media: String,
  serialid: String,
  sdcard: String */
 
 
  
  )
  
 