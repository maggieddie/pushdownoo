package models

case class PermissionPair(perms: List[String]) {
	var isAccessed = false
}