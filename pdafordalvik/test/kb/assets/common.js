function radio_chk(name, num){
	var obj = document.getElementsByName(name);
	for(i=1; i < obj.length+1 ;i++){
		var chk = document.getElementById(name+"_"+i);
		var box = document.getElementById("block_"+name+"_"+i);
		if(i==num){
			if(box != null){
				box.style.display = "block";
			}
			$(chk).addClass("focus");
		}else{
			if(box != null){
				box.style.display = "none";
			}
			$(chk).removeClass("focus");
		}
	}
}

function radio_chk_default(name, num){
	var obj = document.getElementsByName(name);
	for(i=1; i < obj.length+1 ;i++){
		var chk = document.getElementById(name+"_"+i);
		if(i==num){
			$(chk).addClass("focus");
		}else{
			$(chk).removeClass("focus");
		}
	}
}

var agent = navigator.userAgent;
if (agent.match(/iPhone/) != null || agent.match(/iPod/) != null) {
	window.addEventListener('load', function(){
		setTimeout(scrollTo, 0, 0, 1);
	}, false);
	document.write('<meta name="viewport" content="width=device-width; user-scalable=no;" />');
}else if(agent.match(/Windows CE/) != null) {
	window.addEventListener('load', function(){
		setTimeout(scrollTo, 0, 0, 1);
	}, false);
	document.write('<meta name="viewport" content="width=device-width; initial-scale=0.5; maximum-scale=1.0; user-scalable=no;" />');
}else if (agent.match(/Android/) != null) {
	window.addEventListener('load', function(){
		setTimeout(scrollTo, 0, 0, 1);
	}, false);
	document.write('<meta name="viewport" content="width=device-width;" />');
}else{
	document.write('<meta name="viewport" content="width=device-width; initial-scale=0.5; maximum-scale=1.0; user-scalable=no;" />');
}
