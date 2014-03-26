$(document).ready(function(){

	var chat = $.bullet('ws://localhost:8080/api/chat', {});
    console.log("chat created", chat);

	chat.onopen = function(){
        console.log("onopen");
		// $('#status_' + name).text('online');
	    chat.send("Hello :)");
	};

	chat.onclose = chat.onDisconnect = function(){
        console.log("onclose");
		// $('#status_' + name).text('offline');
	};

	chat.onmessage = function(e){
        console.log("onmessage", e);
		// $('#time_' + name).text(e.data);
	};

	chat.onheartbeat = function(){
		console.log("onheartbeat");
	};

	// chat.send("Hello :)");
	// chat.close();
});
