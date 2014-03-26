var chat = null;

function connect() {

    console.log("connect");
	chat = $.bullet('ws://localhost:8080/api/chat', {});

	chat.onopen = function(){
        console.log("onopen");
        chat.send("join"); // TODO + name
        $("#login_screen")[0].style.display = "none";
        $("#chat_screen")[0].style.display = "block";
	};

	chat.onclose = chat.onDisconnect = function(){
        console.log("onclose");
        $("#login_screen")[0].style.display = "block";
        $("#chat_screen")[0].style.display = "none";
	};

	chat.onmessage = function(e){
        console.log("onmessage", e.data);
        var tokens = e.data.split("/");
        var action = tokens[0];
        var data = tokens[1];
        if(action == "joined") {
		    $("#connection_info").text("Connected to " + data);
        }
	};
}

function disconnect() {
	chat.close();
}

$(document).ready(function(){

    $("#enter_btn").on("click", connect);

});