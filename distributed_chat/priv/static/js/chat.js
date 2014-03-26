var chat = null;

function connect() {
    var name = $("#user_name")[0].value;
    if(name == "") return;

    console.log("connect");
	chat = $.bullet('ws://localhost:8080/api/chat', {});

	chat.onopen = function(){
        console.log("onopen");
        chat.send("join/" + name);
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
        if(action == "msg") {
            var user = tokens[1];
            var msg = tokens[2];
            var output = $("#chat_output").html();
            output += "<p><b>" + user + ":</b> " + msg + "</p>";
            $("#chat_output").html(output);
        }
        if(action == "user_join") {
            var user = tokens[1];
            // TODO update and render user list
            var output = "<p><b>" + user + "</b></p>";
            $("#user_list").html(output);
        }
        if(action == "user_leave") {
            var user = tokens[1];
            // TODO update and render user list
            var output = "";
            $("#user_list").html(output);
        }
	};
}

function disconnect() {
	chat.close();
    $("#chat_output").html("");
    $("#user_list").html("");
}

function send() {
    var msg = $("#chat_input")[0].value;
    if(msg == "") return;
    chat.send("msg/" + msg);
    $("#chat_input")[0].value = "";
}

$(document).ready(function(){
    $("#btn_enter").on("click", connect);
    $("#btn_leave").on("click", disconnect);
    $("#btn_send").on("click", send);
    //$("#user_name").on("submit", connect);
    //$("#chat_input").on("submit", send);
});
