var chat = null;
var history = [];
var users = [];

function connect() {
    var name = $("#user_name").val();
    if(name == "") return;

    console.log("connect");
	chat = $.bullet('ws://localhost:8080/api/chat', {});

	chat.onopen = function(){
        console.log("onopen");
        chat.send("join/" + name);
        $("#login_screen").css("display", "none");
        $("#chat_screen").css("display", "block");
        $("#chat_input").focus();
	};

	chat.onclose = chat.onDisconnect = function(){
        console.log("onclose");
        $("#login_screen").css("display", "block");
        $("#chat_screen").css("display", "none");
	};

	chat.onmessage = function(e){
        console.log("onmessage", e.data);
        var tokens = e.data.split("/");
        var action = tokens[0];

        switch(action) {
        case "joined":
            var node = tokens[1];
		    $("#connection_info").text("Connected to " + node);
            break;
        case "msg":
            var user = tokens[1];
            var msg = tokens[2];
            history.push("<p><b>" + user + ":</b> " + msg + "</p>");
            $("#chat_output").html(history.join("\n"));
            $("#chat_output").scrollTop(history.length * 30);
            break;
        case "user_join":
            var user = tokens[1];
            users.push(user);
            renderUsers();
            break;
        case "user_leave":
            var user = tokens[1];
            removeUser(user);
            renderUsers();
            break;
        default:
            console.log("unknown action", action);
        }
	};
}

function disconnect() {
	chat.close();
    history = [];
    users = [];
    $("#chat_output").html("");
    $("#user_list").html("");
    $("#user_name").focus();
}

function send() {
    var msg = $("#chat_input").val();
    if(msg == "") return;
    chat.send("msg/" + msg);
    $("#chat_input").val("");
    $("#chat_input").focus();
}

function removeUser(user) {
    for(var i = 0; i < users.length; i++) {
        if(users[i] == user) {
            users.splice(i, 1);
            break;
        }
    }
}

function renderUsers() {
    var output = "<p><b>" + users.join("</b></p><p><b>") + "</b></p>";
    $("#user_list").html(output);
}

$(document).ready(function(){
    $("#btn_enter").on("click", connect);
    $("#btn_leave").on("click", disconnect);
    $("#btn_send").on("click", send);
    $("#user_name").on("keypress", function(event) {
        if(event.keyCode == 13) connect();
    });
    $("#chat_input").on("keypress", function(event) {
        if(event.keyCode == 13) send();
    });
    $("#user_name").focus();
});
