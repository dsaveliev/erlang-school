var chatUrl = "ws://" + document.URL.split("/")[2] + "/api/chat";
var chat = null;
var history = [];
var users = [];

function connect() {
    var name = $("#user_name").val();
    if(name == "") return;

    console.log("connect", chatUrl);
	chat = $.bullet(chatUrl, {});

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

            var online = tokens[2];
            users = online.split("|");
            renderUsers();

            history = [];
            var messages = tokens[3].split("|");
            for(var i in messages) {
                var parts = messages[i].split(":");
                if(parts.length == 2)
                    history.push("<p><b>" + parts[0] + ":</b> " + parts[1] + "</p>");
            }
            renderHistory();
            break;
        case "msg":
            var user = tokens[1];
            var msg = tokens[2];
            history.push("<p><b>" + user + ":</b> " + msg + "</p>");
            renderHistory();
            break;
        case "user_join":
            var user = tokens[1];
            if(!userExists(user)) {
                users.push(user);
                renderUsers();
            }
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
    for(var i in users) {
        if(users[i] == user) {
            users.splice(i, 1);
            break;
        }
    }
}

function userExists(user) {
    for(var i in users) {
        if(users[i] == user) return true;
    }
    return false;
}

function renderHistory() {
    $("#chat_output").html(history.join("\n"));
    $("#chat_output").scrollTop(history.length * 30);
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
