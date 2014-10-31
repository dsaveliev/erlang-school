var chatUrl = "ws://" + document.URL.split("/")[2] + "/api/chat";
var chat = null;
var chat_history = [];

function connect() {
    var name = $("#user_name").val();
    if(name == "") return;

    console.log("connect", chatUrl);
	chat = $.bullet(chatUrl, {});

	chat.onopen = function(){
        console.log("onopen");
        chat.send("join/" + name); // send json instead
        $("#login_screen").css("display", "none");
        $("#chat_screen").css("display", "block");
        $("#chat_input").focus();
	};

	chat.onclose = chat.onDisconnect = function(){
        console.log("onclose");
        $("#login_screen").css("display", "block");
        $("#chat_screen").css("display", "none");
	};

	chat.onmessage = function(event){
        console.log("onmessage", event);
        var tokens = event.data.split("/"); // parse json instead
        var action = tokens[0];

        switch(action) {
        case "msg":
            var msg = tokens[1];
            chat_history.push("<p>" + msg + "</p>");
            renderHistory();
            break;
        default:
            console.log("unknown action", action);
        }
	};
}

function disconnect() {
	chat.close();
    chat_history = [];
    $("#chat_output").html("");
    $("#user_list").html("");
    $("#user_name").focus();
}

function send() {
    var msg = $("#chat_input").val();
    if(msg == "") return;
    chat.send("msg/" + msg); // send json instead
    $("#chat_input").val("");
    $("#chat_input").focus();
}

function renderHistory() {
    $("#chat_output").html(chat_history.join("\n"));
    $("#chat_output").scrollTop(chat_history.length * 30);
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
