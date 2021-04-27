 document.addEventListener('DOMContentLoaded', main, false);
 
 function main(){
  const httpPort = location.port;
  const httpHostname = location.hostname;
  var wsUrl = "ws://" + httpHostname + ":" + httpPort;
  console.log("Connected to " + wsUrl);
  ws =  new WebSocket("ws://localhost:9160/");
  createWebsocket(ws);
  $("#senden").on("click", function(e){
    var rad = $("#radicand").val()
    ws.send(JSON.stringify(
      {requestAction :"exactRoot", 
        requestData : { 
          radicand : parseInt(rad),
          resExactRootMultiplier : "",
          resExactRootSqrt : ""
        }
      }
    ))
  })
}

function createWebsocket(ws) {
    ws.onopen = function() {
      $('#serverStatus').text('Websocket open');
      //var mes = JSON.stringify({action : "test", device : "test"});
      //ws.send(mes);
   }
   ws.onmessage = function (evt) {
      console.log("Server sendet : " + evt.data)
      var received_msg = JSON.parse(evt.data);
      console.log("Received Message: " + evt.data);
      $("#ergebnis").text("Ergebnis: " +  evt.data  )
   }

   ws.onclose = function() {
     $('#serverStatus').text('Websocket closed');
     console.log("Connection is closed...");
   }
}
