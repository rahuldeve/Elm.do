

function sendEvent(name, target) {
    var ev = {name : name, target: target, value: ""};
    app.ports.jqevents.send(ev);
  }

  function sendEventwithVal(name, target, value) {
    var ev = {name : name, target: target, value: value};
    app.ports.jqevents.send(ev);
  }

function bindEditEntry() {
        
    $('.entry').on('click', '.display', function(e) {
        // console.log("asd");
        sendEvent("clickforedit", parseInt(e.currentTarget.id));
        e.stopPropagation();
      });
}



  // function bindEnterOnEntry() {
  //   $('.entry').on('keypress', '.edit', function(e) {
  //       // console.log(e.currentTarget.id + "asd");
  //       // console.log(e.keyCode);

  //       if(e.keyCode == 13) {
  //         sendEvent("enterpressed", parseInt(e.currentTarget.id));
  //         e.preventDefault(); 
  //         e.stopPropagation();
  //       }

  //     });
  // }

  function bindTabOnEntry() {
    $('.entry').on('keydown', '.edit', function(e) {
        // console.log(e.currentTarget.id + "asd");
        // console.log(e.keyCode);

        if(e.keyCode == 9) {
          sendEvent("tabpressed", parseInt(e.currentTarget.id));
          e.preventDefault(); 
          e.stopPropagation();
        }

      });
  }

  function bindBackspaceOnEmpty() {
    $('.entry').on('keydown', '.edit', function(e) {
      var entry = e.target.innerText.trim();
        // console.log(e.target.innerText.length);
        // console.log(entry.length);
        if(e.keyCode == 8 && entry.length == 0) {
          sendEvent("backspaceonempty", parseInt(e.currentTarget.id));
          e.stopPropagation();
          e.preventDefault();
        }
      });
  }

  function bindBlurOnEdit() {
    $('.entry').on('blur', '.edit', function(e) {
      var newstr = e.target.innerText.trim();
      sendEventwithVal("attemptupdate", parseInt(e.target.id), newstr)
      // app.ports.jqevents.send(ev);
      e.stopPropagation();
    });
  }


  function bindToggle() {
    $('.entry').on('click', '.checkbox-icon', function(e) {
      sendEvent("toggle", parseInt(e.currentTarget.parentElement.id));
      // console.log();
      e.stopPropagation();
      });
  }

  function bindZoomToCrumb() {
    $('.zoom-crumb').on('click', 'a', function(e) {
      sendEvent("zoomto", parseInt(e.target.attributes.href.value));
      // console.log();
      e.stopPropagation();
      });
  }
