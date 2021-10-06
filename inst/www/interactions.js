// Release visnetwork event
releaseVn=function(nodes) {
  Shiny.onInputChange('modelNet_release', Math.random());
};

$(document).keydown(function(event) {

  // Focus on text inputs
  if($("#newTableName")[0]){
    $("#newTableName").focus();
  }
  if($("#tableNewName")[0]){
    $("#tableNewName").focus();
  }

  // Validate changes with keyboard
  if($("#confirmAddTable")[0] && (event.key == "Enter")) {
    $("#confirmAddTable").click();
  }
  if($("#confirmRenameTable")[0] && (event.key == "Enter")) {
    $("#confirmRenameTable").click();
  }
  // if($("#confirmAddField")[0] && (event.key == "Enter")) {
  //   $("#confirmAddField").click();
  // }
  // if($("#confirmAddFK")[0] && (event.key == "Enter")) {
  //   $("#confirmAddFK").click();
  // }
  // if($("#confirmUpdateField")[0] && (event.key == "Enter")) {
  //   $("#confirmUpdateField").click();
  // }

  if(
      (document.activeElement.tagName == "BODY") ||
      (document.activeElement.tagName == "BUTTON") ||
      (document.activeElement.getAttribute("class") == "vis-network")
  ) {

    // Rename table with F2 key
    if(event.keyCode == 113) {
      if($("#renameTable")[0]){
        $("#renameTable").click();
      }else{
        if($("#editFK")[0]){
          $("#editFK").click();
        }
      }
    }

    // Delete tables and keys with keyboard
    if(event.key == "Delete") {
      if($("#removeTables")[0]){
        $("#removeTables").click();
      }else{
        $("#removeFK").click();
      }
    }

    // Undo - Redo
    if(event.keyCode == 90 && event.ctrlKey && !event.shiftKey) {
       $("#undo").click();
    }
    if(event.keyCode == 90 && event.ctrlKey && event.shiftKey) {
       $("#redo").click();
    }

  }

});
