// Release visnetwork event
releaseVn=function(nodes) {
  Shiny.onInputChange('modelNet_release', Math.random());
};

// Focus on text inputs
$(document).keydown(function(event) {
  if($("#newTableName")[0]){
    $("#newTableName").focus();
  }
  if($("#tableNewName")[0]){
    $("#tableNewName").focus();
  }
});

// Validate changes with keyboard
$(document).keyup(function(event) {
  if($("#confirmAddTable")[0] && (event.key == "Enter")) {
    $("#confirmAddTable").click();
  }
  if($("#confirmRenameTable")[0] && (event.key == "Enter")) {
    $("#confirmRenameTable").click();
  }
  if($("#confirmAddField")[0] && (event.key == "Enter")) {
    $("#confirmAddField").click();
  }
  if($("#confirmAddFK")[0] && (event.key == "Enter")) {
    $("#confirmAddFK").click();
  }
  if($("#confirmUpdateField")[0] && (event.key == "Enter")) {
    $("#confirmUpdateField").click();
  }
});

// Rename table with F2 key
$(document).keyup(function(event) {
  if(
    (
      (document.activeElement.tagName == "BODY") ||
      (document.activeElement.tagName == "BUTTON")
    )&&
    (event.keyCode == 113)
  ) {
    if($("#renameTable")[0]){
      $("#renameTable").click();
    }else{
      if($("#editFK")[0]){
        $("#editFK").click();
      }
    }
  }
});

// Delete tables and keys with keyboard
$(document).keyup(function(event) {
  if(
    (
      (document.activeElement.tagName == "BODY") ||
      (document.activeElement.tagName == "BUTTON")
    )&&
    (event.key == "Delete")
  ) {
    if($("#removeTables")[0]){
      $("#removeTables").click();
    }else{
      $("#removeFK").click();
    }
  }
});

// Undo - Redo
$(document).keyup(function(event) {
  if(
    (
      (document.activeElement.tagName == "BODY") ||
      (document.activeElement.tagName == "BUTTON")
    ) &&
    (event.keyCode == 90 && event.ctrlKey && !event.shiftKey)
  ) {
     $("#undo").click();
  }
});
$(document).keyup(function(event) {
  if(
    (
      (document.activeElement.tagName == "BODY") ||
      (document.activeElement.tagName == "BUTTON")
    ) &&
    (event.keyCode == 90 && event.ctrlKey && event.shiftKey)
  ) {
     $("#redo").click();
  }
});
