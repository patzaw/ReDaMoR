releaseVn=function(nodes) {
  Shiny.onInputChange('modelNet_release', Math.random());
};

$(document).keyup(function(event) {
    if ($("#newTableName").is(":focus") * (event.key == "Enter")) {
        $("#confirmAddTable").click();
    }
});
$(document).keyup(function(event) {
    if ($("#tableNewName").is(":focus") * (event.key == "Enter")) {
        $("#confirmRenameTable").click();
    }
});
// $(document).keyup(function(event) {
//     if ($("#tableComment").is(":focus") * (event.key == "Enter")) {
//     console.log("here");
//         $("#refreshComment").click();
//     }
// });
