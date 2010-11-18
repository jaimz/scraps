if (!window.console) {
    window.console = 
        { log: function() {},
          warn: function() {},
          error: function() {} 
        };
}

var NB = {}


$(function() {
    NB.App = {};
    
    NB.App.NotificationCenter = NB.CreateNotificationCenter();
    NB.App.Status = NB.CreateStatusManager();
    NB.App.AccountManager = NB.CreateAccountManager();
    NB.App.StorageManager = NB.CreateStorageManager();
    NB.App.Messages = NB.CreateAppMessages();
    
    
    var user_panel_el = document.getElementById('user_panel');
    if (!user_panel_el)
        console.error("Could not find user panel element");
    else
        NB.App.UserPanel = NB.CreateUserPanel(user_panel_el);
        
    
    var note_list_el = document.getElementById('note_book');
    if (!note_list_el)
        console.error("Could not find note list element");
    else
        NB.App.NoteList = NB.CreateNoteList(note_list_el);
        
    var note_editor = document.getElementById('note_editor');
    if (!note_editor)
        console.error("Could not find note editor");
    else
        NB.App.NoteEditor = NB.CreateNoteEditor(note_editor);
    



    // Get the current account and start the magic...
    NB.App.UserPanel.update();
});